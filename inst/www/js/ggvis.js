/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global vg, ggvis:true*/

ggvis = (function() {
  var ggvis = {
    // Keep track of information about all plots: contains ggvis.Plot objects
    plots: {}
  };

  // Get a ggvis.Plot object with a particular name, creating it if needed
  ggvis.getPlot = function(plotId) {
    if (!this.plots[plotId]) {
      this.plots[plotId] = new ggvis.Plot(plotId);
    }
    return this.plots[plotId];
  };

  // Are we in a small viewer panel?
  ggvis.inViewerPanel = function() {
    // This is a pretty dumb criterion, but it's good enough
    return $(window).width() < 600 || $(window).height() < 600;
  };


  // ggvis.Plot class ----------------------------------------------------------
  ggvis.Plot = (function() {

    var Plot = function(plotId) {
      this.plotId = plotId;
      this.pendingData = {}; // Data objects that have been received but not yet used
      this.chart = null;     // Vega chart object on the page
      this.spec = null;      // Vega spec for this plot
      this.initialized = false; // Has update() or enter() been run?
      this.opts = {};
    };

    var prototype = Plot.prototype;

    // opts is an optional object which can have any entries that are in spec.opts
    // (they get merged on top of spec.opts), and additionally:
    // * mouseover: A callback for the "mouseover" event
    // * mouseout: A callback for the "mouseout" event
    // * hovertime: Number of milliseconds for a hover transition
    prototype.parseSpec = function(spec, opts) {
      var self = this;
      self.spec = spec;
      self.initialized = false;
      // Merge options passed to this function into options from the spec
      self.opts = $.extend(true, self.spec.ggvis_opts, opts);

      vg.parse.spec(spec, function(chart) {
        var opts = self.opts;
        var $el = self.getDiv();

        // If hovertime is supplied, use that later in a custom callback,
        // instead of the default hover behavior.
        var hover = true;
        if (opts.hovertime && opts.hovertime !== 0) hover = false;

        chart = chart({
          el: "#" + self.plotId,
          renderer: opts.renderer || "canvas",
          hover: hover
        });
        // Save the chart object
        self.chart = chart;
        $el.data("ggvis-chart", chart);

        // Set the renderer (update buttons and download link)
        self.setRenderer(opts.renderer, false);

        // If hovertime is specified, set callbacks for hover behavior
        if (opts.hovertime && opts.hovertime !== 0) {
          chart.on("mouseover", function(event, item) {
            this.update({ props:"hover", items:item, duration:opts.hovertime });
          });
          chart.on("mouseout", function(event, item) {
            this.update({ props:"update", items:item, duration:opts.hovertime });
          });
        }

        // If extra callbacks are specified for mouseover and out, add them.
        if (opts.mouseover) chart.on("mouseover", opts.mouseover);
        if (opts.mouseout)  chart.on("mouseout",  opts.mouseout);

        if (opts.resizable) self.enableResizable();

        // If the data arrived earlier, use it.
        if (this.pendingData) self.loadPendingData();

        if (self.dataReady()) self.initialUpdate();

        if (opts.smart_size) {
          if (ggvis.inViewerPanel()) {
            self.resizeToWindow(0);
          } else {
            self.resizeWrapperToPlot();
          }

          self.enableAutoResizeToWindow();
        }

      });
    };

    // Get the div which is around ggvis-output
    prototype.getDiv = function() {
      return $("#" + this.plotId);
    };

    // Wrapper div, which includes sizing handle and gear
    prototype.getWrapper = function() {
      return this.getDiv().parent();
    };

    // Get the marks object (the Canvas or SVG object, which is rendered too)
    prototype.getMarks = function() {
      // Can't do $vega.children(".marks") because it doesn't work for SVG DOM
      // objects. So we'll just grab any svg or canvas object.
      return $(this.chart._el).children("svg, canvas");
    };

    // Set the width of the chart to the wrapper div. If keep_aspect is true,
    // also set the height to maintain the aspect ratio.
    prototype.resizeToWrapper = function(duration, keep_aspect) {
      if (duration === undefined) duration = this.opts.duration;
      if (duration === undefined) duration = 0;
      if (keep_aspect === undefined) keep_aspect = this.opts.keep_aspect;
      if (keep_aspect === undefined) keep_aspect = false;

      var $div = this.getDiv(),
          $wrap = this.getWrapper(),
          $gear = $div.siblings().filter(".plot-gear-icon"),
          chart = this.chart,
          padding = chart.padding(),
          ratio = this.opts.width/this.opts.height;

      var newWidth = $wrap.width() - $gear.width() - padding.left - padding.right,
          newHeight = $wrap.height() - padding.top - padding.bottom;

      if (keep_aspect) {
        if (newHeight > newWidth / ratio) {
          newHeight = Math.floor(newWidth / ratio);
        } else if (newHeight < newWidth / ratio) {
          newWidth = Math.floor(newHeight * ratio);
        }
      }
      // Chart height ends up 5 pixels too large, so compensate for it
      newHeight -= 5;

      chart.width(newWidth);
      chart.height(newHeight);
      chart.update({ duration: duration });
    };

    // Set width and height to fill window
    prototype.resizeToWindow = function(duration) {
      var $win = $(window);
      var $body = $('body');
      var $wrap = this.getWrapper();

      // Left and right padding of body element
      var padding_left  = parseFloat($body.css("padding-left").replace("px", ""));
      var padding_right = parseFloat($body.css("padding-right").replace("px", ""));

      // Resize the wrapper div to the window - take off a little extra to make
      // sure it fits
      $wrap.width($win.width() - 5 - padding_left - padding_right);
      $wrap.height($win.height() - 5);

      this.resizeToWrapper(duration);
    };

    // Change the dimensions of the wrapper div to fit the plot.
    // This is useful when the we're not auto-sizing the plot, and the plot is
    // smaller than the window; if we don't do this, then the div will take the
    // full window width, but the plot will be smaller.
    prototype.resizeWrapperToPlot = function() {
      var chart   = this.chart;
      var $wrap   = this.getWrapper();  // wrapper around $div
      var $div    = this.getDiv();      // ggvis div, containing $el
      var $gear   = $div.siblings().filter(".plot-gear-icon");
      var $vega   = $(chart._el);       // Immediate wrapper around marks
      var $marks  = this.getMarks();

      // Need to use getAttribute because itt works for both svg and canvas
      // DOM objects. (marks.width doesn't work for SVG, nor does)
      var width = Math.ceil($marks.width());
      // There are 5 extra pixels in the bottom
      var height = Math.ceil($marks.height() + 5);

      $vega.width(width).height(height);
      $div.width(width).height(height);
      $wrap.width(width + $gear.width()).height(height);
    };

    // Run an update on the chart for the first time
    prototype.initialUpdate = function() {
      // If chart hasn't been run yet, we need to run it once so that
      // resizeToWrapper will work properly (it needs the spec to have been run
      // before it can figure out what the padding will be).
      if (!this.initialized) this.chart.update({ duration: 0 });

      this.initialized = true;
    };

    // Make manually resizable (by dragging corner)
    prototype.enableResizable = function() {
      var $el = this.getDiv().parent();
      var self = this;

      // When done resizing, update chart with new width and height
      $el.resizable({
        helper: "ui-resizable-helper",
        grid: [10, 10],
        stop: function() { self.resizeToWrapper(); }
      });
    };

    // Make the plot auto-resize to fit window, if in viewer panel
    prototype.enableAutoResizeToWindow = function() {
      var self = this;
      var debounce_id = null;

      $(window).resize(function() {
        clearTimeout(debounce_id);
        debounce_id = setTimeout(function() {
          if (ggvis.inViewerPanel()) {
            self.resizeToWindow();
          }
        }, 100); // Debounce to 100ms
      });
    };

    prototype.loadPendingData = function() {
      this.chart.data(this.pendingData);
      delete this.pendingData;
    };

    // Returns true if all data objects for a spec have been registered, using
    // this.chart.data(dataset)
    prototype.dataReady = function() {
      var existing_data = Object.keys(this.chart.data());
      var expected_data = this.spec.data.map(function (x) {
        return x.name ;
      });

      return arraysEqual(existing_data, expected_data);
    };

    // Set the renderer, and update the renderer button and download link text if
    // present. Also update the chart (unless update is false).
    // renderer is either "canvas" or "svg".
    prototype.setRenderer = function(renderer, update) {
      if (update === undefined) update = true;

      this.renderer = renderer;
      if (update) {
        this.chart.renderer(renderer).update();
      }
      this.setRendererButton(renderer);
      this.updateDownloadButtonText(renderer);
    };

    // Set the value of the renderer button, if present
    prototype.setRendererButton = function(renderer) {
      var $el = $("#" + this.plotId + "_renderer_" + renderer);

      // Toggle the renderer buttons when clicked
      $el.addClass('active');
      $el.siblings().removeClass('active');
    };

    // Given an <a> element, set the href of that element to the canvas content
    // of the plot converted to SVG or PNG. This will set the href when the link
    // is clicked; the download happens when it is released.
    prototype.updateDownloadLink = function(el) {
      var plot = $("#" + this.plotId + ".ggvis-output .marks")[0];
      var imageUrl;

      if (this.renderer === "svg") {
        // Extract the svg code and add needed xmlns attribute
        var svg = $(plot).clone().attr("xmlns", "http://www.w3.org/2000/svg");
        // Convert to string
        svg = $('<div>').append(svg).html();
        imageUrl = "data:image/octet-stream;base64,\n" + btoa(svg);

      } else if (this.renderer === "canvas") {
        imageUrl = plot.toDataURL("image/png").replace("image/png", "image/octet-stream");
      }

      // Set download filename and data URL
      var ext = "";
      if      (this.renderer === "svg")    ext = ".svg";
      else if (this.renderer === "canvas") ext = ".png";
      el.setAttribute("download", this.plotId + ext);
      el.setAttribute("href", imageUrl);
    };

    prototype.updateDownloadButtonText = function(renderer) {
      var $el = $(this.plotId + "_download");
      if ($el) {
        var filetype = "";
        if      (renderer === "svg")    filetype = "SVG";
        else if (renderer === "canvas") filetype = "PNG";

        $el.text("Download " + filetype);
      }
    };


    // Private methods ------------------------------------------------
    // Returns true if arrays have same contents (in any order), false otherwise.
    function arraysEqual(a, b) {
      return $(a).not(b).length === 0 && $(b).not(a).length === 0;
    }

    return Plot;
  })(); // ggvis.Plot

  return ggvis;
})();


$(function(){ //DOM Ready

  // Don't close the dropdown when objects in it are clicked (by default
  // the dropdown menu closes when anything inside is clicked).
  // Need to bind to body instead of document for e.stopPropogation to catch
  // at appropriate point.
  $("body").on('click', '.ggvis-control.dropdown-menu', function(e) {
    e.stopPropagation();
  });

  $("body").on("click", ".ggvis-download", function() {
    var plot = ggvis.plots[$(this).data("plot-id")];
    plot.updateDownloadLink(this);
  });

  $("body").on("click", ".ggvis-renderer-buttons .btn", function(e) {
    var $el = $(this);
    var plot = ggvis.plots[$el.data("plot-id")];

    plot.setRenderer($el.data("renderer"));

    // Don't close the dropdown
    e.stopPropagation();
  });

});
