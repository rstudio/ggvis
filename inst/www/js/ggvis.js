/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global vg*/
var ggvis = window.ggvis = window.ggvis || {};  // If already defined, just extend it

// Keep track of information about all plots: contains GgvisPlot objects
ggvis.plots = {};

// Get a GgvisPlot object with a particular name, creating it if needed
ggvis.getPlot = function(plotId) {
  if (!this.plots[plotId]) {
    this.plots[plotId] = new GgvisPlot(plotId);
  }
  return this.plots[plotId];
};


// GgvisPlot objects ----------------------------------------------------------
// Constructor for GgvisPlot objects
var GgvisPlot = function(plotId) {
  this.plotId = plotId;
  this.pendingData = {}; // Data objects that have been received but not yet used
  this.chart = null;     // Vega chart object on the page
  this.spec = null;      // Vega spec for this plot
  this.initialized = false; // Has update() or enter() been run?
  this.opts = {};
};

GgvisPlot.prototype = {
  // opts is an optional object which can have any entries that are in spec.opts
  // (they get merged on top of spec.opts), and additionally:
  // * mouseover: A callback for the "mouseover" event
  // * mouseout: A callback for the "mouseout" event
  // * hovertime: Number of milliseconds for a hover transition
  parseSpec: function(spec, opts) {
    var self = this;
    self.spec = spec;
    self.initialized = false;
    // Merge options passed to this function into options from the spec
    self.opts = $.extend(true, self.spec.ggvis_opts, opts);

    vg.parse.spec(spec, function(chart) {
      var $el = self.getDiv();

      // If hovertime is supplied, use that later in a custom callback,
      // instead of the default hover behavior.
      var hover = true;
      if (self.opts.hovertime && self.opts.hovertime !== 0) hover = false;

      chart = chart({
        el: "#" + self.plotId,
        renderer: self.opts.renderer || "canvas",
        hover: hover
      });
      // Save the chart object
      self.chart = chart;
      $el.data("ggvis-chart", chart);

      // Set the renderer (update buttons and download link)
      self.setRenderer(self.opts.renderer, false);

      // If hovertime is specified, set callbacks for hover behavior
      if (self.opts.hovertime && self.opts.hovertime !== 0) {
        chart.on("mouseover", function(event, item) {
          this.update({ props:"hover", items:item, duration:self.opts.hovertime });
        });
        chart.on("mouseout", function(event, item) {
          this.update({ props:"update", items:item, duration:self.opts.hovertime });
        });
      }

      // If extra callbacks are specified for mouseover and out, add them.
      if (self.opts.mouseover) chart.on("mouseover", self.opts.mouseover);
      if (self.opts.mouseout)  chart.on("mouseout",  self.opts.mouseout);

      if (self.opts.resizable) self.enableResizable();
      if (self.opts.auto_width) self.enableAutoWidth();

      // If the data arrived earlier, use it.
      if (this.pendingData) self.loadPendingData();
 
      if (self.dataReady()) self.initialUpdate();
    });
  },

  // Get the ggvis-output wrapper div
  getDiv: function() {
    return $("#" + this.plotId);
  },

  // Set the width of the chart to the wrapper div. If keep_aspect is true,
  // also set the height to maintain the aspect ratio.
  resizeToWrapper: function(duration, keep_aspect) {
    if (duration === undefined) duration = this.opts.duration;
    if (duration === undefined) duration = 0;
    if (keep_aspect === undefined) keep_aspect = this.opts.keep_aspect;
    if (keep_aspect === undefined) keep_aspect = false;

    var $div = this.getDiv(),
        $wrap = $div.parent(),
        $gear = $div.siblings().filter(".plot-gear-icon"),
        chart = this.chart,
        padding = chart.padding(),
        ratio = this.opts.width/this.opts.height;

    var newWidth = $wrap.width() - $gear.width() - padding.left - padding.right,
        newHeight;
    if (keep_aspect) {
      newHeight = newWidth / ratio;
    } else {
      newHeight = $wrap.height() - padding.top - padding.bottom;
    }
    // Chart height ends up 5 pixels too large, so compensate for it
    newHeight -= 5;

    chart.width(newWidth);
    chart.height(newHeight);
    chart.update({ duration: duration });
  },

  // Run an update on the chart for the first time
  initialUpdate: function() {
    // If chart hasn't been run yet, we need to run it once so that
    // resizeToWrapper will work properly (it needs the spec to have been run
    // before it can figure out what the padding will be).
    if (!this.initialized) this.chart.update({ duration: 0 });

    this.resizeToWrapper(0);
    this.initialized = true;
  },

  // Make manually resizable (by dragging corner)
  enableResizable: function() {
    var $el = this.getDiv().parent();
    var self = this;

    // When done resizing, update chart with new width and height
    $el.resizable({
      helper: "ui-resizable-helper",
      grid: [10, 10],
      stop: function() { self.resizeToWrapper(); }
    });
  },

  // Make the plot auto-resize to fit available width - debounce to 100ms
  enableAutoWidth: function(auto_width) {
    if (auto_width === undefined) auto_width = this.opts.auto_width;
    if (auto_width === undefined) auto_width = false;

    // auto_width can be true, false, or an array of 2 values
    if (auto_width === false) return;
    if (auto_width instanceof Array) {
      $("#" + this.plotId + "-container").css({
        "min-width": auto_width[0],
        "max-width": auto_width[1]
      });
    }

    var self = this;
    var debounce_id = null;

    $(window).resize(function() {
      clearTimeout(debounce_id);
      debounce_id = setTimeout(function() { self.resizeToWrapper(); }, 100);
    });
  },

  loadPendingData: function() {
    this.chart.data(this.pendingData);
    delete this.pendingData;
  },

  // Returns true if all data objects for a spec have been registered, using
  // this.chart.data(dataset)
  dataReady: function() {
    var existing_data = Object.keys(this.chart.data());
    var expected_data = this.spec.data.map(function (x) {
      return x.name ;
    });

    return this.arraysEqual(existing_data, expected_data);
  },

  // Set the renderer, and update the renderer button and download link text if
  // present. Also update the chart (unless update is false).
  // renderer is either "canvas" or "svg".
  setRenderer: function(renderer, update) {
    if (update === undefined) update = true;

    this.renderer = renderer;
    if (update) {
      this.chart.renderer(renderer).update();
    }
    this.setRendererButton(renderer);
    this.updateDownloadButtonText(renderer);
  },

  // Set the value of the renderer button, if present
  setRendererButton: function(renderer) {
    var $el = $("#" + this.plotId + "_renderer_" + renderer);

    // Toggle the renderer buttons when clicked
    $el.addClass('active');
    $el.siblings().removeClass('active');
  },

  // Given an <a> element, set the href of that element to the canvas content
  // of the plot converted to SVG or PNG. This will set the href when the link
  // is clicked; the download happens when it is released.
  updateDownloadLink: function(el) {
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
  },

  updateDownloadButtonText: function(renderer) {
    var $el = $(this.plotId + "_download");
    if ($el) {
      var filetype = "";
      if      (renderer === "svg")    filetype = "SVG";
      else if (renderer === "canvas") filetype = "PNG";

      $el.text("Download " + filetype);
    }
  },


  // Returns true if arrays have same contents (in any order), false otherwise.
  arraysEqual: function(a, b) {
    return $(a).not(b).length === 0 && $(b).not(a).length === 0;
  }
};


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
