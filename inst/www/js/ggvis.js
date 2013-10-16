/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global vg*/
var ggvis = window.ggvis = window.ggvis || {};  // If already defined, just extend it

// Keep track of information about all plots: contains GgvisPlot objects
ggvis.plots = {};
ggvis.renderer = null; // "canvas" or "svg"

ggvis.getPlot = function(plotId) {
  // Get the GgvisPlot object (and create if needed)
  if (!this.plots[plotId]) {
    this.plots[plotId] = new GgvisPlot(plotId);
  }
  return this.plots[plotId];
};

// Given the name of a plot and an <a> element, set the href of that element
// to the canvas content of the plot converted to PNG. This will set the href
// when the link is clicked; the download happens when it is released.
ggvis.updateDownloadLink = function(plotId, el) {
  var plot = $("#" + plotId + ".ggvis-output .marks")[0];
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
  el.setAttribute("download", plotId + ext);
  el.setAttribute("href", imageUrl);
};

// Set the renderer for all plots (unless updatePlots is false), and update
// the renderer selector and download button if present.
// renderer is either "canvas" or "svg"
ggvis.setRenderer = function(renderer, updatePlots) {
  if (updatePlots === undefined) updatePlots = true;

  this.renderer = renderer;
  if (updatePlots) this.setPlotRenderers(renderer);
  this.setRendererChooser(renderer);
  this.updateDownloadButtonText(renderer);
};

// Change the renderer for all plots
ggvis.setPlotRenderers = function(renderer) {
  for (var plotId in this.plots) {
    if (this.plots.hasOwnProperty(plotId)) {
      this.plots[plotId].chart.renderer(renderer).update();
    }
  }
};

// Set the value of the renderer selector, if present
ggvis.setRendererChooser = function(renderer) {
  var $el = $("#ggvis_renderer_" + renderer);

  // Toggle the renderer buttons when clicked
  $el.addClass('active');
  $el.siblings().removeClass('active');
};

ggvis.updateDownloadButtonText = function(renderer) {
  var $el = $("#ggvis_download");
  if ($el) {
    var filetype = "";
    if      (renderer === "svg")    filetype = "SVG";
    else if (renderer === "canvas") filetype = "PNG";

    $el.text("Download " + filetype);
  }
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
  // opts is an optional object which can have entries:
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

      // If the overall renderer option hasn't been set, set it
      if(ggvis.renderer === null) {
        ggvis.setRenderer(self.opts.renderer, false);
      }

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
  resizeToDiv: function(duration, keep_aspect) {
    if (duration === undefined) duration = this.opts.duration;
    if (duration === undefined) duration = 0;
    if (keep_aspect === undefined) keep_aspect = this.opts.keep_aspect;
    if (keep_aspect === undefined) keep_aspect = false;

    var $el = this.getDiv();
    var chart = this.chart;
    var padding = chart.padding();
    var ratio = this.opts.width/this.opts.height;

    var newWidth = $el.width() - padding.left - padding.right;
    var newHeight;
    if (keep_aspect) {
      newHeight = newWidth / ratio;
    } else {
      newHeight = $el.height() - padding.top - padding.bottom;
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
    // resizeToDiv will work properly (it needs the spec to have been run
    // before it can figure out what the padding will be).
    if (!this.initialized) this.chart.update({ duration: 0 });

    this.resizeToDiv(0);
    this.initialized = true;
  },

  // Make manually resizable (by dragging corner)
  enableResizable: function() {
    var $el = this.getDiv();
    var self = this;

    // When done resizing, update chart with new width and height
    $el.resizable({
      helper: "ui-resizable-helper",
      grid: [10, 10],
      stop: function() { self.resizeToDiv(); }
    });
  },

  // Make the plot auto-resize to fit available width - debounce to 100ms
  enableAutoWidth: function() {
    var self = this;
    var debounce_id = null;

    $(window).resize(function() {
      clearTimeout(debounce_id);
      debounce_id = setTimeout(function() { self.resizeToDiv(); }, 100);
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

  // Returns true if arrays have same contents (in any order), false otherwise.
  arraysEqual: function(a, b) {
    return $(a).not(b).length === 0 && $(b).not(a).length === 0;
  }
};


$(function(){ //DOM Ready
  var $el;

  // Attach event handlers to buttons
  $el = $("#quit");
  if ($el) {
    $el.on("click", function() { window.close(); });
  }

  $el = $("#ggvis_download");
  if ($el) {
    $el.on("click", function() {
      var plotId = $(this).data("plot-id");
      ggvis.updateDownloadLink(plotId, this);
    });
  }

  $el = $("#ggvis_renderer_buttons .btn");
  if ($el) {
    $el.on("click", function() {
      ggvis.setRenderer(this.textContent.toLowerCase());
    });
  }
});
