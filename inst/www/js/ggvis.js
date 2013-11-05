/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global vg, ggvis:true, lodash*/

ggvis = (function(_) {
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

  // Are we in a viewer pane?
  ggvis.inViewerPane = function() {
    return queryVar("viewer_pane") === "1";
  };

  // Private methods --------------------------------------------------

  // Returns the value of a GET variable
  function queryVar (name) {
    return decodeURI(window.location.search.replace(
      new RegExp("^(?:.*[&\\?]" +
                 encodeURI(name).replace(/[\.\+\*]/g, "\\$&") +
                 "(?:\\=([^&]*))?)?.*$", "i"),
      "$1"));
  }


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

        if (self.opts.brush) self.enableBrushing();

        if (ggvis.inViewerPane()) {
          self.enableAutoResizeToWindow();
        } else if (opts.resizable) {
          self.enableResizable();
        }

        // If the data arrived earlier, use it.
        if (this.pendingData) self.loadPendingData();

        if (self.dataReady()) self.initialUpdate();
      });
    };

    // Get the div which wraps the svg or canvas object (created by vega).
    prototype.getVegaDiv = function() {
      // This is also known is this.getDiv().children(".vega")
      return $(this.chart._el);
    };

    // Get the div which wraps the .vega div
    prototype.getDiv = function() {
      return $("#" + this.plotId);
    };

    // Wrapper div, which includes sizing handle and gear
    prototype.getWrapper = function() {
      return this.getDiv().parent();
    };

    // Get the marks object (the Canvas or SVG object, which is rendered too)
    prototype.getMarks = function() {
      // Can't do this.getVegaDiv().children(".marks") because it doesn't work
      // for SVG DOM objects. So we'll just grab any svg or canvas object.
      return this.getVegaDiv().children("svg, canvas");
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
      var $body = $('body');
      var $wrap = this.getWrapper();

      // Left and right padding of body element
      var padding_left  = parseFloat($body.css("padding-left").replace("px", ""));
      var padding_right = parseFloat($body.css("padding-right").replace("px", ""));

      // Resize the wrapper div to the window, inside of scrollbars if present
      // The wrapper has overflow:hidden so that objects inside of it won't
      // scrollbars to appear while it's being resized.
      var docEl = document.documentElement;
      $wrap.width(docEl.clientWidth - padding_left - padding_right);
      $wrap.height(docEl.clientHeight);
      // Resize again - needed because if the first resize caused a scrollbar to
      // disappear, there will be a little extra space.
      $wrap.width(docEl.clientWidth - padding_left - padding_right);
      $wrap.height(docEl.clientHeight);

      // Now if there are any other elements in the body that cause the page to
      // be larger than the window (like controls), we need to shrink the
      // plot so that they end up inside the window.
      $wrap.height(2 * docEl.clientHeight - $body.height());

      this.resizeToWrapper(duration);
    };

    // Change the dimensions of the wrapper div to fit the plot.
    // This is useful when the we're not auto-sizing the plot, and the plot is
    // smaller than the window; if we don't do this, then the div will take the
    // full window width, but the plot will be smaller.
    prototype.resizeWrapperToPlot = function() {
      var $wrap   = this.getWrapper();  // wrapper around $div
      var $div    = this.getDiv();      // ggvis div, containing $el
      var $vega   = this.getVegaDiv();  // Immediate wrapper around marks
      var $marks  = this.getMarks();
      var $gear   = $div.siblings().filter(".plot-gear-icon");

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

      // Resizing to fit has to happen after the initial update
      if (ggvis.inViewerPane()) {
        this.resizeToWindow(0);
      } else {
        this.resizeWrapperToPlot();
      }
    };

    // Make manually resizable (by dragging corner)
    prototype.enableResizable = function() {
      var $el = this.getDiv().parent();
      var self = this;

      // When done resizing, update chart with new width and height
      $el.resizable({
        helper: "ui-resizable-helper",
        grid: [10, 10],
        handles: "se",
        stop: function() { self.resizeToWrapper(); }
      });
    };

    // Make the plot auto-resize to fit window, if in viewer panel
    prototype.enableAutoResizeToWindow = function() {
      var self = this;
      var debounce_id = null;

      $(window).resize(function() {
        clearTimeout(debounce_id);
        // Debounce to 100ms
        debounce_id = setTimeout(function() { self.resizeToWindow(); }, 100);
      });
    };

    // This is called when control outputs for a plot are updated
    prototype.onControlOutput = function() {
      if (ggvis.inViewerPane()) {
        this.resizeToWindow(0);
      }
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
      var $el = $("#" + this.plotId + "_download");
      if ($el[0]) {
        var filetype = "";
        if      (renderer === "svg")    filetype = "SVG";
        else if (renderer === "canvas") filetype = "PNG";

        $el.text("Download " + filetype);
      }
    };


    // Private methods ------------------------------------------------

    // Returns all top-level marks in the scene graph
    prototype._allMarks = function() {
      return this.chart.model().scene().items[0].items;
    };

    prototype._getSceneBounds = function() {
      return this.chart.model().scene().items[0].bounds;
    };

    // Return the brush mark; if not present, return null.
    prototype._getBrushMark = function() {
      // We can identify the brush mark because it draws data from ggvis_brush.
      var brushMark = _.find(this._allMarks(), function(mark) {
        return getMarkPropKey(mark, "ggvis", "data") === "ggvis_brush";
      });

      if (brushMark === undefined) return null;

      return brushMark;
    };

    prototype._getBrushItem = function() {
      var brushMark = this._getBrushMark();
      if (brushMark === null || brushMark.items === null) return null;

      return brushMark.items[0];
    };

    // Return all brushable items
    prototype._getBrushableItems = function() {
      var brushableMarks = _.filter(this._allMarks(), function(mark) {
        if (getMarkProp(mark, "brush"))
          return true;
        else
          return false;
      });

      var items = _.pluck(brushableMarks, "items");
      return _.flatten(items);
    };


    // Internal functions----------------------------------------------

    // Returns true if arrays have same contents (in any order), false otherwise.
    function arraysEqual(a, b) {
      return $(a).not(b).length === 0 && $(b).not(a).length === 0;
    }

    // Given a mark and a property name, return the property function
    function getMarkProp(mark, propname) {
      if (propname === undefined || propname === null) {
        return null;
      }

      return mark.def.properties[propname];
    }

    // Given a property function and (optional) key, return the value of that
    // key. If key is undefined, then return an object with all keys.
    function getPropKey(property, key) {
      if (property === undefined || property === null) {
        return null;
      }

      // Call the property function on a dummy object
      var temp = {};
      property(temp);

      if (key === undefined || key === null) {
        return temp;
      } else {
        return temp[key];
      }
    }

    // Given a mark, a property name, and an (optional) key, return the mark's
    // property's key. If key is null or undefined, return the mark's property
    // object.
    function getMarkPropKey(mark, propname, key) {
      var property = getMarkProp(mark, propname);
      return getPropKey(property, key);
    }


    // Brushing -------------------------------------------------------
    prototype.enableBrushing = function() {
      var self = this;
      var $div = this.getDiv();
      var chart = this.chart;

      var brushBounds = new vg.Bounds();  // Current brush bounds
      var lastMatchingItems = [];  // Items that were brushed in previous call
      var mouseDownStart = null;   // Coordinates where mouse was clicked
      var lastMouse = null;        // Previous mouse coordinate
      var brushing = false;
      var dragging = false;

      // Remove any existing handlers
      $div.off("mousedown.ggvis_brush");
      $div.off("mouseup.ggvis_brush");
      $div.off("mousemove.ggvis_brush");

      // Hook up handlers
      $div.on("mousedown.ggvis_brush", "div.vega", function (event) {
        var point = removePadding(mouseOffset(event));

        if (brushBounds.contains(point.x, point.y)) {
          startDragging(point);
        } else {
          startBrushing(point);
        }
      });
      $div.on("mouseup.ggvis_brush", "div.vega", function (event) {
        /* jshint unused: false */
        if (dragging) stopDragging();
        if (brushing) stopBrushing();
      });
      $div.on("mousemove.ggvis_brush", "div.vega", function (event) {
        var point = removePadding(mouseOffset(event));
        if (dragging) dragTo(point);
        if (brushing) brushTo(point);
      });

      // x/y coords are relative to the containing div. We need to account for the
      // padding that surrounds the data area by removing the padding before we
      // compare it to any scene item bounds.
      function removePadding(point) {
        return {
          x: point.x - chart.padding().left,
          y: point.y - chart.padding().top
        };
      }

      function mouseOffset(e) {
        return {
          x: e.offsetX,
          y: e.offsetY
        };
      }

      // Dragging functions
      function startDragging(point) {
        dragging = true;
        lastMouse = point;
        mouseDownStart = point;
      }
      function dragTo(point) {
        if (!dragging) return;

        var dx = point.x - lastMouse.x;
        var dy = point.y - lastMouse.y;

        brushBounds.translate(dx, dy);
        updateBrush();

        lastMouse = point;
      }
      function stopDragging() {
        dragging = false;
        mouseDownStart = null;
      }

      // Brushing functions
      function startBrushing(point) {
        // Reset brush
        brushBounds.set(0, 0, 0, 0);
        updateBrush();

        brushing = true;
        mouseDownStart = point;
      }

      function stopBrushing() {
        brushing = false;
        mouseDownStart = null;
      }

      function brushTo(point) {
        if (!brushing) return; // We're not brushing right now

        var limits = self._getSceneBounds();

        // Calculate the bounds based on start and end points
        var end = point;
        var maxX = Math.min(Math.max(mouseDownStart.x, end.x), limits.x2);
        var minX = Math.max(Math.min(mouseDownStart.x, end.x), limits.x1);
        var maxY = Math.min(Math.max(mouseDownStart.y, end.y), limits.y2);
        var minY = Math.max(Math.min(mouseDownStart.y, end.y), limits.y1);

        brushBounds.set(minX, minY, maxX, maxY);

        updateBrush();
      }

      // Update the brush with new coordinates stored in brushBounds variable.
      // This updates the box, and calls update on scenegraph items which have
      // a change of brush state.
      function updateBrush() {
        // Update the brush bounding box
        chart.data({
          ggvis_brush: [{
            x: brushBounds.x1,
            y: brushBounds.y1,
            width: brushBounds.width(),
            height: brushBounds.height()
          }]
        });

        // Find the items in the current scene that match
        var items = self._getBrushableItems();
        var matchingItems = [];
        for (var i = 0; i < items.length; i++) {
          if (brushBounds.intersects(items[i].bounds)) {
            matchingItems.push(items[i]);
          }
        }

        // Clear any un-brushed items, then highlight new ones
        var newBrushItems = _.difference(matchingItems, lastMatchingItems);
        var unBrushItems = _.difference(lastMatchingItems, matchingItems);

        chart.update({ props: "brush", items: newBrushItems });
        // Need to update brushed items and brush rect in one step, because of
        // bug in Vega's canvas renderer.
        chart.update({ props: "update", items: unBrushItems.concat(self._getBrushItem()) });

        lastMatchingItems = matchingItems;
      }

    };

    return Plot;
  })(); // ggvis.Plot

  return ggvis;

})(lodash);


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
