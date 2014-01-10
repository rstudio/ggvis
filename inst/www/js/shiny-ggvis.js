/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global Shiny, ggvis, vg*/
$(function(){ //DOM Ready

  var _ = window.lodash;

  var ggvisOutputBinding = new Shiny.OutputBinding();
  $.extend(ggvisOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-ggvis-output');
    },
    onValueError: function(el, err) {
      Shiny.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function(el, data) {
      vg.parse.spec(data.spec, function(chart) {
        chart({el: el}).update({duration: 250});
      });
    }
  });
  Shiny.outputBindings.register(ggvisOutputBinding, 'shiny.ggvisOutput');

  // A customized version of Shiny's htmlOutputBinding which can call a plot's
  // onControlOutput function when outputs are updated drawn.
  var ggvisControlOutputBinding = new Shiny.OutputBinding();
  $.extend(ggvisControlOutputBinding, {
    find: function(scope) {
      return $(scope).find('.ggvis-control-output');
    },
    onValueError: function(el, err) {
      Shiny.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function(el, data) {
      var $el = $(el);

      Shiny.unbindAll(el);
      $el.html(data);
      Shiny.initializeInputs(el);
      Shiny.bindAll(el);

      // Run onControlOutput for each plot listed in data-plot-id
      var plotId = $el.data('plot-id');
      if (plotId !== undefined) {
        var ids = plotId.split(/ +/);

        for (var i = 0; i < ids.length; i++) {
          var plot = ggvis.plots[ids[i]];
          if (plot && plot.onControlOutput) {
            plot.onControlOutput();
          }
        }
      }
    }
  });
  Shiny.outputBindings.register(ggvisControlOutputBinding, 'shiny.ggvisControlOutput');


  // Receive data object and dispatch to appropriate vega object
  Shiny.addCustomMessageHandler("ggvis_data", function(message) {
    var plotId = message.plotId;
    var name = message.name;
    var data = message.value[0].values;
    var format = message.value[0].format;

    var plot = ggvis.getPlot(plotId);

    if (plot.chart) {
      // If the plot exists already, feed it the data
      var dataset = {};

      dataset[name] = vg.data.read(data, format);
      plot.chart.data(dataset);

      // If all data objects have been received, update
      if (plot.dataReady()) {
        if (!plot.initialized) {
          plot.initialUpdate();
        } else {
          plot.chart.update({ duration: plot.opts.duration });
        }
      }

    } else {
      // The plot doesn't exist, save the data for when the plot arrives
      if (!plot.pendingData) plot.pendingData = {};

      plot.pendingData[name] = data;
    }
  });


  // Receive a vega spec and parse it
  Shiny.addCustomMessageHandler("ggvis_vega_spec", function(message) {
    var plotId = message.plotId;
    var spec = message.spec;
    var plot = ggvis.getPlot(plotId);

    // Brush handler
    var brush_policy = spec.ggvis_opts.brush_policy || "debounce";
    var brushHandler;
    if (brush_policy === "throttle") {
      brushHandler = _.throttle(createBrushHandler(plotId), spec.ggvis_opts.brush_delay);
    } else if (brush_policy === "debounce") {
      brushHandler = _.debounce(createBrushHandler(plotId), spec.ggvis_opts.brush_delay);
    }

    plot.parseSpec(spec, {
      brush: {
        handlers: { updateItems: brushHandler}
      }
    });
  });

  // Send information about the current brush
  function createBrushHandler(plotId) {
    return function(info) {
      info.items = info.items.map(function(item) {
        var newitem = $.extend({}, item.datum.data);
        newitem.key__ = item.key;
        return newitem;
      });
      Shiny.onInputChange("ggvis_" + plotId + "_brush", info);
    };
  }

  // Tooltip message handler
  Shiny.addCustomMessageHandler('ggvis_tooltip', function(data) {
    if (data.visible) {
      // Remove any existing tooltips
      $('.ggvis-tooltip').remove();

      // Add the tooltip div
      var $el = $('<div id="ggvis-tooltip" class="ggvis-tooltip"></div>')
        .appendTo('body');

      $el.html(data.html);
      $el.css({
        left:  data.pagex,
        top:   data.pagey,
        display: "block"
      });

    } else {
      $('.ggvis-tooltip').remove();
    }
  });

  // ---------------------------------------------------------------------------
  // Interaction event handlers
  // These are defined here instead of ggvis.js because at present all of the
  // event handlers use shiny.
  // ---------------------------------------------------------------------------
  // Keyboard handler
  // Sends ggvis_xxxx_key_press events
  ggvis.handlers.keyboard = (function() {
    var keyboard = function(plot, h_spec) {
      this.plot = plot;
      this.h_spec = h_spec;

      // jQuery event ID for naming event handlers and removing later
      this._eventId = "ggvis_" + h_spec.id;
      // The prefix to the shiny input name
      this._inputId = "ggvis_" + h_spec.id + "_key_press";
      // Used for keeping track of number of key events. Needed so that Shiny
      // will send info when same key is pressed multiple times in a row.
      this._counter = 0;

      var self = this;

      // keypress handler works for regular character keys
      $(document).on("keypress." + this._eventId, function(e) {
        var str = String.fromCharCode(e.which);
        self._sendValue(str);
      });

      // keydown handler for special keys that aren't caught by keypress,
      // like arrows
      $(document).on("keydown." + this._eventId, function(e) {
        var str = keycodes[e.which];
        if (str) {
          self._sendValue(str);
        }
      });
    };

    // Mappings for keycodes of special characters
    var keycodes = {
      8: "backspace",
      9: "tab",
      27: "esc",
      37: "left",
      38: "up",
      39: "right",
      40: "down",
      46: "delete"
    };

    var prototype = keyboard.prototype;

    prototype.remove = function() {
      $(document).off("keypress." + this._eventId);
      $(document).off("keydown." + this._eventId);
    };

    prototype._sendValue = function(str) {
      this._counter++;
      Shiny.onInputChange(this._inputId, { value: str, _nonce: this._counter });
    };

    return keyboard;
  })(); // ggvis.handlers.keyboard


  // ---------------------------------------------------------------------------
  // Hover handler
  // Sends ggvis_xxxx_mouse_over and ggvis_xxxx_mouse_out events
  ggvis.handlers.hover = (function() {
    var hover = function(plot, h_spec) {
      this.plot = plot;
      this.h_spec = h_spec;

      // Event ID for naming event handlers and removing later
      this._eventId = "ggvis_" + h_spec.id;
      // The prefix to the shiny input name
      this._inputIdPrefix = "ggvis_" + h_spec.id;

      plot.chart.on("mouseover." + this._eventId, this._createMouseOverHandler());
      plot.chart.on("mouseout."  + this._eventId, this._createMouseOutHandler());
    };

    var prototype = hover.prototype;

    prototype.remove = function() {
      this.plot.chart.off("mouseover." + this._eventId);
      this.plot.chart.off("mouseout."  + this._eventId);
    };

    prototype._createMouseOverHandler = function() {
      var self = this;
      return function(event, item) {
        Shiny.onInputChange(self._inputIdPrefix + "_mouse_over",
          {
            plot_id: self.plot.plotId,
            data: item.datum.data,
            pagex: event.pageX,
            pagey: event.pageY
          }
        );
      };
    };

    prototype._createMouseOutHandler = function() {
      var self = this;
      return function(event, item) {
        /* jshint unused: false */
        Shiny.onInputChange(self._inputIdPrefix + "_mouse_out",
          {
            plot_id: self.plot.plotId,
            data: null,
            pagex: null,
            pagey: null
          }
        );
      };
    };

    return hover;
  })(); // ggvis.handlers.hover


});
