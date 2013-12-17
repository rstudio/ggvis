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
      handlers: {
        mouseover: _.throttle(createMouseOverHandler(plotId), 100),
        mouseout: _.throttle(createMouseOutHandler(plotId), 100)
      },
      brush: {
        handlers: { updateItems: brushHandler}
      }
    });
  });


  // Returns a mouseover handler with plotId
  function createMouseOverHandler(plotId) {
    return function(event, item) {
      Shiny.onInputChange("ggvis_" + plotId + "_hover",
        {
          plot_id: plotId,
          data: item.datum.data,
          pagex: event.pageX,
          pagey: event.pageY
        }
      );
    };
  }

  // Returns a mouseout handler with plotId
  function createMouseOutHandler(plotId) {
    return function(event, item) {
      /* jshint unused: false */
      Shiny.onInputChange("ggvis_" + plotId + "_hover",
        {
          plot_id: plotId,
          data: null,
          pagex: null,
          pagey: null
        }
      );
    };
  }

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

});
