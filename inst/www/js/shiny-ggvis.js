/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global Shiny, vg*/
$(function(){ //DOM Ready

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
        chart({el: el}).update();
      });
    }
  });
  Shiny.outputBindings.register(ggvisOutputBinding, 'shiny.ggvisOutput');


  var pendingData = {};
  Shiny.addCustomMessageHandler("ggvis_data", function(message) {
    var plotId = message.plotId;
    var name = message.name;
    var value = message.value[0].values;

    if (allPlots[plotId]) {
      // If the plot exists already, feed it the data
      var dataset = {};
      dataset[name] = value;
      allPlots[plotId].data(dataset);
      allPlots[plotId].update();

      updateGgvisDivSize(plotId);
    } else {
      // The plot doesn't exist, save it for when the plot arrives
      if (!pendingData[plotId])
        pendingData[plotId] = {};
      pendingData[plotId][name] = value;
    }
  });


  // Receive a vega spec and parse it
  Shiny.addCustomMessageHandler("ggvis_vega_spec", function(message) {
    var plotId = message.plotId;
    var spec = message.spec;
    var renderer = message.renderer;

    vg.parse.spec(spec, function(chart) {
      var selector = ".ggvis-output#" + plotId;
      var $el = $(selector);
      chart = chart({ el: selector, renderer: renderer });
      $el.data("ggvis-chart", chart);
      ggvisInit(plotId);

      // When done resizing, update with new width and height
      $el.resizable({
        helper: "ui-resizable-helper",
        grid: [10, 10],
        stop: function() {
          var padding = chart.padding();
          chart.width($el.width() - padding.left - padding.right);
          chart.height($el.height() - padding.top - padding.bottom);
          chart.update();
        }
      });
    });
  });


  // Sets height and width of wrapper div to contain the plot area.
  // This is so that the resize handle will be put in the right spot.
  function updateGgvisDivSize(plotId) {
    var $el = $(".ggvis-output#" + plotId);
    var $plotarea = $el.find("div.vega > .marks");

    $el.width($plotarea.width());
    $el.height($plotarea.height());
  }


  var allPlots = {};
  function ggvisInit(plotId) {
    var chart = $(".ggvis-output#" + plotId).data("ggvis-chart");
    allPlots[plotId] = chart;

    if (pendingData[plotId]) {
      // The data arrived earlier; use it.
      chart.data(pendingData[plotId]);
      chart.update();
      delete pendingData[plotId];

      updateGgvisDivSize(plotId);
    }
  }
});
