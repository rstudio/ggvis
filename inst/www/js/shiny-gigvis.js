$(function(){ //DOM Ready

  var gigvisOutputBinding = new Shiny.OutputBinding();
  $.extend(gigvisOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-gigvis-output');
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
  Shiny.outputBindings.register(gigvisOutputBinding, 'shiny.gigvisOutput');

});

var pendingData = {};
Shiny.addCustomMessageHandler("gigvis_data", function(message) {
  var plot = message.plot;
  var name = message.name;
  var value = message.value[0].values;

  if (allPlots[plot]) {
    // If the plot exists already, feed it the data
    var dataset = {};
    dataset[name] = value;
    allPlots[plot].data(dataset);
    allPlots[plot].update();
  } else {
    // The plot doesn't exist, save it for when the plot arrives
    if (!pendingData[plot])
      pendingData[plot] = {}
    pendingData[plot][name] = value;
  }
});


// Receive a vega spec and parse it
Shiny.addCustomMessageHandler("gigvis_vega_spec", function(message) {
  var plotId = message.plotId;
  var spec = message.spec;

  vg.parse.spec(spec, function(chart) {
    var selector = ".gigvis-output#" + plotId;
    var chart = chart({ el: selector, renderer: "canvas" });
    $(selector).data("gigvis-chart", chart);
    gigvisInit(plotId);
  });
});


var allPlots = {};
window.gigvisInit = function(plotId) {
  var chart = $(".gigvis-output#" + plotId).data("gigvis-chart");
  allPlots[plotId] = chart;

  if (pendingData[plotId]) {
    // The data arrived earlier; use it.
    chart.data(pendingData[plotId]);
    chart.update();
    delete pendingData[plotId];
  }
};