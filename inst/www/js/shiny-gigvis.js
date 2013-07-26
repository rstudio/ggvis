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
  var value = message.value;
  
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

var allPlots = {};
window.gigvisInit = function(plotId) {
  var chart = $("#" + plotId).data("gigvis-chart");
  allPlots[plotId] = chart;

  if (pendingData[plotId]) {
    // The data arrived earlier; use it.
    chart.data(pendingData[plotId]);
    chart.update();
    delete pendingData[plotId];
  }
};