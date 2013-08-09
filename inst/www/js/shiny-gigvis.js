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
  var plotId = message.plotId;
  var name = message.name;
  var value = message.value[0].values;

  if (allPlots[plotId]) {
    // If the plot exists already, feed it the data
    var dataset = {};
    dataset[name] = value;
    allPlots[plotId].data(dataset);
    allPlots[plotId].update();

    updateGigvisDivSize(plotId);
  } else {
    // The plot doesn't exist, save it for when the plot arrives
    if (!pendingData[plotId])
      pendingData[plotId] = {}
    pendingData[plotId][name] = value;
  }
});


// Receive a vega spec and parse it
Shiny.addCustomMessageHandler("gigvis_vega_spec", function(message) {
  var plotId = message.plotId;
  var spec = message.spec;

  vg.parse.spec(spec, function(chart) {
    var selector = ".gigvis-output#" + plotId;
    var $el = $(selector);
    var chart = chart({ el: selector, renderer: "canvas" });
    $el.data("gigvis-chart", chart);
    gigvisInit(plotId);

    // When done resizing, update with new width and height
    $el.resizable({
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
window.updateGigvisDivSize = function(plotId) {
  var $el = $(".gigvis-output#" + plotId);
  var $plotarea = $el.find("div.vega > .marks");

  $el.width($plotarea.width());
  $el.height($plotarea.height());
}

var allPlots = {};
window.gigvisInit = function(plotId) {
  var chart = $(".gigvis-output#" + plotId).data("gigvis-chart");
  allPlots[plotId] = chart;

  if (pendingData[plotId]) {
    // The data arrived earlier; use it.
    chart.data(pendingData[plotId]);
    chart.update();
    delete pendingData[plotId];

    updateGigvisDivSize(plotId);
  }
};
