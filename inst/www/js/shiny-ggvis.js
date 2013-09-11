/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global Shiny, ggvis, vg*/
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
        chart({el: el}).update({duration: 250});
      });
    }
  });
  Shiny.outputBindings.register(ggvisOutputBinding, 'shiny.ggvisOutput');


  Shiny.addCustomMessageHandler("ggvis_data", function(message) {
    var plotId = message.plotId;
    var name = message.name;
    var value = message.value[0].values;


    if (ggvis.charts[plotId]) {
      // If the plot exists already, feed it the data
      var dataset = {};
      dataset[name] = value;
      ggvis.charts[plotId].data(dataset);

      // If all data objects have been received, update.
      if (ggvis.data_ready(plotId)) {
        opts = {};
        if (ggvis.initialized[plotId]) opts.duration = 250;

        ggvis.charts[plotId].update(opts);
        ggvis.updateGgvisDivSize(plotId);
        ggvis.initialized[plotId] = true;
      }
    } else {
      // The plot doesn't exist, save the data for when the plot arrives
      if (!ggvis.pendingData[plotId])
        ggvis.pendingData[plotId] = {};

      ggvis.pendingData[plotId][name] = value;
    }
  });


  // Receive a vega spec and parse it
  Shiny.addCustomMessageHandler("ggvis_vega_spec", function(message) {
    var plotId = message.plotId;
    var spec = message.spec;

    // If no renderer already selected, set it here
    if (!ggvis.renderer) {
      ggvis.renderer = message.renderer || "canvas";
      ggvis.setRendererChooser(ggvis.renderer);
      ggvis.updateDownloadButtonText();
    }

    // Save the spec
    ggvis.specs[plotId] = spec;

    ggvis.parseSpec(spec, plotId);
  });

});
