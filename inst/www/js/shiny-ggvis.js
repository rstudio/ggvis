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


  var ggv = window.ggv = {
    pendingData: {},  // data objects that have been received but not yet used
    plots: {},        // all vega chart objects on the page
    specs: {},        // all specs
    renderer: null
  };

  Shiny.addCustomMessageHandler("ggvis_data", function(message) {
    var plotId = message.plotId;
    var name = message.name;
    var value = message.value[0].values;

    if (ggv.plots[plotId]) {
      // If the plot exists already, feed it the data
      var dataset = {};
      dataset[name] = value;
      ggv.plots[plotId].data(dataset);
      ggv.plots[plotId].update();

      ggv.updateGgvisDivSize(plotId);
    } else {
      // The plot doesn't exist, save the data for when the plot arrives
      if (!ggv.pendingData[plotId])
        ggv.pendingData[plotId] = {};

      ggv.pendingData[plotId][name] = value;
    }
  });


  // Receive a vega spec and parse it
  Shiny.addCustomMessageHandler("ggvis_vega_spec", function(message) {
    var plotId = message.plotId;
    var spec = message.spec;

    // If no renderer already selected, set it here
    if (!ggv.renderer) {
      ggv.renderer = message.renderer || "canvas";
      ggv.setRendererChooser(ggv.renderer);
    }

    // Save the spec
    ggv.specs[plotId] = spec;

    vg.parse.spec(spec, function(chart) {
      var selector = ".ggvis-output#" + plotId;
      var $el = $(selector);

      chart = chart({ el: selector, renderer: ggv.renderer });
      // Save the chart object
      ggv.plots[plotId] = chart;
      $el.data("ggvis-chart", chart);

      // If the data arrived earlier, use it.
      if (ggv.pendingData[plotId]) {
        chart.data(ggv.pendingData[plotId]);
        delete ggv.pendingData[plotId];
      }

      chart.update();
      ggv.updateGgvisDivSize(plotId);

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
  ggv.updateGgvisDivSize = function(plotId) {
    var $el = $(".ggvis-output#" + plotId);
    var $plotarea = $el.find("div.vega > .marks");

    $el.width($plotarea.width());
    $el.height($plotarea.height());
  };

  // Given the name of a plot and an <a> element, set the href of that element
  // to the canvas content of the plot converted to PNG. This will set the href
  // when the link is clicked; the download happens when it is released.
  ggv.setGgvisDownloadHref = function(plotId, el) {
    var plot = $("#" + plotId + ".ggvis-output .marks")[0];
    var imageUrl;

    if (ggv.renderer === "svg") {
      // Extract the svg code and add needed xmlns attribute
      var svg = $(plot).clone().attr("xmlns", "http://www.w3.org/2000/svg");
      // Convert to string
      svg = $('<div>').append(svg).html();
      imageUrl = "data:image/octet-stream;base64,\n" + btoa(svg);

    } else if (ggv.renderer === "canvas") {
      imageUrl = plot.toDataURL("image/png").replace("image/png", "image/octet-stream");
    }

    // Set download filename and data URL
    el.setAttribute("download", plotId + "." + ggv.renderer);
    el.setAttribute("href", imageUrl);
  };

  // Change the renderer and update all plots
  ggv.setRenderer = function(renderer) {
    ggv.renderer = renderer;

    for (var plotId in ggv.specs) {
      if (ggv.specs.hasOwnProperty(plotId))
        ggv.plots[plotId].renderer(renderer).update();
    }
  };

  // Set the value of the renderer selector, if present
  ggv.setRendererChooser = function(renderer) {
    var $el = $("#ggvis_renderer");
    if ($el) {
      $el.val(renderer);
    }
  };

  // Attach event handlers to buttons
  $("button#quit").on("click", function() { window.close(); });

  $("a#ggvis_download").on("click", function() {
    var plotId = $(this).data("plot-id");
    ggv.setGgvisDownloadHref(plotId, this);
  });

  $("#ggvis_renderer").on("change", function() {
    ggv.setRenderer(this.value);
  });

});
