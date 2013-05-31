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
