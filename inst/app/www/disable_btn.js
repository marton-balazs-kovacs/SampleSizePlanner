$( document ).ready(function() {
    Shiny.addCustomMessageHandler('disable_btn', function(what) {
    $(what).attr('title', 'Please upload a valid infosheet');
    $(what).attr('disabled', 'disabled');
  });
  
  Shiny.addCustomMessageHandler('reable_btn', function(what) {
    $(what).removeAttr('title');
    $(what).removeAttr('disabled');
  });
});
