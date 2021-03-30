

$(function(){
    $('body').on('admin-displayed', ()=>{
        var checkDash;
        checkDash = setInterval(function() {
            if ($('script[src*="shinydashboardPlus"]').length > 0) {
              $(window).trigger("resize");
              $('a[href="#shiny-tab-admin-info"]').trigger('click');
              clearInterval(checkDash);
            }
        }, 100);
    });
});

$(document).on('shiny:value', function(e) {
    if (e.name === "page_admin") {
        setTimeout(function() {
            Shiny.setInputValue("adminUI_loaded", true, {priority: "event"});
        }, 1000);
    }
})

//watch enter key
$(function(){
    document.querySelector('#admin-login .login-box').addEventListener('keypress', function (e) {
        if (e.key === 'Enter') {
          $('#admin-login_click').trigger('click');
        }
    });
})
