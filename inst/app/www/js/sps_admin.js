

$(function(){
    // dash resize on ui rendering
    $('body').on('admin-displayed', ()=>{
      $(window).trigger("resize");
      $('#page_admin ul.sidebar-menu > li:first-of-type a').trigger('click')
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
