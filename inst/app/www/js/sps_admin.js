

$(function(){
    $('body').on('admin-displayed', ()=>{
        setTimeout(function() {
            $(window).trigger("resize");
            $('a[href="#shiny-tab-admin-info"]').trigger('click');
        }, 1000);
    });
});
