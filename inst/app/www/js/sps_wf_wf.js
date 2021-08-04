// general wf step module js
var stepIndex;
function enlargeBox(target) {
    $('body').on('click', target, function(e){
    let enlarged = $(this).attr('enlarged');
    $($(this).attr('enlarge_target')).toggleClass('enlarge-full-screen');
    if (enlarged === 'false') {
        $(this).attr('enlarged', 'true').addClass('fa-compress-arrows-alt').removeClass('fa-expand-arrows-alt');
    } else {
        $(this).attr('enlarged', 'false').addClass('fa-expand-arrows-alt').removeClass('fa-compress-arrows-alt');
    }
});
}

enlargeBox('#wf-wf-step_enlarge');
enlargeBox('#wf-wf-wf_plot_enlarge');

$(function(){
    $(document).on('wf_plot_created', function(){
        $('#wf-wf-wf_plot, #wf-wf-wf_plot svg').css({overflow: '', height: '97%', width: '100%'});
    });

    Shiny.addCustomMessageHandler("wf-undo-redo", function(data){
        $('.wf-history-panel tr td:nth-child(2)').text(function(x) {
            return data.msg[x % data.msg.length];
        });
        clearPopTip();
    });
});

function clearPopTip(){
    $('.popover[role="tooltip"], .tooltip[role="tooltip"]').remove();
}
