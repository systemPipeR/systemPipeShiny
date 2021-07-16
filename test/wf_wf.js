var sortable;
$(function(){
    console.log(Sortable);
    sortable = Sortable.create($('#sortable')[0], {
	    ghostClass: 'spr-steps-moving',
	    animation: 150,
	    handle: ".step-grid"
    });
});


$('body').on('click', '#step_enlarge', function(e){
    let enlarged = $(this).attr('enlarged');
    $($(this).attr('enlarge_target')).toggleClass('enlarge-full-screen');
    if (enlarged === 'false') {
        $(this).attr('enlarged', 'true').addClass('fa-compress-arrows-alt').removeClass('fa-expand-arrows-alt');
    } else {
        $(this).attr('enlarged', 'false').addClass('fa-expand-arrows-alt').removeClass('fa-compress-arrows-alt');
    }
});
