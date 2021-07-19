// attach when new sortable table renders.
var sortable, selStep, stepIndex;
$(function(){
    sortable = Sortable.create($('#sortable')[0], {
        ghostClass: 'spr-steps-moving',
        group: "steps",
        animation: 150,
        handle: ".step-grid",
        //removeOnSpill: true,
        onEnd: function(evt){
            stepIndex = sortable.toArray();
            Shiny.setInputValue("wf-wf-step_orders", stepIndex)
        }
    });

    Sortable.create($('#step_trash')[0], {
        group: "steps",
        onAdd: function(evt) {
            this.el.removeChild(evt.item);
            let span = $(this.el).children('span');
            span.css({transform: 'rotate(-45deg)'});
            setTimeout(()=>span.css({transform: 'rotate(0deg)'}), 250);
        }
    });

    $('#sortable').on('focus', '.step-grid', function(){
        selStep = this;
    });
});
