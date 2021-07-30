// attach when new sortable table renders.
$(function(){
    var sortable, selStep, stepIndex;
    sortable = Sortable.create($('#wf-wf-sortable')[0], {
        ghostClass: 'spr-steps-moving',
        group: "steps",
        animation: 150,
        handle: ".step-grid",
        //removeOnSpill: true,
        onEnd: function(evt){
            if(sortable.toArray().join() === stepIndex.join()) return false;
            stepIndex = sortable.toArray();
            Shiny.setInputValue("wf-wf-step_orders", stepIndex);
        }
    });
    stepIndex = sortable.toArray(); // get initial stepIndex
    Shiny.setInputValue("wf-wf-step_orders", stepIndex);

    Sortable.create($('#wf-wf-step_trash')[0], {
        group: "steps",
        onAdd: function(evt) {
            this.el.removeChild(evt.item);
            let span = $(this.el).children('span');
            span.css({transform: 'rotate(-45deg)'});
            setTimeout(()=>span.css({transform: 'rotate(0deg)'}), 250);
        }
    });

    $('#wf-wf-sortable').on('focus', '.step-grid', function(){
        selStep = this;
    });

    Shiny.addCustomMessageHandler("wf-warn-dep", function(data) {
        $('#wf-wf-sortable').children().css('background-color', function (index) {
            return data.colors[index % data.colors.length];
        });
    });
});
