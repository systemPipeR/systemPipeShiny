// attach when new sortable table renders.
$(function(){
    var selStep;
    const sortable = Sortable.create($('#wf-wf-sortable')[0], {
        ghostClass: 'spr-steps-moving',
        group: "steps",
        animation: 150,
        handle: ".step-grid",
        //removeOnSpill: true,
        onEnd: function(evt){
            if(sortable.toArray().join() === stepIndex.join()) return false;
            let delMsg =  sortable.toArray().length < stepIndex.length ? "del" : "";
            stepIndex = sortable.toArray();
            if(stepIndex.length === 0) stepIndex = ["0"];
            Shiny.setInputValue("wf-wf-step_orders", stepIndex);
            console.log(stepIndex)
            Shiny.setInputValue("wf-wf-step_order_del", delMsg);
        }
    });

    if(stepIndex === undefined) {
        stepIndex = sortable.toArray(); // get initial stepIndex
        Shiny.setInputValue("wf-wf-step_orders", stepIndex);
    }

    const delStack = Sortable.create($('#wf-wf-step_trash')[0], {
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
    // prevent event listensers from fileInput
    $('#wf-wf-sortable')[0].addEventListener('dragleave', e => e.stopPropagation());
});
