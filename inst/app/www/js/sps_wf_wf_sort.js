// attach when new sortable table renders.
$(function(){
    var selStep, stepIndex;
    const sortable = Sortable.create($('#wf-wf-sortable')[0], {
        ghostClass: 'spr-steps-moving',
        group: "steps",
        animation: 150,
        handle: ".step-grid",
        //removeOnSpill: true,
        onEnd: function(evt){
            //console.log("sortable index", sortable.toArray())
            //console.log("stored index", stepIndex)
            if(sortable.toArray().join() === stepIndex.join()) return false;
            if(sortable.toArray().length !== stepIndex.length) return false;
            if(stepIndex.length === 0) stepIndex = ["0"];
            stepIndex = sortable.toArray();
            Shiny.setInputValue("wf-wf-step_orders", stepIndex);
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
            wfDelIndex += 1;
            //console.log("delindex", wfDelIndex)
            Shiny.setInputValue("wf-wf-step_order_del_trigger", wfDelIndex);
            Shiny.setInputValue("wf-wf-step_order_del", sortable.toArray());
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
