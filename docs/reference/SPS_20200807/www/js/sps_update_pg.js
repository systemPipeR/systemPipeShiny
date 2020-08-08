Shiny.addCustomMessageHandler("sps-update-pg", function(data) {
    var pg_panel = $("#" + data.panel_id + "-pg-collapse");
    var pg_li = pg_panel.find(".progress-bar").slice(0, -1);
    var current_pg = [];
    pg_li.each(function(){
      current_pg.push(parseFloat(this.style.width.replace('%','')));
    });
    var current_pg_total = current_pg.filter(e => (e === 0 || e)).reduce((a, b) => a + b, 0);
    var pg_li_all = $("#" + data.panel_id + "-pg-all");
    var pg_li_all_percent = current_pg_total / pg_li.length;
    // update total pg
    pg_li_all.css({"width": pg_li_all_percent + "%"});
    // update other things
    var this_pg = pg_panel.find("#" + data.which_pg + "-icon");
    if(parseFloat(data.value) >= 100){
        this_pg.attr('class', 'fa fa-check bg-olive');
    } else {
        this_pg.attr('class', 'fa fa-times bg-red');
    }
    if(pg_li_all_percent >= 100){
        pg_panel.find('span').attr('class', 'bg-olive');
    } else {
        pg_panel.find('span').attr('class', 'bg-orange');
    }
});
