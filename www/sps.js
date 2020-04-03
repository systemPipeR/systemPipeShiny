// need to load and jQuery, bootstrap

// find tab hyperlink reference for tabpanel items 
function findTabHref(panel_id, value){
    var tab_href = $("#" + panel_id).find("li:contains(" + value + ")").find("[href]").attr("href");
    return tab_href;
}
