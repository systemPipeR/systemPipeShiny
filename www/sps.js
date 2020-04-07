// need to load  jQuery and bootstrap3
// default shiny has both, only library shiny is enough

// find tab hyperlink reference for tabpanel items 
function findTabHref(panel_id, value){
    var tab_href = $("#" + panel_id).find("li:contains(" + value + ")").find("[href]").attr("href");
    return tab_href;
}

// for shinydashboardPlus    
// allow the main title in a menuItem object also has the ability to jump a tab
// @param: span_text, string, which main title you want to enble jumping
// @param: link_tab, str, tab name of target, define in R{tabItem(tabName = "xxx", ...)}
function sidebarSpanJump(span_text, link_tab){
	$(".sidebar-menu").find("span:contains(" + span_text + ")")[0].onclick = 
  	function(){
    	$('[href="#shiny-tab-' + link_tab + '"]').tab('show');
    };
}


// Enable a tag to change tab by clicking text with links
$(document).ready(function() {
  $('a').click(function(e) {
    var link = $(this).attr('href');
    if (link.match('#')) {
      e.preventDefault();
      $('a[href="' + $(this).attr('href') + '"]').tab('show');
    }
  });
});

// Keep leftside menu subitems always expanded
$(document).ready(function() {
  $(".treeview-menu").css("display", "block");
});
