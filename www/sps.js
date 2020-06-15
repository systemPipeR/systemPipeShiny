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
        $("#wf-panel").addClass("shinyjs-hide");
    };
}

$(document).ready(function(){
    sidebarSpanJump('Workflow Mangement', 'wf_main');
    sidebarSpanJump('Visualization', 'vs_main');
});

// Enable a tag to change tab by clicking text with links
$(document).ready(function() {
  $('.tab-content').on('click', '.sps-tab-link', function(e) {
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


// js for clearableTextInput
function clearText(clear_input_id) {
  var textInput = document.getElementById(clear_input_id);
  var clearBtn = textInput.nextElementSibling;
  var textInputBox = textInput.parentElement;
  textInput.onkeyup = function() {
    clearBtn.style.visibility = (this.value.length) ? "visible" : "hidden";
  };
  clearBtn.onclick = function() {
    this.style.visibility = "hidden";
    textInput.value = "";
    Shiny.setInputValue(clear_input_id, "");
  };
  textInput.addEventListener("focus", function() {
    textInputBox.classList.add("text-input-focused");
  }, false);
  textInput.addEventListener("blur", function() {
    textInputBox.classList.remove("text-input-focused");
  }, false);
}


// change fileInput color to bs primary
// change text bar in file input local mode to read only
$(document).ready(function() {
  $('.btn-file').removeClass('btn-default').addClass('btn-primary');
  $(".sps-file input").attr("readonly", true);
});


// change plot document size based on distance to bottom
function stretchSpsPlot() {
    var stretch_elem = $(".sps-plot-container");
    var parent_pane = stretch_elem.parents(".tab-pane");
    var div_dis = $(document).height() - stretch_elem.offset().top - stretch_elem.height();
    var h = 0;
    if(div_dis < window.innerHeight/4) {
        h = parent_pane.height() + window.innerHeight/2;
        parent_pane.height(h);
    }
    if(div_dis > window.innerHeight/2) {
        h = parent_pane.height() - window.innerHeight/4;
        parent_pane.height(h);
    }
}

$(document).resize(stretchSpsPlot);
