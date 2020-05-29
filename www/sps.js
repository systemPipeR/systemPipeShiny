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
  $('.sps-tab-link').click(function(e) {
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


