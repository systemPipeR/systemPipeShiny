// need to load  jQuery and bootstrap3
// default shiny has both, only library shiny is enough

// find tab hyperlink reference for tabpanel items
findTabHref = (panel_id, value) =>{
    var tab_href = $("#" + panel_id)
      .find("li:contains(" + value + ")")
      .find("[href]").attr("href");
    return tab_href;
};

// for shinydashboardPlus
// allow the main title in a menuItem object also has the ability to jump a tab
// @param: span_text, string, which main title you want to enble jumping
// @param: link_tab, str, tab name of target, define in R{tabItem(tabName = "xxx", ...)}
sidebarSpanJump = (span_text, link_tab) =>{
	$(".sidebar-menu").find("span:contains(" + span_text + ")")[0].onclick =
  	function(){
    	$('[href="#shiny-tab-' + link_tab + '"]').tab('show');
        $("#wf-panel").addClass("shinyjs-hide");
    };
};

$(document).ready(() =>{
    sidebarSpanJump('Workflow Mangement', 'wf_main');
    sidebarSpanJump('Visualization', 'vs_main');
});

// Enable a tag to change tab by clicking text with links
$(document).ready(function(){
  $('.tab-content').on('click', '.sps-tab-link', (e) =>{
    var link = $(this).attr('href');
    if (link.match('#')) {
      e.preventDefault();
      $('a[href="' + $(this).attr('href') + '"]').tab('show');
    }
  });
});

// Keep leftside menu subitems always expanded
$(document).ready(() =>{
  $(".treeview-menu").css("display", "block");
});


// js for clearableTextInput
clearText = (clear_input_id) =>{
  var textInput = document.getElementById(clear_input_id);
  var clearBtn = textInput.nextElementSibling;
  var textInputBox = textInput.parentElement;
  textInput.onkeyup = () =>{
    clearBtn.style.visibility = (this.value.length) ? "visible" : "hidden";
  };
  clearBtn.onclick = () =>{
    this.style.visibility = "hidden";
    textInput.value = "";
    Shiny.setInputValue(clear_input_id, "");
  };
  textInput.addEventListener("focus", () =>{
    textInputBox.classList.add("text-input-focused");
  }, false);
  textInput.addEventListener("blur", () =>{
    textInputBox.classList.remove("text-input-focused");
  }, false);
};


// change fileInput color to bs primary
// change text bar in file input local mode to read only
$(document).ready(() =>{
  $('.btn-file').removeClass('btn-default').addClass('btn-primary');
  $(".sps-file input").attr("readonly", true);
});


// change canvas size based on plot distance to bottom
stretchCanvas = () =>{
    $(document).on("resize", ".sps-canvas", ()=>{
        var canvas_plots = $(".sps-plot-canvas");
        var container = canvas_plots.parents(".sps-canvas");
        var doc_height = $(document).height();
        var div_dis = canvas_plots.map(function(){
            return  doc_height - $(this).offset().top - $(this).height();
            });
        var min_dis = Math.min(...div_dis);
        var h = 0;
        if(min_dis < window.innerHeight/4) {
            h = container.height() + window.innerHeight/2;
            container.height(h);
        }
        if(min_dis > window.innerHeight/2) {
            h = container.height() - window.innerHeight/4;
            container.height(h);
        }
    });
};

// change plot tab size based on distance to bottom
stretchPlotTab = (id) =>{
    $("#" + id).parents(".sps-plot-container").resize(() => {
      var stretch_elem = $("#" + id);
      var container = stretch_elem.parents(".tab-pane");
      var div_dis = $(document).height() - stretch_elem.offset().top - stretch_elem.height();
      var h = 0;
      if(div_dis < window.innerHeight/4) {
          h = container.height() + window.innerHeight/2;
          container.height(h);
      }
      if(div_dis > window.innerHeight/2) {
          h = container.height() - window.innerHeight/4;
          container.height(h);
      }
    });
};
