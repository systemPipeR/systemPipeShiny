/*Welcome page js */

$(function(){
  (async ()=> {
    await new Promise((resolve, reject) => {
      $("#welcome-svg").load("img/Untitled.svg", resolve)
    });
    var svg = $("#welcome-svg > svg");
    svg.removeAttr("height").removeAttr("width");
    //$("#sps_header_logo").fadeIn(2000);
    //hoverLogo(document.querySelector("#sps_header_logo"));
    VanillaTilt.init(document.querySelector("#sps_header_logo > g"), {
      "full-page-listening": true,
      glare: true,
      max: 45
    });
  })();


  $('#core_welcome-go_down').click(()=> {
    document.documentElement.scrollTo({
      top: window.innerHeight,
      behavior: "smooth"
    });
  })

  $('.welcome-header .card').map(function(){
    var status = $(this).attr('data-status');
    if (status === "disabled") {
      $(this).addClass("disabled").attr('onclick', '');
      this.vanillaTilt.destroy();
    }
    if (status === "missing") {$(this).addClass("missing");}
    $(this).mouseover(function(){
      var mod = $(this).attr('data-desc');
      var modDesc = $(`.mod-desc h5[data-desc="${mod}"]`);
      if(modDesc.css("display") === "none") {
        $(`.mod-desc h5[data-desc]`).fadeOut();
        modDesc.fadeIn();
      }
    });
  });

});

