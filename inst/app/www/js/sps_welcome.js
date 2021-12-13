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
});

