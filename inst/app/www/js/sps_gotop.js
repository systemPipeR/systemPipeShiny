function goTop() {
  document.documentElement.scrollTo({
    top: 0,
    behavior: "smooth"
  });
}

function handleScroll() {
  var gotop_btn = $("#gotop");
  if (document.body.scrollTop > 50 || document.documentElement.scrollTop > 50){
    gotop_btn.slideDown();
  } else {
    gotop_btn.slideUp();
  }
}

$(function() {
    document.addEventListener("scroll", handleScroll);
});
