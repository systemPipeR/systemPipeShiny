$(document).ready(function(){
    var li_find = false;
    var setup_box_img_src = ""
    $("#wf-wf_setup-choose_wf").parent().on('DOMNodeInserted', function(){
      console.log(1);
      if($(this).find('div').length > 0 && li_find == false){
        console.log(2);
        li_find = true;
        $(this).find('div')
        .on('mouseenter', 'li', function(){
          var current_li = $(this).find('span').text();
          var img_src;
          switch(current_li) {
            case 'RNAseq':
              img_src = 'sps/img/rnaseq.png';
              break;
            case 'Varseq':
              img_src = 'sps/img/varseq.png';
              break;
            case 'Riboseq':
              img_src = 'sps/img/riboseq.png';
              break;
            case 'Chipseq':
              img_src = 'sps/img/chipseq.png';
              break;
            default:
              img_src = 'sps/img/spr.png';
         }
         if(img_src != setup_box_img_src){
           $("#setup-box-img").fadeOut('fast', function () {
             $(this).attr('src', img_src);
             $(this).fadeIn('fast');
           });
           setup_box_img_src =img_src;
         }
        });
      }
    });
});
