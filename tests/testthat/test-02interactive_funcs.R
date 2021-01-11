# Functions cannot be checked by running examples
## Shiny test template
# testServer(function(id) {
#     moduleServer(
#         id,
#         function(input, output, session){
#
#         })
# }, {
#
# })
temp_dir <- tempdir()
systemPipeShiny::quiet(systemPipeShiny::spsInit(app_path = temp_dir,
                                                project_name = "test_funcs",
                                                open_files = FALSE,
                                                overwrite = TRUE,
                                                change_wd = TRUE))
app_path <- file.path(temp_dir, "test_funcs")

context("add/get data functions")

testServer(shinyApp(
    fluidPage(
        actionButton("add", "add"),
        actionButton("get", "get"),
        actionButton("wrong", "when it gets wrong")
    ),
    function(input, output, session) {
        shared <- reactiveValues()
        data <- 123
        observeEvent(input$add, {
            addData(data, shared, "core_about")
        })
        observeEvent(input$get, {
            get_return <- getData('core_about', shared)
        })
        observeEvent(input$wrong, {
            getData('not_there', shared)
        })
    }
), {
    session$setInputs(add = 1)
    expect_equal(shared$data$core_about, 123)
    expect_equal(getData('core_about', shared), 123)
    quiet(expect_error(getData('not_there', shared)))
})

context("SPS validate func")
testServer(shinyApp(
    fluidPage(
        actionButton("vd1", "validate1"),
        actionButton("vd2", "validate2")
    ),
    function(input, output, session){
        mydata <- datasets::iris
        shared <- reactiveValues()
        observeEvent(input$vd1, {
            spsValidate({
                is.data.frame(mydata)
            }, vd_name = "Is df")
            shared$vd1 <- "vd pass"
        })
        observeEvent(input$vd2, {
            quiet(spsValidate({
                nrow(mydata) > 200
            }, vd_name = "more than 200 rows"))
            shared$vd2 <- "vd blocked"
        })
    }),
    {
        session$setInputs(vd1 = 1)
        expect_equal(shared$vd1, "vd pass")
        session$setInputs(vd2 = 1)
        expect_null(shared$vd2)
    }
)

context("genGallery func")
expect_is(genGallery(c("plot_example1")), "shiny.tag")
expect_is(genGallery(type = "plot"), "shiny.tag")

context("genHrefTable func")
expect_is(genHrefTable(
    list(
        row1 = c("core_canvas", "core_about"),
        row2 =  "data")
    ),
    "shiny.tag"
)

context("genHrefTab func")
expect_is(genHrefTab(c("core_canvas", "core_about")), "shiny.tag")
expect_error(genHrefTab("random"))


context("spsHr func")
expect_is(spsHr(), "shiny.tag")


context("dynamicFile dynamicFileServer funcs")
test_that("dynamicFile() creates expected HTML in local/server mode", {
    spsOption("mode", "local")
    expect_true(
        str_detect(dynamicFile("getFile")  %>% as.character(),
                   "shinyFiles btn btn-btn btn-primary")
    )
    spsOption("mode", "server")
    expect_true(
        str_detect(dynamicFile("getFile")  %>% as.character(),
                   "progress active shiny-file-input-progress")
    )
})
# local server
spsOption("mode", "local")
testServer(function(id) {
    moduleServer(
        id,
        function(input, output, session){
            myfile <- dynamicFileServer(input, session, id = "getFile")
        })
}, {
    session$setInputs(getFile = list("test_file"))
    expect_is(myfile(), "data.frame")
})
# server mode server
spsOption("mode", "server")
testServer(function(id) {
    moduleServer(
        id,
        function(input, output, session){
            myfile <- dynamicFileServer(input, session, id = "getFile")
        })
}, {
    session$setInputs(getFile = "my_file_path")
    expect_equal(myfile(), "my_file_path")
})


context("loadDF func")
upload_df <- loadDF("upload")
eg_df <- loadDF("eg")
expect_false(emptyIsFalse(upload_df[[1]][1]))
expect_equal(upload_df, eg_df)
expect_is(upload_df, "tbl")
temp_file <- tempfile()
write.csv(iris, temp_file, row.names = FALSE, quote = FALSE)
loaded_df <- loadDF("upload", upload_path = temp_file, delim = ",")
expect_equal(loaded_df[[1]][1], 5.1)
loaded_df_eg <- loadDF("eg", upload_path = temp_file, delim = ",")
expect_equal(loaded_df_eg[[1]][1], "")


context("shinyCatch server behavior")
testServer(function(id) {
    moduleServer(
        id,
        function(input, output, session){
            shared <- reactiveValues()
            fn_warning <- function() {
                warning("this is a warning!")
                return(1)
            }
            observeEvent(input$btn1, {
                quiet(shinyCatch(stop("error with blocking"), blocking_level = "error"))
                shared$blocked <- "blocked"
            })
            observeEvent(input$btn2, {
                quiet(shinyCatch(stop("error without blocking")))
                shared$not_blocked <- "not_blocked"
            })
            observeEvent(input$btn3, {
                return_value <- quiet(shinyCatch(fn_warning()))
                shared$return_not_block <- return_value
            })
            observeEvent(input$btn4, {
                return_value <- quiet(shinyCatch(fn_warning(), blocking_level = "warning"))
                shared$return_block <- return_value
            })
            observeEvent(input$btn5, {
                shinyCatch(message("message"))
            })
        })
}, {
    session$setInputs(btn1 = 1)
    expect_null(shared$blocked)
    session$setInputs(btn2 = 1)
    expect_equal(shared$not_blocked, "not_blocked")
    session$setInputs(btn3 = 1)
    expect_equal(shared$return_not_block, 1)
    session$setInputs(btn4 = 1)
    expect_null(shared$return_block)
})

context("shinyCheckPkg server func")
testServer(function(id) {
    moduleServer(
        id,
        function(input, output, session){
            pkg_return <- reactiveValues()
            observeEvent(input$haha, {
                pkg_return$a <- suppressWarnings(shinyCheckPkg(
                    session, cran_pkg = c("pkg1", "pkg2"),
                    bioc_pkg = "haha", github = "sdasdd/asdsad"))
                expect_warning(shinyCheckPkg(session, cran_pkg = c("pkg1", "pkg2")))
            })
            observeEvent(input$hehe, {
                pkg_return$b <- shinyCheckPkg(
                    session, cran_pkg = c("base", "ggplot2"),
                    github = "sdasdd/base")
            })
        })
}, {
    session$setInputs(haha = 1)
    expect_false(pkg_return$a)
    session$setInputs(hehe = 1)
    expect_true(pkg_return$b)
})


context("clearableTextInput func")
expect_equal(
    openssl::md5(clearableTextInput("input1", "This is a input box") %>% as.character()) %>%
        as.character(),
    "393c72592b81850215ee9b355fd29e17"
)
# <div>
#     <label for="input1">This is a input box</label>
#     <span class="text-input-clearable" style="background-color: #f5f5f5;">
#       <input id="input1" type="text" value="" placeholder=""/>
#       <span id="clear_input" class="glyphicon glyphicon-remove"></span>
#     </span>
# </div>
# <script>clearText('input1')</script>

test_that("textInputGroup func", {
    local_edition(3)
    expect_snapshot_output(textInputGroup("id1", "id2"))
})

context("tabTitle func")
expect_equal(
    openssl::md5(tabTitle("This title") %>% as.character()) %>%
        as.character(),
    "374f1cd9ca21efa46b6ff2fa733a39c0"
)
# <h2 style="color:#17a2b8;">This title</h2>


context("gallery func")
expect_equal(
    openssl::md5(
        gallery(
            Id = "id", texts = "a",
            hrefs = "a.a", images = "a.jpg") %>%
            as.character()
    ) %>%
        as.character(),
    "80eb3c9741b53cc08187173c2f42e87d"
)
# <div id="id" class="col">
#     <p class="text-center h2" style="color: #0275d8;">Gallery</p>
#     <div class="row" style="  margin: 10px;">
#       <a href="a.a"  class="col-sm-4 sps-tab-link" style="right: 1px;">
#           <img src="a.jpg" class="img-gallery" height=300 width=400 style="width: 100%;">
#           <p class="text-center h4">a</p>
#       </a>
#     </div>
# </div>


context("hexLogo  hexPanel func")
expect_equal(
    openssl::md5(
        hexLogo("logo", hex_img = "log.jpg") %>%
            as.character()
    ) %>%
        as.character(),
    "925ff9cf33afac82a938737dc00fe832"
)
# <div id="logo" class="hex-container">
#  <svg class="hex-box" viewBox="0 0 100 115" version="1.1" xmlns="http://www.w3.org/2000/svg">
#   <defs>
#       <pattern id="logo-hex" patternUnits="userSpaceOnUse" height="100%" width="100%">
#         <image href="log.jpg" x="-10" y="-20" height="125%" width="125%" />
#       </pattern>
#   </defs>
#   <polygon points="50 1 95 25 95 75 50 99 5 75 5 25"fill="url(#logo-hex)" stroke="var(--primary)"stroke-width="2"/>
#  </svg>
# </div>
expect_equal(
    openssl::md5(
        hexPanel(id = "panel", title = "panel", hex_imgs = "panel.jpg") %>%
            as.character()
    ) %>%
        as.character(),
    "e5e558708e9c4b87c05b5bc9d55db5a9"
)
# <div class="row hex-panel">
#   <h5 class="text-primary">panel</h5>
#   <div class="hex-item"><div id="panel1" class="hex-container">
#     <svg class="hex-box" viewBox="0 0 100 115" version="1.1" xmlns="http://www.w3.org/2000/svg">
#       <defs>
#         <pattern id="panel1-hex" patternUnits="userSpaceOnUse" height="100%" width="100%">
#           <image href="panel.jpg" x="-10" y="-20" height="125%" width="125%" />
#         </pattern>
#       </defs>
#     <polygon points="50 1 95 25 95 75 50 99 5 75 5 25"fill="url(#panel1-hex)" stroke="var(--primary)"stroke-width="2"/>
#     </svg>
#   </div></div>
# </div>


context("hrefTab  func")
expect_equal(
    openssl::md5(
        hrefTab(
            label_text = c("a", "b"), Id = "a",
            hrefs = c("", "")) %>%
            as.character()
    ) %>%
        as.character(),
    "5c6e116d6ba386d6a562d93cc571dd4d"
)
# <div id="a" class="col">
#   <p class="h4" style="color: #0275d8; text-align: left;">A list of tabs</p>
#   <div><a href="" class="href-button
# sps-tab-link">a</a>
#  <a href="" class="href-button
# sps-tab-link">b</a>
# </div>
# </div>


context("hrefTab  func")
expect_equal(
    openssl::md5(
        hrefTable(
            Id = "table",
            item_titles = c("a", "b"),
            item_labels = list(c("a1"), c("b1", "b2")),
            item_hrefs = list(c(""), c("", "")),
        ) %>%
            as.character()
    ) %>%
        as.character(),
    "276ab1b4cd645dff3ed1b54c85ea59ea"
)
# <table id="table" class="table table-hover table-href table-striped">
#   <caption class="text-center h2" style="color: #0275d8;">A Table of list of tabs</caption>
#   <thead>
#                 <tr class="info">
#                   <th>Category</th>
#                   <th>Options</th>
#                 </tr>
#               </thead>
#   <tbody>  <tr>
#     <td class="h4" style="color: #0275d8;">a</td>
#     <td><a href="" class="href-button sps-tab-link">a1</a></td>
#   </tr>
#    <tr>
#     <td class="h4" style="color: #0275d8;">b</td>
#     <td><a href="" class="href-button sps-tab-link">b1</a><a href="" class="href-button sps-tab-link">b2</a></td>
#   </tr>
# </tbody>
# </table>


context("pgPaneUpdate pgPaneUI func")
expect_equal(
    openssl::md5(
        pgPaneUI(
            "thispg",
            c("this a", "this b"),
            c("a", "b"), "Example Progress") %>%
            as.character()
    ) %>%
        as.character(),
    "b881444bec979a9b9f84ca7b61db6f90"
)
# <div class="tab-pane" id="thispg-pg-container">
#   <div class="control-panel draggable" style="top:5%;right:2%;width:310px;height:auto;position:absolute;cursor:inherit; background-color: white; z-index:999;">
#     <div class="row">
#       <div class="col-sm-3"></div>
#       <div class="col-sm-7">
#         <h4>Example Progress</h4>
#       </div>
#       <div class="col-sm-2"><button class="action-button bttn bttn-simple bttn-xs bttn-primary bttn-no-outline"data-target="#thispg-pg-collapse" data-toggle="collapse"><i class="fa fa-minus"></i></button></div>
#     </div>
#     <div class="collapse" id="thispg-pg-collapse">
#       <ul class="timeline" id="thispg-timeline">
#         <li style="margin-bottom: 0;">
#           <i id="a-icon" class="fa fa-times bg-red"></i>
#           <div class="timeline-item">
#             <h3 class="timeline-header no-border">this a</h3>
#             <div class="timeline-body" style="padding: 0px;">
#               <div class="progress-group">
#                 <div class="progress">
#                   <div class="progress-bar progress-bar-primary progress-bar-striped" id="a-pg" role="progressbar"></div>
#                 </div>
#               </div>
#             </div>
#           </div>
#         </li>
#         <li style="margin-bottom: 0;">
#           <i id="b-icon" class="fa fa-times bg-red"></i>
#           <div class="timeline-item">
#             <h3 class="timeline-header no-border">this b</h3>
#             <div class="timeline-body" style="padding: 0px;">
#               <div class="progress-group">
#                 <div class="progress">
#                   <div class="progress-bar progress-bar-primary progress-bar-striped" id="b-pg" role="progressbar"></div>
#                 </div>
#               </div>
#             </div>
#           </div>
#         </li>
#         <li class="time-label">
#           <span class="bg-orange" id="thispg-pg-label">Ready</span>
#         </li>
#         <div style="margin-left: 60px; margin-right: 15px;">
#           <div class="progress-group">
#             <div class="progress">
#               <div class="progress-bar progress-bar-primary progress-bar-striped" id="thispg-pg-all" role="progressbar"></div>
#             </div>
#           </div>
#         </div>
#       </ul>
#     </div>
#   </div>
#   <script>$(".draggable").draggable();</script>
# </div>
testServer(function(id) {
    moduleServer(
        id,
        function(input, output, session){
        })
}, {
    quiet(expect_error(pgPaneUpdate("thispg", "a", 1000)))
    quiet(expect_error(pgPaneUpdate("thispg", "a", "a")))
    expect_null(pgPaneUpdate("thispg", "a", 50))
})


context("desc func")
expect_equal(
    openssl::md5(
        renderDesc(
            id = "desc",
            '
            ## title
            1. xxx
            2. xxx
            `aaa`
            '
        ) %>%
            as.character()
    ) %>%
        as.character(),
    "315d516bc28af52016c00421cd3e7cc8"
)
# <div class="desc">
#     <div class="collapse desc-body" id="desc" aria-expanded="false">
#      <h2>title</h2>
#
# <ol>
# <li>xxx</li>
# <li>xxx
# <code>aaa</code></li>
# </ol>
#
#     </div>
#
#     <a role="button" class="collapsed" data-toggle="collapse"
#        href="#desc" aria-expanded="false" aria-controls="desc">
#     </a>
# </div>



