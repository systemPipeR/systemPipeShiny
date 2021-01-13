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

test_that("sps_shiny_funcs", {
    # add / get methods
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

    # validation methods
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

})

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


test_that("dynamic_file_server_under_modes", {
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
        expect_s3_class(myfile(), "data.frame")
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
})

expect_s3_class(genGallery(c("plot_example1")), "shiny.tag")
expect_s3_class(genGallery(type = "plot"), "shiny.tag")

expect_s3_class(genHrefTable(
    list(
        row1 = c("core_canvas", "core_about"),
        row2 =  "data")
    ),
    "shiny.tag"
)

test_that("loadDF func", {
    upload_df <- loadDF("upload")
    eg_df <- loadDF("eg")
    expect_false(emptyIsFalse(upload_df[[1]][1]))
    expect_equal(upload_df, eg_df)
    expect_s3_class(upload_df, "tbl")
    temp_file <- tempfile()
    write.csv(iris, temp_file, row.names = FALSE, quote = FALSE)
    loaded_df <- loadDF("upload", upload_path = temp_file, delim = ",")
    expect_equal(loaded_df[[1]][1], 5.1)
    loaded_df_eg <- loadDF("eg", upload_path = temp_file, delim = ",")
    expect_equal(loaded_df_eg[[1]][1], "")
})

test_that("shinyCatch & shinyCheckPkg", {
    # shinyCatch
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
    # shinyCheckPkg
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
})

############################# skip bioc and cran checks #######################
# if(!interactive()) skip("Skip on checks")
skip_on_bioc()
skip_on_cran()

expect_s3_class(genHrefTab(c("core_canvas", "core_about")), "shiny.tag")
expect_error(genHrefTab("random"))

test_that("hr", {
    expect_snapshot_output(spsHr())
})

test_that("textInputGroup", {
    expect_snapshot_output(textInputGroup("id1", "id2"))
})

test_that("clearableTextInput", {
    expect_snapshot_output(clearableTextInput("input1", "This is a input box"))
})

test_that("tabtile", {
    expect_snapshot_output(tabTitle("This title"))
})

test_that("gallery", {
    expect_snapshot_output(gallery(
        Id = "id", texts = "a",
        hrefs = "a.a", images = "a.jpg"
    ))
})

test_that("hexLogo", {
    expect_snapshot_output(hexLogo("logo", hex_img = "log.jpg"))
})

test_that("hexPanel", {
    expect_snapshot_output(hexPanel(id = "panel", title = "panel", hex_imgs = "panel.jpg"))
})

test_that("hrefTab", {
    expect_snapshot_output(hrefTab(
        label_text = c("a", "b"), Id = "a",
        hrefs = c("", "")
    ))
})

test_that("hrefTable", {
    expect_snapshot_output(hrefTable(
        Id = "table",
        item_titles = c("a", "b"),
        item_labels = list(c("a1"), c("b1", "b2")),
        item_hrefs = list(c(""), c("", "")),
    ))
})

test_that("pgPaneUI", {
    expect_snapshot_output(pgPaneUI(
        "thispg",
        c("this a", "this b"),
        c("a", "b"),
        "Example Progress"
    ))

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
})

test_that("renderDesc", {
    expect_snapshot_output(renderDesc(
        id = "desc",
        '
            ## title
            1. xxx
            2. xxx
            `aaa`
            '
    ))
})



