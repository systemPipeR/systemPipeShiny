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
spsUtil::quiet(systemPipeShiny::spsInit(app_path = temp_dir,
                                                project_name = "test_funcs",
                                                open_files = FALSE,
                                                overwrite = TRUE,
                                                change_wd = TRUE))
app_path <- file.path(temp_dir, "test_funcs")

## muterd before spsComps 0.2
# expect_s3_class(genGallery(type = "plot"), "shiny.tag")
#
# expect_s3_class(genHrefTable(
#     list(
#         row1 = c("core_canvas", "core_about"),
#         row2 =  "plot")
#     ),
#     "shiny.tag"
# )

############################# skip bioc and cran checks #######################
if(!interactive()) skip("Skip on checks")
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
        spsUtil::quiet(expect_error(pgPaneUpdate("thispg", "a", 1000)))
        spsUtil::quiet(expect_error(pgPaneUpdate("thispg", "a", "a")))
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



