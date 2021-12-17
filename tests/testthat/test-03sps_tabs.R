temp_dir <- tempdir()
spsUtil::quiet(systemPipeShiny::spsInit(app_path = temp_dir,
                                                project_name = "test_tabs",
                                                open_files = FALSE,
                                                overwrite = TRUE,
                                                change_wd = TRUE))
app_path <- file.path(temp_dir, "test_tabs")

test_that("admin page test", {
    expect_s3_class(systemPipeShiny:::adminUI(), "shiny.tag.list")
})

test_that("about tab test", {
    expect_s3_class(core_aboutUI("a"), "shiny.tag")
})

test_that("canvas test", {
    expect_s3_class(core_canvasUI("a"), "shiny.tag.list")
    expect_null(testServer(core_canvasServer, {}))
})

test_that("core_dashboard test", {
    missing_mod <- list("wf" = "", "rna" = "", "ggplot" ="", "canvas" = "")
    expect_s3_class(core_welcomeUI("a", missing_mod), "shiny.tag")
    expect_null(testServer(core_welcomeServer, {}))
})

test_that("core_top test", {
    expect_s3_class(core_topUI("a"), "shiny.tag")
    expect_null(testServer(core_topServer, {}))
})

test_that("vs_main test", {
    expect_s3_class(vs_mainUI("a"), "shiny.tag.list")
    expect_null(testServer(vs_mainServer, {}))
})

test_that("wf_run test", {
    expect_s3_class(wf_runUI("a"), "shiny.tag.list")
    expect_null(testServer(wf_runServer, {}))
})

test_that("wf_target test", {
    expect_s3_class(wf_targetUI("a"), "shiny.tag.list")
    expect_null(testServer(wf_targetServer, {}))
})

test_that("wf_wf test", {
    expect_s3_class(wf_wfUI("a"), "shiny.tag.list")
    expect_null(testServer(wf_wfServer, {}))
})




