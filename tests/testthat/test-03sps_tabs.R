temp_dir <- tempdir()
systemPipeShiny::quiet(systemPipeShiny::spsInit(app_path = temp_dir,
                                                project_name = "test_tabs",
                                                open_files = FALSE,
                                                overwrite = TRUE,
                                                change_wd = TRUE))
app_path <- file.path(temp_dir, "test_tabs")

context("admin page")
test_that("admin page test", {
    expect_is(systemPipeShiny:::adminUI(), "shiny.tag")
})


context("about tab")
test_that("about tab test", {
    expect_is(core_aboutUI("a"), "shiny.tag")
})


context("canvas tab")
test_that("canvas test", {
    expect_is(core_canvasUI("a"), "shiny.tag.list")
    expect_null(testServer(core_canvasServer, {}))
})


context("dashboard tab")
test_that("core_dashboard test", {
    expect_is(core_dashboardUI("a"), "shiny.tag.list")
    expect_null(testServer(core_dashboardServer, {}))
})


context("top tab")
test_that("core_top test", {
    expect_is(core_topUI("a"), "shiny.tag.list")
    expect_null(testServer(core_topServer, {}))
})


context("vs_main tab")
test_that("vs_main test", {
    expect_is(vs_mainUI("a"), "shiny.tag.list")
    expect_null(testServer(vs_mainServer, {}))
})


context("workflow cofig tab")
test_that("wf_config test", {
    expect_is(vs_mainUI("a"), "shiny.tag.list")
    expect_null(testServer(wf_configServer, {}))
})


context("workflow main tab")
test_that("wf_main test", {
    expect_is(wf_mainUI("a"), "shiny.tag.list")
    expect_null(testServer(wf_mainServer, {}))
})


context("workflow run tab")
test_that("wf_run test", {
    expect_is(wf_runUI("a"), "shiny.tag.list")
    expect_null(testServer(wf_runServer, {}))
})


context("workflow targets tab")
test_that("wf_target test", {
    expect_is(wf_targetUI("a"), "shiny.tag.list")
    expect_null(testServer(wf_targetServer, {}))
})


context("workflow workflow tab")
test_that("wf_wf test", {
    expect_is(wf_wfUI("a"), "shiny.tag.list")
    expect_null(testServer(wf_wfServer, {}))
})




