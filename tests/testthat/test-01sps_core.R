context("test SPS Init func")
temp_dir <- tempdir()
systemPipeShiny::quiet(systemPipeShiny::spsInit(app_path = temp_dir,
                                                project_name = "test_sps",
                                                open_files = FALSE,
                                                overwrite = TRUE,
                                                change_wd = TRUE))
app_path <- file.path(temp_dir, "test_sps")
test_that("test SPS setup function", {
    expect_true(file.exists(file.path(app_path, "global.R")),
                "global.R not there")
    expect_true(file.exists(file.path(app_path, "ui.R")),
                "ui.R not there")
    expect_true(file.exists(file.path(app_path, "server.R")),
                "server.R not there")
    expect_true(file.exists(file.path(app_path, "config", "tabs.csv")),
                "tabs.csv not there")
    expect_true(file.exists(file.path(app_path, "config", "sps.db")),
                "sps.db not there")
})


context("test SPS main class init")
sps_plots <<- plotContainer$new()
sps_enc <<- spsEncryption$new()
test_that("SPS classes", {
    expect_is(sps_plots, "plot_container", "plot container class not created")
    expect_is(sps_enc, "spsencrypt", "Encryption class not created")
})

context("snapshots container methods")
# UI methods
expect_warning(expect_error(sps_plots$addUI(div(), tab_id = "plotui1")))
plot_ui_return <- sps_plots$addUI(div(id = "myplot"), "plot1")
expect_is(plot_ui_return, "shiny.tag")
expect_is(sps_plots$getUI("plot1"), "shiny.tag")
# server methods
plot_server_return <- sps_plots$addServer(renderPlot, tab_id = "plot1",  "abc")
expect_is(plot_server_return, "shiny.render.function")
expect_is(sps_plots$getServer("plot1"), "shiny.render.function")
# canvas notify methods
expect_null(sps_plots$notifySnap("plot1"))
expect_equal(sps_plots$notifySnap("plot1")[2], "1")
sps_plots$notifySnap("plot1", reset = TRUE)
expect_null(sps_plots$notifySnap("plot1"))


context("test SPS main start func")
sps <- sps()
test_that("SPS main UI and server", {
    expect_length(sps, 2)
    expect_is(sps$ui, "shiny.tag.list", "SPS UI not a tag list")
    expect_is(sps$server, "function", "SPS server not a function")
})

context("test server can be loaded")
shiny::testServer(shinyApp(sps$ui, sps$server), {
    expect_is(shared, "reactivevalues", "Wrong shared object at start")
    expect_true(exists("core_aboutServer"))
    expect_true(exists("core_dashboardServer"))
    expect_true(exists("core_canvasServer"))
    expect_true(exists("core_aboutServer"))
    expect_true(exists("wf_mainServer"))
    expect_true(exists("wf_targetServer"))
    expect_true(exists("wf_wfServer"))
    expect_true(exists("wf_configServer"))
    expect_true(exists("wf_runServer"))
    expect_true(exists("vs_mainServer"))
    expect_equal(admin_url(), "mocksearch")

    # expect custom tabs are there
    expect_true(exists("data_exampleServer"))
    expect_true(exists("plot_example1Server"))
    expect_true(exists("plot_example2Server"))
})

context("test SPS options")
test_that("SPS options", {
    expect_equal(normalizePath(app_path), spsOption("app_path"))
    expect_length(getOption("sps"), 9)
    expect_invisible(quiet(viewSpsDefaults()))
})

# # change wd back
# setwd(old_wd)

