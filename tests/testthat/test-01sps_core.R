temp_dir <- tempdir()
spsUtil::quiet(systemPipeShiny::spsInit(app_path = temp_dir,
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


test_that("SPS classes", {
    sps_enc <<- spsEncryption$new()
    expect_s3_class(sps_enc, "spsEncryption")
})

sps <- suppressWarnings(sps())
test_that("SPS main UI and server", {
    expect_length(sps, 2)
    expect_s3_class(sps$ui, "shiny.tag.list")
    expect_type(sps$server, "closure")
})

test_that("servers", {
        expect_true(exists("core_aboutServer"))
        expect_true(exists("core_welcomeServer"))
        expect_true(exists("core_canvasServer"))
        expect_true(exists("core_aboutServer"))
        expect_true(exists("wfServer"))
        expect_true(exists("vs_mainServer"))
})

test_that("SPS options", {
    expect_equal(
        normalizePath(app_path, winslash = "/"),
        normalizePath(spsOption("app_path"), winslash = "/"))
    expect_length(getOption("sps"), 22)
    expect_invisible(spsUtil::quiet(spsOptDefaults()))
})

# # change wd back
# setwd(old_wd)

