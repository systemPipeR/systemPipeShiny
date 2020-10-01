context("SPS files")
temp_dir <- tempdir()
systemPipeShiny::quiet(systemPipeShiny::spsInit(app_path = temp_dir,
                                                project_name = "test_spsinit",
                                                open_files = FALSE,
                                                overwrite = TRUE,
                                                change_wd = FALSE))
app_path <- file.path(temp_dir, "test_spsinit")
test_that("Test SPS setup function", {
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
