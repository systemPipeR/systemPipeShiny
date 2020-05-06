# SPS main function

runSPS <- function(appDir = ".", ...) {
    assert_that(getOption('sps.runmode') %in% c("local", "deploy"),
                msg = "sps.runmode can only be local or deploy")


    runApp(appDir = appDir, ...)
}


switch ("ggg",
    "local" = "local",
    "app" = "app",
    "other"
)
