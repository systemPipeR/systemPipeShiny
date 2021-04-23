.onLoad <- function(libname, pkgname) {
    #setup options
    options(sps = list(
        title = "systemPipeShiny",
        title_logo = "img/sps_small.png",
        mode = "local",
        warning_toast = FALSE,
        login_screen = FALSE,
        login_theme = "random",
        use_crayon = TRUE,
        verbose = FALSE,
        admin_page = TRUE,
        admin_url = "admin",
        note_url = 'https://raw.githubusercontent.com/systemPipeR/systemPipeShiny/master/inst/remote_resource/notifications.yaml',
        tab_welcome = TRUE,
        tab_vs_main = TRUE,
        tab_canvas = TRUE,
        tab_about = TRUE,
        module_wf = TRUE,
        module_rnaseq = TRUE,
        module_ggplot = TRUE,
        traceback = FALSE
    ))
    invisible()
}
