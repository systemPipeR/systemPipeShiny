## use shiny::runApp() in console or click right top button
## '>Run App' in Rstudio to start app,
## but do not write `shiny::runApp()` in script, type it in console

time_start <- Sys.time()
library(systemPipeShiny)
library(magrittr) # load pipes
# load additional libraries that you want to use here


## SPS options
# mode: running mode - "local", "server"
# warning_toast: toast pop-up message when you are under some dangerous options - TRUE, FALSE
# loading_screen: to show loading screen? - TRUE, FALSE
# loading_theme: loading screen themes, loading_screen need be TRUE - "vhelix"
# use_crayon: Do you want colorful terminal messages? TRUE, FALSE
# verbose: display some info during processing? - TRUE, FALSE
# admin_url: admin_page query url - "admin"
## to load some default modules:
# module_wf: load the workflow module? - TRUE, FALSE
# module_rnaseq: load the RNA-Seq module? - TRUE, FALSE
# module_ggplot: load the Quick ggplot module? - TRUE, FALSE
options(sps = list(
    mode = "server",
    warning_toast = FALSE,
    loading_screen = TRUE,
    loading_theme = "vhelix",
    use_crayon = TRUE,
    verbose = FALSE,
    admin_page = FALSE,
    admin_url = "admin",
    module_wf = TRUE,
    module_rnaseq = TRUE,
    module_ggplot = TRUE
))


## other useful shiny options
## max upload size, 30Mb here
options(shiny.maxRequestSize = 30*1e6)

## for debugging
# options(shiny.reactlog = TRUE)
# options(shiny.trace = TRUE)
# options(shiny.fullstacktrace = TRUE)
# options(shiny.error = browser)
# options(shiny.autoreload.r = TRUE)
# options(shiny.autoreload = FALSE) # takes some computer power, you may consider turn it off

## load tab info
tab_info <- suppressMessages(vroom::vroom("config/tabs.csv", comment = "#", na = character()))

####### Main App Function Starts #########

sps_app <- sps(
    tabs = c("vs_example"),
    server_expr = {
        msg("Custom expression runs -- Hello World", "GREETING", "green")
    }
)

