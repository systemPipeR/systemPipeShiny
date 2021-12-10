## use shiny::runApp() in console or click the top-right button '>Run App'
## in Rstudio to start app, but do not write `shiny::runApp()` in this script, type it in console

time_start <- Sys.time()
library(systemPipeShiny)
library(magrittr) # load pipes
# load additional libraries that you want to use below


## SPS options to configure the app
# read "https://systempipe.org/sps/adv_features/config/#app-options" for details
spsOption(.list = list(
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
    traceback = FALSE,
    is_demo = FALSE,
    welcome_guide = TRUE
))

## An alternative is to comment above and use `spsOption` to overwrite **single** options, eg:
# spsOption("mode", "server")

## use `spsOptions` to check current settings
# spsOptions()


## other useful shiny options
## max upload size, 30Mb here
options(shiny.maxRequestSize = 30*1e6)

## for debugging
# options(shiny.reactlog = TRUE)
# options(shiny.trace = TRUE)
# options(shiny.fullstacktrace = TRUE)
# options(shiny.error = browser)
# options(shiny.autoreload = FALSE) # takes some computer power, you may consider turn it off

##  account information
## PLEASE use following to add your own accounts and remove the default accounts for deployment
# mydb <- spsAccount$new()
# mydb$accList()
# mydb$accAdd(acc_name = "XXX", acc_pass = "$xxxx", role = "admin")
# mydb$accRemove("admin")
# mydb$accRemove("user")

####### SPS Main App Function Starts #########

sps_app <- sps(
    # once you have create more custom tabs, add IDs of these tabs in this vector
    # to confirm you want to load them.
    tabs = c(
        "vs_example"
    ),
    server_expr = {
        # add you own additional server expressions below, for example:
        msg("Custom expression runs -- Hello World", "GREETING", "green")
    }
)
