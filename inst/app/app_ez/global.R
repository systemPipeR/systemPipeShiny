## use shiny::runApp() in console or click right top button
## '>Run App' in Rstudio to start app,
## but do not write `shiny::runApp()` in script, type it in console

time_start <- Sys.time()
library(systemPipeShiny)
library(magrittr) # load pipes
# load additional libraries that you want to use below


## SPS options
# read "https://systempipe.org/sps/adv_features/config/#app-options" for details
# title: dashboard and website title - any string
# title_logo: logo to display when dashboard is collapsed and on website tab - url of an image
# mode: running mode - "local", "server"
# warning_toast: toast pop-up message when you are under some dangerous options - TRUE, FALSE
# login_screen: to show login screen? - TRUE, FALSE
# login_theme: login screen themes, login_screen need be TRUE - "random"
# use_crayon: Do you want colorful terminal messages? TRUE, FALSE
# verbose: display some info during processing? - TRUE, FALSE
# admin_url: admin_page query url - "admin"
# note_url: User notification broadcast file url - http(s) address
# tab_welcome -- module_wf: whether to load the corresponding tab or module? - TRUE, FALSE
# traceback: for expressions wrapped inside `spsComps::shinyCatch`, show full traceback if error? TRUE, FALSE

options(sps = list(
    title = "systemPipeShiny",
    title_logo = "img/sps_small.png",
    mode = "local",
    warning_toast = FALSE,
    login_screen = TRUE,
    login_theme = "global",
    use_crayon = TRUE,
    verbose = FALSE,
    admin_page = FALSE,
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
    tabs = c("vs_example"),
    server_expr = {
        # add you own server functions below
        msg("Custom expression runs -- Hello World", "GREETING", "green")
    }
)
