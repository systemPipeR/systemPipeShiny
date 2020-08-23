# use shiny::runApp() in console or click right top button in Rstudio to start app,
# but do not write this code in script, type it in console

time_start <- Sys.time()
library(systemPipeShiny)


# suggested to install, no need to library them:
# to unlock some tabs, visualizations and more
# BiocManager::install("dreamRs/particlesjs") # particlesjs for loading theme
# BiocManager::install("systemPipeR")
# BiocManager::install("DESeq2")
# BiocManager::install("edgeR")
# BiocManager::install("ape")
# BiocManager::install("glmpca")
# BiocManager::install("RColorBrewer")
# BiocManager::install("pheatmap")
# BiocManager::install("limma")
# BiocManager::install("Rtsne")

# SPS options
# mode: running mode - local, server
# warning_toast: toast popup message when you are under some dangerous options - TRUE, FALSE
# loading_screen: to show loading screen? - TRUE, FALSE
# loading_theme: loading screen themes, loading_screen must be TRUE - vhelix
# use_crayon: Do you want colorful terminal messages? must install `crayon`- TRUE, FALSE
# verbose: display some info during processing? - TRUE, FALSE
# dev: developer mode, you are able to add tabs - TRUE, FALSE
# admin_url: admin_page query url - admin
options(sps = list(
    mode = "local",
    warning_toast = TRUE,
    loading_screen = TRUE,
    loading_theme = "vhelix",
    use_crayon = TRUE,
    verbose = TRUE,
    dev = TRUE,
    admin_page = FALSE,
    admin_url = "admin"
))


# other useful shiny options
# max upload size, 30Mb here
options(shiny.maxRequestSize = 30*1e6)

# for debugging
# options(shiny.reactlog = TRUE)
# options(shiny.trace = TRUE)
# options(shiny.fullstacktrace = TRUE)
# options(shiny.error = browser)
# options(shiny.autoreload.r = TRUE)
# options(shiny.autoreload = FALSE) # takes some computer power, you may consider turn it off

# load tab info
tab_info <- suppressMessages(vroom::vroom("config/tabs.csv", comment = "#", na = character()))
# use `sps_enc$createDb()` to create a new database if there is no db
## to save plot snap shots
sps_plots <- plotContainer$new()
## for database and encryption functions
sps_enc <- spsEncryption$new()

####### Main App Function Starts #########

sps_app <- sps(
    vstabs = "",
    server_expr = {
        msg("Custom expression runs -- Hello World", "GREETING", "green")
    }
)

