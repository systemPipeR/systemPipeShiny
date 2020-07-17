# use runApp() or click right top button in Rstudio to start app,
# but do not write this code in script, type it in console


# shiny and js
library(shiny)
library(shinydashboard) # main UI structure
library(shinydashboardPlus) # additional features on shinydashboard
library(pushbar) # drop down bars
library(rhandsontable) # handsontable js for table
library(magrittr) # pipe
library(shinyjs) # general js functions
library(shinyWidgets) # additional UI elements, alerts
library(shinyTree) # js tree veiw
library(networkD3) # network plots
library(plotly) # plotly plots
library(shinyAce) # Ace editor
library(shinytoastr) # toastr js
library(shinyjqui) # jQuery UI
library(shinyFiles) # local mode select input file
library(waiter) # loading process bar
# remotes::install_github("dreamRs/particlesjs") # particlesjs for loading theme
# install.packages("crayon")

# general processing
library(ggplot2)
library(stringr)
library(assertthat)
library(glue)
library(dplyr)
library(vroom)
library(rlang)
## for functions
library(DESeq2, quietly = TRUE)
library(edgeR, quietly = TRUE)
library(ape, warn.conflicts = FALSE)
library(glmpca)
library(RColorBrewer)
library(pheatmap)
library(limma)
library(Rtsne)
library(DOT)
library(rsvg)

# suggested to install, no need to library them:
# remotes::install_github("dreamRs/particlesjs") # particlesjs for loading theme
# install.packages("crayon") # colorful terminal message
# remotes::install_github("Rstudio/shinymeta") # particlesjs for loading theme

# SPS options
# mode: running mode - local, server
# warning_toast: toast popup message when you are under some dangerious options - TRUE, FALSE
# loading_screen: to show loading screen? - TRUE, FALSE
# loading_theme: loading screen themes, loading_screen must be TRUE - vhelix
# loading_particles: particle effects on loading, loading_screen must be TRUE - TRUE, FALSE
# use_crayon: Do you want colorful terminal messages? must install `crayon`- TRUE, FALSE
# verbose: display some info during processing? - TRUE, FALSE
# dev: developer mode, you are able to add tabs - TRUE, FALSE
# admin_url: admin_page query url - admin
options(sps = list(
    mode = "local",
    warning_toast = TRUE,
    loading_screen = F,
    loading_theme = "vhelix",
    loading_particles = TRUE,
    use_crayon = TRUE,
    verbose = FALSE,
    dev = TRUE,
    admin_page = FALSE,
    admin_url = "admin"
))
source("R/spsCore.R")
source("R/spsUlti.R")
source("R/spsClasses.R")
checkSps()

# other useful shiny options
# max upload size
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
# add  global containers
# Please do not change the container variable name
## to save plot snap shots
sps_plots <- plotContainer$new()
## for database and encryption functions
sps_enc <- spsEncryption$new()
# use `sps_enc$createDb()` to create a new database if there is no db

####### you can add additional code #########


