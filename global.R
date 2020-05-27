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
library(waiter)
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
# loading_screen: to show loading screen? - TRUE, FALSE
# loading_theme: loading screen themes, loading_screen must be TRUE - vhelix
# loading_particles: particle effects on loading, loading_screen must be TRUE - TRUE, FALSE
# use_crayon: Do you want colorful terminal messages? must install `crayon`- TRUE, FALSE
# verbose: display some info during processing? - TRUE, FALSE
options(sps = list(
    mode = "local",
    loading_screen = FALSE,
    loading_theme = "vhelix",
    loading_particles = TRUE,
    use_crayon = TRUE,
    verbose = TRUE
))

source("R/runSPS.R")
source("R/spsSupport.R")
resolveOptions(appDir = getwd())

# other useful shiny options
# max upload size, default 5MB
options(shiny.maxRequestSize = 5*1024^2)

# for debugging
# options(shiny.reactlog = TRUE)
# options(shiny.autoload.r = FALSE)
# options(shiny.trace = TRUE)
# options(shiny.fullstacktrace = TRUE)
# options(shiny.error = browser)
# options(shiny.autoreload.r = TRUE)
options(shiny.autoreload = TRUE) # takes some computer power, you may consider turn it off

# load tab info
tab_info <- suppressMessages(vroom::vroom("config/tabs.csv", comment = "#", na = character()))

####### you can add additional code #########

