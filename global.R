# use runApp() or click right top button in Rstudio to start app,
# do not write this code in script, type it in console


# shiny and js
library(shiny)
library(shinydashboard) # main UI structure
library(shinydashboardPlus) # additional features on shinydashboard
library(pushbar) # drop down bars
library(rhandsontable) # handsontable js for table
library(magrittr) # pipe
library(shinyjs) # general js functions
library(dplyr)
library(ggplot2)
library(shinyWidgets) # additional UI elements, alerts
library(shinyTree) # js tree veiw
library(networkD3) # network plots
library(plotly) # plotly plots
library(shinyAce) # Ace editor
library(shinytoastr) # toastr js
library(shinyjqui) # jQuery UI
# general processing
library(ggplot2)
library(stringr)
library(assertthat)
library(glue)
library(dplyr)
library(vroom)
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

# SPS options
# appDir: app folder - path
# mode: running mode - local, server
# loading_screen: to show loading screen? - TRUE, FALSE
# loading_theme: loading screen themes, loading_screen must be TRUE - vhelix
options(sps = list(
    appDir = ".",
    mode = "server",
    loading_screen = TRUE,
    loading_theme = "vhelixs"
))
source("R/runSPS.R")
resolveOptions()
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



################ test

