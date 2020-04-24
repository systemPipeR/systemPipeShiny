# shiny and shiny visual

library(shiny)
library(shinydashboard) # main UI structure
library(shinydashboardPlus) # additional features on shinydashboard
library(shinyWidgets) # UI elements 
library(pushbar) # drop down bars
library(rhandsontable) # handsontable js for table
library(magrittr) # pipe
library(shinyjs) # general js functions
library(shinyWidgets) # additional UI elements, alerts
library(shinyTree) # js tree veiw
library(networkD3) # network plots
library(plotly) # plotly plots
library(shinyAce) # Ace editor
# source("https://install-github.me/mangothecat/shinytoastr")
library(shinytoastr) # need to install use the script above
library(shinyjqui) # jQuery UI
## for functions 
library(DESeq2, quietly = TRUE)
library(edgeR, quietly = TRUE)
library(ape, warn.conflicts = FALSE)
library(glmpca)
library(RColorBrewer)
library(pheatmap)
library(limma)
library(Rtsne)
library(ggplot2)
library(stringr)
library(assertthat)
library(DOT)
library(rsvg)
library(glue)
library(dplyr)

# for debugging
# option    s(shiny.reactlog = TRUE) 
# options(shiny.autoload.r = FALSE)
# options(shiny.trace = TRUE)
# options(shiny.fullstacktrace = TRUE)
# options(shiny.error = browser)
options(shiny.autoreload = TRUE)
        
# source all functions
({
  list.files("R", pattern = "*.R") %>% paste0("R/", .) %>% sapply(source, .GlobalEnv)
}) %>% invisible()

# load tab info
tab_info <- suppressMessages(readr::read_csv("tabs.csv", comment = "#", na = character()))


    ################ test
