# shiny and shiny visual
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(pushbar)
library(rhandsontable)
library(magrittr)
library(shinyjs)
library(shinyWidgets)
library(shinyTree)
library(networkD3)
library(plotly)
library(shinyAce)
# BiocManager::install("timelyportfolio/sweetalertR")
# library(sweetalertR) # use alert from shinyWeidgets for now, if no more advanced function required
# source("https://install-github.me/mangothecat/shinytoastr")
library(shinytoastr) # need to install use the script above
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
library(dplyr)

({
  list.files("R", pattern = "*.R") %>% paste0("R/", .) %>% sapply(source, .GlobalEnv)
}) %>% invisible()



################ test

# ui <- fluidPage(
#     pushbar_deps(),
#     br(),
#     actionButton("open", "Open pushbar"),
#     pushbar(
#         h4("HELLO"),
#         id = "myPushbar", # add id to get event
#         actionButton("close", "Close pushbar")
#     ),
#     fluidRow(
#         column(5),
#         column(5, span("Is a pushbar opened?"), verbatimTextOutput("ev"))
#     )
# )
# 
# server <- function(input, output, session){
#     
#     setup_pushbar() # setup
#     
#     observeEvent(input$open, {    
#         pushbar_open(id = "myPushbar")
#     })  
#     
#     observeEvent(input$close, {
#         pushbar_close()
#     })  
#     
#     output$ev <- renderPrint({
#         input$myPushbar_pushbar_opened
#     })
# }
# 
# if(interactive()) shinyApp(ui, server)
