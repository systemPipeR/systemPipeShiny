## UI 
plot_pcaUI <- function(id){
  ns <- NS(id)
  tabPanel(title = "PCA", 
           h2("Make a PCA plot"),
           fluidRow(
             actionButton(ns("render"),
                          label = "Render the plot", 
                          icon("paper-plane")),
           ),
           fluidRow(
             actionButton(
               ns("op_1"),
               label = "Option1", 
               icon("cog")
             ),
             actionButton(
               ns("op_2"),
               label = "Option2", 
               icon("cog")
             ),
             actionButton(
               ns("op_3"),
               label = "Option3", 
               icon("cog")
             ),
             actionButton(
               ns("op_4"),
               label = "Option4", 
               icon("cog")
             ),
           ),
           uiOutput(ns("plot_ui"))
  )
}

## server
plot_pcaServer <- function(input, output, session, shared){
  ns <- session$ns
  observeEvent(input$render, {
    targets_file <- shared$targets$file
    targets <- read.delim(targets_file, comment.char = "#")
    targets <- data.frame(targets)
    countDF <- read.table(shared$count$file)
    colData <- data.frame(row.names = targets$SampleName, 
                          condition = targets$Factor)
    output$plot_ui <- renderUI(
      plotlyOutput(ns("pca"))
    )
    output$pca <- renderPlotly({
      run_PCA(countDF = countDF, targets = targets, colData = colData, method = "raw")
    })
  })
}
