## UI 
plot_pcaUI <- function(id){
  ns <- NS(id)
  tabPanel(title = "PCA", 
           h2("Make a PCA plot"),
           fluidRow(
             actionButton(
               ns("op_1"),
               label = "Raw", 
               icon("cog")
             ),
             actionButton(
               ns("op_2"),
               label = "R-log", 
               icon("cog")
             ),
             actionButton(
               ns("op_3"),
               label = "VST", 
               icon("cog")
             )
           ),
           fluidRow(
             actionButton(ns("render"),
                          label = "Render the plot", 
                          icon("paper-plane")),
           ),
           uiOutput(ns("plot_ui"))
           )
}

## server
plot_pcaServer <- function(input, output, session, shared){
  ns <- session$ns
  observeEvent(input$render, {
    targets <- data.frame(shared$targets$df)
    countDF <- data.frame(shared$count$df)
    colnames(countDF) <- countDF[1,]
    colnames(targets) <- targets[1,]
    countDF <- countDF[-1,]
    targets <- targets[-1,]
    rownames(countDF) <- countDF[,1]
    countDF <- countDF[,-1]
    rownames(targets) <- targets[,1]
    targets <- targets[,-1]
    colData <- data.frame(row.names = targets$SampleName, 
                          condition = targets$Factor)
    countDF[] <- lapply(countDF, function(x) as.numeric(x))
    countDF <- as.matrix(countDF)
    output$plot_ui <- renderUI(
      plotlyOutput(ns("pca"))
    )
    output$pca <- renderPlotly({
      run_PCA(countDF = countDF, targets = targets, colData = colData, method = "raw")
    })
  })
}
