## UI 
plot_tsneUI <- function(id){
    ns <- NS(id)
    tabPanel(title = "t-SNE", 
             h2("Make a t-SNE plot"),
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
plot_tsneServer <- function(input, output, session, shared){
    ns <- session$ns
 
    observeEvent(input$render, {
        targets <- data.frame(shared$df$target)
        countDF <- data.frame(shared$count$df)
        colnames(countDF) <- countDF[1,]
        colnames(targets) <- targets[1,]
        countDF <- countDF[-1,]
        targets <- targets[-1,]
        rownames(countDF) <- countDF[,1]
        countDF <- countDF[,-1]
        rownames(targets) <- targets[,1]
        targets <- targets[,-1]
        countDF[] <- lapply(countDF, function(x) as.numeric(x))
        countDF <- as.matrix(countDF)
        output$plot_ui <- renderUI(
            plotlyOutput(ns("tsne"))
        )
        output$tsne <- renderPlotly({
            run_TSNE(countDF = countDF, targets = targets)
        })
    })
}
