## UI 
plot_volcanoUI <- function(id){
    ns <- NS(id)
    tabPanel(title = "Volcano Plot", 
             h2("Make a Volcano plot"),
             fluidRow(
                 actionButton(
                     ns("op_1"),
                     label = "Fold", 
                     icon("cog")
                 ),
                 actionButton(
                     ns("op_2"),
                     label = "FDR", 
                     icon("cog")
                 ),
                 actionButton(
                     ns("op_3"),
                     label = "Comparison", 
                     icon("cog")
                 )
             ),
             fluidRow(
                 actionButton(ns("render"),
                              label = "Render the plot", 
                              icon("paper-plane"))
             ),
             uiOutput(ns("plot_ui"))
    )
}

## server
plot_volcanoServer <- function(input, output, session, shared){
    ns <- session$ns
    observeEvent(input$render, {
        countDF <- data.frame(shared$count$df)
        colnames(countDF) <- countDF[1,]
        countDF <- countDF[-1,]
        rownames(countDF) <- countDF[,1]
        countDF <- countDF[,-1]
        output$plot_ui <- renderUI(
            plotlyOutput(ns("volcano"))
        )
        output$volcano <- renderPlotly({
            run_volcano(DF = countDF,FDR = 10, Fold = 2, comparison = "M1-A1")
        })
    })
}
