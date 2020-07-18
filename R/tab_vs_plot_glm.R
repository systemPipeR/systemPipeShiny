## UI
plot_glmUI <- function(id){
    ns <- NS(id)
    tabPanel(title = "glm-PCA",
             h2("Make a glm-PCA plot"),
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
plot_glmServer <- function(id, shared){
    module <- function(input, output, session){
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
            colData <- data.frame(row.names = targets$SampleName,
                                  condition = targets$Factor)
            countDF[] <- lapply(countDF, function(x) as.numeric(x))
            countDF <- as.matrix(countDF)
            output$plot_ui <- renderUI(
                plotlyOutput(ns("glm"))
            )
            output$glm <- renderPlotly({
                run_GLM(countDF = countDF, targets = targets, colData = colData)
            })
        })
    }
    moduleServer(id, module)
}
