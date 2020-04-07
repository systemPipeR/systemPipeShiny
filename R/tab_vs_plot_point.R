## UI 
plot_pointUI <- function(id){
    ns <- NS(id)
    tabPanel(title = "Scatter", 
             h2("Make a xx plot"),
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
plot_pointServer <- function(input, output, session, shared){
    ns <- session$ns
    observeEvent(input$render, {
        output$plot_ui <- renderUI(
            plotOutput(ns("point"))
            )
    })
    output$point <- renderPlot({
        p <- ggplot(mtcars, aes(wt, mpg))
        print(p + geom_point(aes(colour = factor(cyl))))
    })
}
