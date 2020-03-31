## UI
vs_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        tabBox(title = toupper("Collection of plots"), id = ns("targets"),
               width = 12,
               plot1UI(ns("plot_1")),
               tabPanel(title = "Plot 1"),
               tabPanel(title = "Plot 2"),
               tabPanel(title = "Plot 3"),
               tabPanel(title = "Plot 4"),
               tabPanel(title = "Plot 5"),
               tabPanel(title = "Plot 6"),
               tabPanel(title = "Plot 7"),
               tabPanel(title = "Plot 8")
        )
    )
}

## server
vs_mainServer <- function(input, output, session, shared){
    callModule(plot1Server, "plot_1", shared = shared)

}
