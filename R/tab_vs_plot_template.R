## UI
plot_templateUI <- function(id){
    ns <- NS(id)
    desc <- "
    #### Some Description of this kind of plot
    - you should ...
        1. eg 1.
        2. eg 2.
        - **Notice**: ...`this` ...


    ```
    some code demo ...
    ```
    "
    tagList(
        h2("Title for this kind of plot"), spsHr(),
        renderDesc(id = ns("desc"), desc), spsHr(),
        # first validate required packages and other prerequisites
        div(style = "text-align: center;",
            actionButton(inputId = ns("validate_start"), label = "Start with this tab")
        ),
        div(
            id = ns("tab_main"), class = "shinyjs-hide",
            uiExamples(ns), spsHr(),
            fluidRow(

                actionButton(ns("render"), label = "Render the plot",
                             icon("paper-plane")),
            ),
            uiOutput(ns("plot_ui"))
        )
    )

}

## server
plot_templateServer <- function(input, output, session, shared){
    ns <- session$ns
    # start the tab by checking if required packages are installed
    observeEvent(input$validate_start, {
        req(shinyCheckSpace(session = session,
            cran_pkg = c("base"),
            bioc_pkg = c(""),
            github = c("")
        ))

        shinyjs::show(id = "tab_main")
        shinyjs::hide(id = "validate_start")

    })
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
