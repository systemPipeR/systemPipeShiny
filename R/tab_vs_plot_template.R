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
        pgPaneUI(ns("pg")),
        h2("Title for this kind of plot"), spsHr(),
        renderDesc(id = ns("desc"), desc), spsHr(),
        genHrefTab(c("df_targets", "df_template"),
                   title = "You need to prepare data from these tabs:"),
        spsHr(),
        div(style = "text-align: center;",
            strong("Click the button below to start or reload data"), br(),
            actionButton(inputId = ns("validate_start"), label = "Start/Reload")
        ),
        spsHr(),
        div(
            id = ns("tab_main"), class = "shinyjs-hide",
            uiExamples(ns), spsHr(),
            fluidRow(

                actionButton(ns("render"), label = "Render the plot",
                             icon("paper-plane")),
            ),
            div(class = "sps-plot-container", uiOutput(ns("plot_ui")))
        )
    )

}

## server
plot_templateServer <- function(input, output, session, shared){
    ns <- session$ns
    # register the progress control first
    pg_values <- reactiveValues()
    output$pg <- pgPaneServer(c("Package Requirements" = "pkg" ,
                                # "Input Metadata" = "meta",
                                "Input Data" = "data",
                                # "Validate Meta" = "vd_meta",
                                "Validate Data" = "vd_data"),
                              pg_values, ns)
    # define data containers
    mydata <- reactiveValues()
    # define validators
    validate_meta <- list(
        meta = function(meta, data1) {
            cat(data1)
            if (ncol(meta) > 1) return(c(" " = TRUE))
            else return(c("Need more than 1 column" = FALSE))
        }
    )
    validate_data <- list(
        data = function(data, data2) {
            cat(data2)
            if (ncol(data) > 1) return(c(" " = TRUE))
            else return(c("Need more than 1 column" = FALSE))
        }
    )
    # start the tab by checking if required packages are installed
    observeEvent(input$validate_start, {
        req(shinyCheckSpace(session = session,
            cran_pkg = c("base"),
            bioc_pkg = c(""),
            github = c("")
        ))
        updatePg('pkg', 100, pg_values)
        # mydata$meta <- getData("df_targets", shared)
        # updatePg("meta", 100, pg_values)
        mydata$df <- getData("df_template", shared)
        updatePg('data', 100, pg_values)
        # spsValidator(validate_meta,
        #              args = list(meta = mydata$meta,
        #                          data1 = "this is data1\n"),
        #              title = "Meta Validations")
        # updatePg("vd_meta", 100, pg_values)
        spsValidator(validate_data,
                     args = list(data = mydata$df,
                                 data2 = "this is data2\n"),
                     title = "Data Validations")
        updatePg("vd_data", 100, pg_values)
        shinyjs::show(id = "tab_main")
        toastr_success(
            title = "Ready for plotting!",
            message = "",
            timeOut = 5000,
            position = "bottom-right"
        )
    })
    observeEvent(input$render, {
        plot_out <- renderUI(
            jqui_resizable(plotOutput(ns("plot")))
            )
        output$plot_ui <- plot_out
        addPlot(plot_out, shared, ns)
    })
    output$plot <- renderPlot({
        ggplot(mydata$df, aes(Sepal.Length, Sepal.Width)) +
            geom_point(aes(colour = Species))
    })
}
