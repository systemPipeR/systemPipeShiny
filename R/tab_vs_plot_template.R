## UI

#' @importFrom plotly plotlyOutput
#' @importFrom shinyjqui jqui_resizable
#' @importFrom shinyWidgets pickerInput
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
        # in case you need more than one data input, uncomment lines below
        pgPaneUI(ns("pg"),
                 titles = c("Package Requirements",
                            # "Input Metadata" = "meta",
                            "Input Dataframe",
                            # "Validate Meta",
                            "Validate Dataframe"),
                 pg_ids = c(ns("pkg"),
                            # ns("meta"),
                            ns("data"),
                            # ns("vd_meta"),
                            ns("vd_data"))
        ),
        tabTitle("Title for this kind of plot"),
        spsHr(), h3("Descrption"),
        hexPanel(ns("poweredby"), "POWERED BY:",
                 hex_imgs = c("img/sps.png"),
                 hex_titles = c("SystemPipeShiny"), ys = c("-10")),
        renderDesc(id = ns("desc"), desc),
        spsHr(), h3("Data preparation"),
        fluidRow(
            column(6,
                   genHrefTab(
                       c("data_targets"),
                       title = "You need to meta data from these tabs:")),
            column(6,
                   genHrefTab(
                       c("data_template"),
                       title = "You need to tabular data from these tabs:"))
        ),
        h5("Once you have prepared the data,
           select which tab(s) your data is coming from:"),
        column(6, shinyWidgets::pickerInput(ns("source_meta"), "Meta Data",
                    choices = c("Meta Data" = "data_targets"),
                    options = list(style = "btn-primary"))),
        column(6, shinyWidgets::pickerInput(ns("source_data"), "Tabular Data",
                    choices = c("Template Data Tab" = "data_template"),
                    options = list(style = "btn-primary"))), spsHr(),
        div(style = "text-align: center;",
            strong("Click the button below to start or reload data"), br(),
            actionButton(inputId = ns("validate_start"), label = "Start/Reload")
        ),
        spsHr(), h3("Plotting"),
        div(
            id = ns("tab_main"), class = "shinyjs-hide",
            uiExamples(ns), spsHr(),
            fluidRow(
                actionButton(ns("render"), label = "Render/Snapshot plot",
                             icon("paper-plane")),
            ),
            div(class = "sps-plot-container",
                shinyjqui::jqui_resizable(
                    sps_plots$addUI(plotly::plotlyOutput(ns("plot")), id)
                ),
                tags$script(glue('stretchPlotTab("{ns("plot")}")'))
            )
        )
    )
}

## server
#' @importFrom shinytoastr toastr_success toastr_info
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom shinyjs show
plot_templateServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "plot_template"
        # define data containers
        mydata <- reactiveValues()
        # start the tab by checking if required packages are installed
        observeEvent(input$validate_start, {
            req(shinyCheckPkg(session = session,
                                cran_pkg = c("base"),
                                bioc_pkg = c(""),
                                github = c("")
            ))
            pgPaneUpdate('pg', 'pkg', 100)
            mydata$data <- getData(isolate(input$source_data), shared)
            pgPaneUpdate('pg', 'data', 100)
            spsValidate({
                if (ncol(mydata$data) > 1) TRUE
                else stop("Need more than 1 column")
            }, "Raw data column check")

            pgPaneUpdate('pg', 'vd_data', 100)
            shinyjs::show(id = "tab_main")
            shinytoastr::toastr_success(
                title = "Ready for plotting!", message = "", timeOut = 5000,
                position = "bottom-right"
            )
        })
        observeEvent(input$render, {
            output$plot <- sps_plots$addServer(plotly::renderPlotly, tab_id, {
                plotly::ggplotly(
                    ggplot(mydata$df,
                           aes(Sepal.Length, Sepal.Width)) +
                    geom_point(ggplot2::aes(colour = Species))
                )
            })
            shared$snap_signal <- sps_plots$notifySnap(tab_id)
            req(shared$snap_signal)
            shinytoastr::toastr_info(
                glue("Snapshot {glue_collapse(shared$snap_signal, '-')}",
                     "added to canvas"),
                position = "bottom-right")
        })
    }
    moduleServer(id, module)
}
