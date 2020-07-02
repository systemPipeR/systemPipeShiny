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
                 hex_imgs = c("sps/systemPipeR_site.png"),
                 hex_titles = c("SystemPipeShiny"), ys = c("-10")),
        renderDesc(id = ns("desc"), desc),
        spsHr(), h3("Data preparation"),
        fluidRow(
            column(6, genHrefTab(c("df_targets"), title = "You need to meta data from these tabs:")),
            column(6, genHrefTab(c("df_template"), title = "You need to tabular data from these tabs:"))
        ),
        h5("Once you have prepared the data, select which tab(s) your data is coming from:"),
        column(6, pickerInput(ns("source_meta"), "Meta Data",
                    choices = c("Meta Data" = "df_targets"),
                    options = list(style = "btn-primary"))),
        column(6, pickerInput(ns("source_data"), "Tabular Data",
                    choices = c("Template Data Tab" = "df_template"),
                    options = list(style = "btn-primary"))), spsHr(),
        div(style = "text-align: center;",
            strong("Click the button below to start or reload data"), br(),
            actionButton(inputId = ns("validate_start"), label = "Start/Reload")
        ),
        spsHr(), h3("Plotting"),

            fluidRow(
                actionButton(ns("render"), label = "Render/Snapshot plot",
                             icon("paper-plane")),
            ),
            div(class = "sps-plot-container",
                jqui_resizable(sps_plots$addUI(plotlyOutput(ns("plot")), id)),
                tags$script(glue('stretchPlotTab("{ns("plot")}")'))
            )
    )
}

## server
plot_templateServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "plot_template"
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
            pgPaneUpdate('pg', 'pkg', 100)
            # mydata$meta <- getData(isolate(input$source_meta), shared)
            # pgPaneUpdate('pg', 'meta', 100)
            mydata$df <- getData(isolate(input$source_data), shared)
            pgPaneUpdate('pg', 'data', 100)
            # spsValidator(validate_meta,
            #              args = list(meta = mydata$meta,
            #                          data1 = "this is data1\n"),
            #              title = "Meta Validations")
            # pgPaneUpdate('pg', 'vd_meta', 100)
            spsValidator(validate_data,
                         args = list(data = mydata$df,
                                     data2 = "this is data2\n"),
                         title = "Data Validations")
            pgPaneUpdate('pg', 'vd_data', 100)
            shinyjs::show(id = "tab_main")
            toastr_success(
                title = "Ready for plotting!", message = "", timeOut = 5000,
                position = "bottom-right"
            )
        })
        observeEvent(input$render, {
            output$plot <- sps_plots$addServer(renderPlotly, tab_id, {
                ggplotly(ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
                             geom_point(aes(colour = Species))
                )
            })
            shared$snap_signal <- sps_plots$notifySnap(tab_id)
            req(shared$snap_signal)
            toastr_info(glue("Snapshot {glue_collapse(shared$snap_signal, '-')} added to canvas"),
                        position = "bottom-right")
        })
    }
    moduleServer(id, module)
}
