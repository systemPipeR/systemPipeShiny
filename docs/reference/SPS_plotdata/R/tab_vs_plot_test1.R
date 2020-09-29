###################### SPS Plot Tab Title tab######################
## creation date: 2020-09-29 14:18:51
## Author:

## lines with `#####` around are important sections, you may change/add  the
## values inside if you used default settings when creating the tab.

## UI for plot_test1

plot_test1UI <- function(id) {
    ns <- NS(id)
    desc <- "
    
    #### Some Description of this data in markdown
    - you should ...
        1. eg 1.
        2. eg 2.
        - **Notice**: ...`this` ...


    ```
    some code demo ...
    ```
    
    "
    tagList(
        ##### Progress tracker
        pgPaneUI(ns("pg"),
            titles = c(
                "Package Requirements",
                'Input from raw data',
                'Input from meta data',
                'Validate raw data',
                'Validate meta data'
            ),
            pg_ids = c(
                ns("pkg"),
                ns('raw'),
                ns('meta'),
                ns('vd_raw'),
                ns('vd_meta')
            )
        ),
        #####
        tabTitle("Plot Tab Title"),
        spsHr(), h3("Descrption"),
        hexPanel(ns("poweredby"), "POWERED BY:",
            hex_imgs = c("img/sps.png"),
            hex_titles = c("SystemPipeShiny"), ys = c("-10")
        ),
        renderDesc(id = ns("desc"), desc),
        spsHr(), h3("Data preparation"),
        fluidRow(
            column(6, genHrefTab(
                c("data_df1"),
                title = "You need to prepare raw data from these tabs:"
            )),
            column(6, genHrefTab(
                c("data_df1", "data_df2"),
                title = "You need to prepare meta data from these tabs:"
            ))
        ),
        h5("Once you have prepared the data,
           select which tab(s) your data is coming from:"),
        ##### select input data
        fluidRow(
            column(6, shinyWidgets::pickerInput(ns("source_raw"), "raw data",
                choices = c("df 1" = "data_df1"),
                options = list(style = "btn-primary")
            )),
            column(6, shinyWidgets::pickerInput(ns("source_meta"), "meta data",
                choices = c(
                    "df 1" = "data_df1",
                    "df 2" = "data_df2"
                ),
                options = list(style = "btn-primary")
            ))
        ),
        spsHr(),
        #####
        div(
            style = "text-align: center;",
            strong("Click the button below to start or reload data"), br(),
            actionButton(inputId = ns("validate_start"), label = "Start/Reload")
        ),
        spsHr(), h3("Plotting"),
        div(
            id = ns("tab_main"), class = "shinyjs-hide",
            ##### UI of different plot options
            div(
                id = ns("plot_opts"), style = "background: white;",
                tagList(h3("Some plotting options"))
            ),
            #####
            spsHr(),
            fluidRow(
                actionButton(ns("render"),
                    label = "Render/Snapshot plot",
                    icon("paper-plane")
                ),
            ),
            div(
                class = "sps-plot-container",
                shinyjqui::jqui_resizable(
                    ##### plot container
                    sps_plots$addUI(plotly::plotlyOutput(ns("plot")), id)
                    #####
                ),
                tags$script(glue('stretchPlotTab("{ns("plot")}")'))
            )
        )
    )
}

## server for plot_test1

plot_test1Server <- function(id, shared) {
    module <- function(input, output, session) {
        ns <- session$ns
        tab_id <- "plot_test1"
        # define a data container for all data sets
        mydata <- reactiveValues()
        observeEvent(input$validate_start, {
            ##### package check
            req(shinyCheckPkg(
                session = session,
                cran_pkg = c('base'),
                bioc_pkg = c(''),
                github = c('')
            ))
            #####
            pgPaneUpdate('pg', 'pkg', 100)
            ##### get and validate input data sets
            mydata$raw <- getData(isolate(input$source_raw), shared)
            pgPaneUpdate("pg", "raw", 100)
            mydata$meta <- getData(isolate(input$source_meta), shared)
            pgPaneUpdate("pg", "meta", 100)
            spsValidate(
                {
                    if (!is.data.frame(mydata$raw)) stop("Input raw data need to be a dataframe")
                },
                vd_name = "Validate raw data"
            )
            pgPaneUpdate("pg", "vd_raw", 100)
            spsValidate(
                {
                    if (!is.data.frame(mydata$meta)) stop("Input raw data need to be a dataframe")
                    if (nrow(mydata$meta) < 1) stop(
                        "Input raw data need to have at least one row"
                    )
                },
                vd_name = "Validate meta data"
            )
            pgPaneUpdate("pg", "vd_meta", 100)
            #####
            shinyjs::show(id = "tab_main")
            shinytoastr::toastr_success(
                title = "Ready for plotting!", message = "", timeOut = 5000,
                position = "bottom-right"
            )
        })
        observeEvent(input$render, {
            ##### plotting function
            output$plot <- sps_plots$addServer(plotly::renderPlotly, tab_id, {
                plotly::ggplotly(ggplot(mydata$data, aes_string(names(mydata$data)[1], names(
                    mydata$data
                )[2])) +
                    geom_point(aes(color = seq_len(nrow(mydata$data)))))
            })
            #####
            shared$snap_signal <- sps_plots$notifySnap(tab_id)
            req(shared$snap_signal)
            shinytoastr::toastr_info(
                glue(
                    "Snapshot {glue_collapse(shared$snap_signal, '-')}",
                    "added to canvas"
                ),
                position = "bottom-right"
            )
        })
    }
    moduleServer(id, module)
}
