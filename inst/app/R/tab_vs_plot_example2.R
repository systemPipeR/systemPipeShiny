###################### SPS Plot Tab Title tab######################
## creation date: 2020-09-28 22:47:20
## Author:

## lines with `#####` around are important sections, you may change/add  the
## values inside if you used default settings when creating the tab.

## UI for plot_example2

plot_example2UI <- function(id) {
    ns <- NS(id)
    desc <- "

    #### Make a Bar Plot
    An example plot tab that receives data from 'Data Tab Example' and generates
    a bar plot of total reads from a count table.
    "
    tagList(
        ##### Progress tracker
        pgPaneUI(ns("pg"),
            titles = c(
                "Package Requirements",
                'Input from Count table',
                'Validate Count table'
            ),
            pg_ids = c(
                ns("pkg"),
                ns('data'),
                ns('vd_data')
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
                c("data_example"),
                title = "You need to prepare count data from these tabs:"
            ))
        ),
        h5("Once you have prepared the data,
           select which tab(s) your data is coming from:"),
        ##### select input data
        fluidRow(
            column(6, shinyWidgets::pickerInput(ns("source_data"), "Count data",
                choices = c("Data Tab Example" = "data_example"),
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
                shinydashboardPlus::boxPlus(
                    width = 12,
                    title = "Some exmaple options to control the plot",
                    closable = FALSE,
                    clearableTextInput(ns("plot_title"),
                                       label = "Plot title",
                                       value = "Total reads by samples")
                )
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
                tags$script(glue::glue('stretchPlotTab("{ns("plot")}")'))
            )
        )
    )
}

## server for plot_example2
plot_example2Server <- function(id, shared) {
    module <- function(input, output, session) {
        ns <- session$ns
        tab_id <- "plot_example2"
        # define a data container for all data sets
        mydata <- reactiveValues()
        observeEvent(input$validate_start, {
            ##### package check
            req(shinyCheckPkg(
                session = session,
                cran_pkg = c('ggplot2', 'plotly'),
                bioc_pkg = c(''),
                github = c('')
            ))
            #####
            pgPaneUpdate('pg', 'pkg', 100)
            ##### get and validate input data sets
            mydata$data <- getData(isolate(input$source_data), shared)
            pgPaneUpdate("pg", "data", 100)
            spsValidate({
                if (is.data.frame(mydata$data)) TRUE
                else stop("Count table needs to be a dataframe or tibble")
            })
            pgPaneUpdate("pg", "vd_data", 100)
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
                reads <- apply(mydata$data[, -1], 2, sum)
                reads_df <- data.frame(samples = names(reads),
                                       reads = reads)
                plotly::ggplotly({
                    ggplot2::ggplot(reads_df) +
                        ggplot2::geom_bar(ggplot2::aes(x = samples, y = reads, fill = samples),
                             stat = "identity")+
                        ggplot2:: coord_flip()+
                        ggplot2::ggtitle(as.character(input$plot_title))

                })
            })
            #####
            shared$snap_signal <- sps_plots$notifySnap(tab_id)
            req(shared$snap_signal)
            shinytoastr::toastr_info(
                glue::glue(
                    "Snapshot {glue::glue_collapse(shared$snap_signal, '-')}",
                    "added to canvas"
                ),
                position = "bottom-right"
            )
        })
    }
    moduleServer(id, module)
}
