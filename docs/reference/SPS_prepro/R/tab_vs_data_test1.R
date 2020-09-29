###################### SPS test 1 tab######################
## creation date: 2020-09-29 14:18:53
## Author:

# data_test1 UI
data_test1UI <- function(id) {
    ns <- NS(id)
    # describe your tab in markdown format, this will go right under the title
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
        pgPaneUI(ns("pg"),
            titles = c(
                "Package Requirements", "Data Loaded",
                "Input Data Validation", "Preprocess"
            ),
            pg_ids = c(ns("pkg"), ns("data"), ns("vd_data"), ns("prepro"))
        ),
        tabTitle("Title for test 1"), spsHr(),
        hexPanel(ns("poweredby"), "POWERED BY:",
            hex_imgs = c("img/sps.png"),
            hex_titles = c("SystemPipeShiny"), ys = c("-10")
        ),
        renderDesc(id = ns("desc"), desc),
        div(
            style = "text-align: center;",
            actionButton(
                inputId = ns("validate_start"),
                label = "Start with this tab"
            )
        ),
        div(
            id = ns("tab_main"), class = "shinyjs-hide",
            shinyWidgets::radioGroupButtons(
                inputId = ns("data_source"),
                label = "Choose your data file source:",
                selected = "upload",
                choiceNames = c("Upload", "Example"),
                choiceValues = c("upload", "eg"),
                justified = TRUE, status = "primary",
                checkIcon = list(
                    yes = icon("ok", lib = "glyphicon"),
                    no = icon("")
                )
            ),
            fluidRow(
                column(width = 5, dynamicFile(id = ns("file_upload"))),
                column(
                    width = 3,
                    shinyWidgets::pickerInput(
                        inputId = ns("delim"), label = "File delimiter",
                        choices = c(
                            `,` = ",", space = " ",
                            Tab = "\t", `|` = "|", `:` = ":", `;` = ";"
                        ),
                        options = list(style = "btn-primary")
                    )
                ),
                column(
                    width = 3,
                    clearableTextInput(
                        ns("comment"), "File comments",
                        value = "#"
                    )
                )
            ),
            fluidRow(h4("Input Data", style = "text-align: center;")),
            div(style = "background-color: #F1F1F1;", DT::DTOutput(ns("df"))),
            fluidRow(
                hr(), h4("Choose a proprocessing method"),
                p("Depending on different ways of preprocessing,
                  different plotting options will be available"),
                column(
                    4,
                    shinyWidgets::pickerInput(
                        inputId = ns("select_prepro"),
                        choices = c(
                            `take log of first column` = 'log',
                            `add one to first column` = 'addone'
                        ),
                        options = list(style = "btn-primary")
                    )
                ),
                column(
                    2,
                    actionButton(ns("preprocess"),
                        label = "Preprocess",
                        icon("paper-plane")
                    )
                )
            ),
            fluidRow(
                id = ns("plot_option_row"), class = "shinyjs-hide",
                uiOutput(ns("plot_option"))
            )
        )
    )
}

## data_test1 server

data_test1Server <- function(id, shared) {
    module <- function(input, output, session) {
        ns <- session$ns
        tab_id <- "data_test1"
        # start the tab by checking if required packages are installed
        observeEvent(input$validate_start, {
            req(shinyCheckPkg(
                session = session,
                cran_pkg = c('base'),
                bioc_pkg = c(''),
                github = c('')
            ))
            shinyjs::show(id = "tab_main")
            shinyjs::hide(id = "validate_start")
            pgPaneUpdate('pg', 'pkg', 100) # update progress
        })
        observeEvent(input$data_source,
            shinyjs::toggleState(id = "file_upload"),
            ignoreInit = TRUE
        )
        # get upload path, note path is in upload_path()$datapath
        upload_path <- dynamicFileServer(input,
            session,
            id = "file_upload"
        ) # this is reactive
        # load the file dynamically
        data_df <- reactive({
            df_path <- upload_path()
            pgPaneUpdate('pg', 'data', 0) # set data progress to 0 every reload
            loadDF(
                choice = input$data_source, upload_path = df_path$datapath,
                delim = input$delim, comment = input$comment,
                eg_path = '/home/dcassol/danielac@ucr.edu/testing/systemPipeShiny/docs/reference/SPS_prepro/data/iris.csv'
            )
        })
        # display table
        output$df <- DT::renderDT({
            shiny::validate(
                need(not_empty(data_df()), message = "Data file is not loaded")
            )
            pgPaneUpdate('pg', 'data', 100)
            DT::datatable(
                data_df(),
                style = "bootstrap",
                class = "compact", filter = "top",
                extensions = c('Scroller', 'Buttons'),
                options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    deferRender = TRUE,
                    scrollY = 580, scrollX = TRUE, scroller = TRUE,
                    columnDefs = list(list(
                        className = 'dt-center',
                        targets = "_all"
                    ))
                )
            )
        })
        # start validation and preprocess
        observeEvent(input$preprocess, ignoreInit = TRUE, {
            # get filtered df
            data_filtered <- data_df()[input$df_rows_all, ]
            # validate data with common validation checks
            spsValidate(
                {
                    "pass"
                },
                "common"
            )
            # validate special requirements for different preprocess methods
            switch(
                input$select_prepro,
                'log' = spsValidate(
                    {
                        if (!is.data.frame(data_filtered)) stop(
                            "Input input data need to be a dataframe"
                        )
                    },
                    vd_name = "log method pre-checks"
                ),
                'addone' = myVd(data_filtered, "add one method pre-checks")
            )
            pgPaneUpdate('pg', 'vd_data', 100)
            # if validation passed, start reprocess
            data_processed <- shinyCatch(
                switch(input$select_prepro,
                    'log' = {
                        if (is.numeric(data_filtered[, 1])) {
                            if (all(data_filtered[, 1] > 0)) {
                                data_filtered[, 1] <- log(data_filtered[, 1])
                            }
                        }
                        data_filtered
                    },
                    'addone' = myPreprocess(data_filtered)
                ),
                blocking_level = 'error'
            )
            spsValidate(not_empty(data_processed), "Final data is not empty")
            pgPaneUpdate('pg', 'prepro', 100)
            # add data to task
            addData(data_processed, shared, tab_id)
            shinytoastr::toastr_success(
                title = "Preprocess done!",
                message = "You can choose your plot options below",
                timeOut = 5000,
                position = "bottom-right"
            )
            shinyjs::show(id = "plot_option_row")
            output$plot_option <- renderUI({
                switch(
                    input$select_prepro,
                    'log' = genGallery(c('plot_xx1', 'plot_xx2')),
                    'addone' = genGallery(c('plot_xx1'))
                )
            })
        })
    }
    moduleServer(id, module)
}
