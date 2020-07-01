########################## Template for data tab ###############################

## UI
df_templateUI <- function(id){
    ns <- NS(id)
    # describe your tab in markdown format, this will go right under the title
    desc <- "
    #### Some Description of this data
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
                 titles = c("Package Requirements", "Data Loaded",
                            "Input Data Validation", "Preprocess"),
                 pg_ids = c(ns("pkg"), ns("data"), ns("vd_data"), ns("prepro"))
        ),
        tabTitle("Title for this kind of dataframe"), spsHr(),
        hexPanel(ns("poweredby"), "POWERED BY:",
                 hex_imgs = c("sps/systemPipe_small.png"),
                 hex_titles = c("SystemPipeShiny")),
        renderDesc(id = ns("desc"), desc),
        div(style = "text-align: center;",
            actionButton(inputId = ns("validate_start"), label = "Start with this tab")
        ),
        div(
            id = ns("tab_main"), class = "shinyjs-hide",
            radioGroupButtons(
                inputId = ns("data_source"), label = "Choose your data file source:",
                selected = "upload",
                choiceNames = c("Upload", "Example"),
                choiceValues = c("upload", "eg"),
                justified = TRUE, status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
            ),
            fluidRow(
                column(width = 5, dynamicFile(id = ns("file_upload"))),
                column(width = 3,
                       pickerInput(
                           inputId = ns("delim"), label = "File delimiter",
                           choices = c(`,`=",", space=" ", Tab="\t", `|`="|", `:`=":", `;`=";"),
                           options = list(style = "btn-primary")
                )),
                column(width = 3, clearableTextInput(ns("comment"), "File comments", value = "#"))
            ),
            fluidRow(h4("Input Data", style="text-align: center;")),
            div(style = "background-color: #F1F1F1;", DT::DTOutput(ns("df"))),
            fluidRow(
                hr(), h4("Choose a proprocessing method"),
                p("Depending on different ways of preprocessing, different plotting options will be available"),
                column(4,
                    pickerInput(
                        inputId = ns("select_prepro"),
                        choices = c(`Do Nothing`='nothing', `Method 1`='md1', `Method 2`='md2'),
                        options = list(style = "btn-primary")
                    )
                ),
                column(2,
                      actionButton(ns("preprocess"), label = "Preprocess", icon("paper-plane"))
                )
            ),
            fluidRow(id = ns("plot_option_row"), class = "shinyjs-hide",
                     uiOutput(ns("plot_option"))
            )
        )
    )
}

## server
df_templateServer <- function(input, output, session, shared){
    ns <- session$ns
    tab_id <- "df_template"
    # start the tab by checking if required packages are installed
    observeEvent(input$validate_start, {
        print(class(session))
        req(shinyCheckSpace(
            session = session,
            cran_pkg = c("base"),
            bioc_pkg = c(""),
            github = c("")
        ))
        shinyjs::show(id = "tab_main")
        shinyjs::hide(id = "validate_start")
        pgPaneUpdate('pg', 'pkg', 100) # update progress
    })
    observeEvent(input$data_source, toggleState(id = "file_upload"), ignoreInit = TRUE)
    # get upload path, note path is in upload_path()$datapath
    upload_path <- dynamicFileServer(input, session, id = "file_upload")
    # load the file dynamically
    data_df <- reactive({
        df_path <- upload_path()
        pgPaneUpdate('pg', 'data', 0) # set data progress to 0 every time reloads
        loadDF(choice = input$data_source, upload_path = df_path$datapath,
                  delim = input$delim, comment = input$comment,
                  eg_path = "inst/extdata/iris.csv")
    })
    # display table
    output$df <- DT::renderDT({
        shiny::validate(
            need(not_empty(data_df()), message = "Data file is not loaded")
        )
        pgPaneUpdate('pg', 'data', 100)
        DT::datatable(
            data_df(), style = "bootstrap", class = "compact",  filter = "top",
            extensions = c( 'Scroller','Buttons'), options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                deferRender = TRUE, scrollY = 580, scrollX = TRUE, scroller = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
            ))
    })

    # preprocess
    ## define validators
    df_validate_common <- list(
        vd1 = function(df){
            if (is(df, "data.frame")) {result <- c(" " = TRUE)}
            else {result <- c("Input is not dataframe or tibble" = FALSE)}
            return(result)
        },
        vd2 = function(df){
            if (ncol(df) > 1) {result <- c(" " = TRUE)}
            else {result <- c("Input is not dataframe or tibble" = FALSE)}
            Sys.sleep(1)
            return(result)
        }
    )
    ## define special validations for different reprocesses
    df_validate_method1 <- list(
        md1 = function(df, special1) {
            cat(special1)
            if (ncol(df) > 1) return(c(" " = TRUE))
            else return(c("Need more than 1 column" = FALSE))
        }
    )
    df_validate_method2 <- list(
        md1 = function(df, special2) {
            cat(special2)
            if (ncol(df) > 1) return(c(" " = TRUE))
            else return(c("Need more than 1 column" = FALSE))
        }
    )
    # start validation and preprocess
    observeEvent(input$preprocess, ignoreInit = TRUE, {
        # get filtered df
        df_filter <-  data_df()[input$df_rows_all, ]
        # validate data
        spsValidator(df_validate_common,
                     args = list(df = df_filter,
                                 apple = "apple\n",
                                 banana = "banana\n"),
                     title = "Common Validations")
        # validate special requirements for different preprocess
        switch(
            input$select_prepro,
            'md1' = spsValidator(
                df_validate_method1,
                args = list(df = df_filter, special1 = "validation for method 1\n"),
                title = "Special validation for method 1"
            ),
            'md2' = spsValidator(
                df_validate_method2,
                args = list(df = df_filter, special2 = "validation for method 2\n"),
                title = "Special validation for method 2"
            ),
            msg('No addition validation required')
        )
        pgPaneUpdate('pg', 'vd_data', 100)
        # if validation passed, start reprocess
        df_processed <- shinyCatch(switch(input$select_prepro,
           'md1' = {
               # your preprocess function, e.g
               df_filter[, 1] = df_filter[, 1] + 1
           },
           'md2' = {
               df_filter[, 1] = log(df_filter[, 1])
           },
           df_filter
        ), blocking_level = 'error')
        req(!is.null(df_processed))
        pgPaneUpdate('pg', 'prepro', 100)
        # add data to task
        addData(df_processed, shared, tab_id)
        toastr_success(
            title = "Preprocess done!",
            message = "You can choose your plot options below",
            timeOut = 5000,
            position = "bottom-right"
        )
        shinyjs::show(id = "plot_option_row")
        gallery <- switch(input$select_prepro,
                          'md1' = genGallery("plot_pca"),
                          'md2' = genGallery(c("plot_template", "plot_pca")),
                          genGallery(type = "vs")
        )
        output$plot_option <- renderUI({
            gallery
        })
    })
}
