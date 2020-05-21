########################## Template for data tab ###############################

## UI
df_rawUI <- function(id){
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
        h2("Title for this kind of dataframe"),
        renderDesc(desc),
        # first validate required packages and other prerequisites
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
                       selectizeInput(
                           inputId = ns("delim"), label = "File delimiter",
                           choices = c(Tab="\t", space=" ", `,`=",", `|`="|", `:`=":", `;`=";")
                )),
                column(width = 3, clearableTextInput(ns("comment"), "File comments", value = "#")),
            ),
            fluidRow(h4("Input Data", style="text-align: center;")),
            div(style = "background-color: #F1F1F1;", DT::DTOutput(ns("df"))),
            hr(), h4("Choose a proprocessing method"),
            p("Depending on different ways of preprocessing, different plotting options will be available"),
            column(
                width = 12, style = "padding-top: 15px; text-align: center;",
                # actionButton(ns("df_reload"), label = "Load/Reload Data Input", icon("redo-alt")),
                actionButton(ns("to_task"), label = "Add to task", icon("paper-plane"))
            ),
            fluidRow(id = ns("plot_options"),
                     a("Scatter Plot", href = "#shiny-tab-plot_point"),
                     a("plot2"),
                     a("plot3"),
                     p("...")
            )
        )
    )
}

## server
df_rawServer <- function(input, output, session, shared){
    ns <- session$ns
    # start the tab by checking if required packages are installed
    shinyjs::hide(id = "tab_main")
    observeEvent(input$validate_start, {
        if (shinyCheckSpace(
            session = session,
            cran_pkg = c("base"),
            bioc_pkg = c(""),
            github = c("")
        )) {
            shinyjs::show(id = "tab_main")
            shinyjs::hide(id = "validate_start")
        }
    })
    observeEvent(input$data_source, toggleState(id = "file_upload"), ignoreInit = TRUE)
    # get upload path, note path is in upload_path()$datapath
    upload_path <- dynamicFileServer(input,session, id = "file_upload")
    observe({
        # print(input$delim )
        # print(input$df_rows_all)
        # print(data_df()[input$df_rows_all, ])
        })
    # load the file dynamically
    data_df <- reactive({
        df_path <- upload_path()
        loadDF(choice = input$data_source, upload_path = df_path$datapath,
                  delim = input$delim, comment = input$comment,
                  eg_path = "inst/extdata/iris.csv")
    })
    # display table
    output$df <- DT::renderDT(DT::datatable(
        data_df(), style = "bootstrap", class = "compact",  filter = "top",
        extensions = c( 'Scroller','Buttons'), options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            deferRender = TRUE, scrollY = 600, scrollX = TRUE, scroller = TRUE,
            columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
    ))

    # preprocess
    observeEvent(input$to_task, {
        check_results <- T
        if(!all(check_results)) {

        } else {
            shinyjs::show(id = "plot_options")
            sendSweetAlert(
                session = session, type = "success",
                title = "Data added", text = "Choose a plot type"
            )
        }

    })

}
