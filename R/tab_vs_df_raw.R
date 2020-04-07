## UI
df_rawUI <- function(id){
    ns <- NS(id)
    tagList(
        h4("From this dataframe, you can plot:"),
        radioGroupButtons(
            inputId = ns("plot_source"), label = "Choose your plot file source:", 
            selected = "upload",
            choiceNames = c("Upload", "Example"), 
            choiceValues = c("upload", "eg"),
            justified = TRUE, status = "primary",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
        ),
        fileInput(
            ns("df_upload"), "If upload, choose your df file here:",
            multiple = FALSE,
            accept = c("txt", "csv", "tsv"),
            placeholder = "Choose your df file path",
        ),
        column(width = 12, style = "padding-left: 0;",
               downloadButton(ns("down_config"), "Save"),
               actionButton(ns("to_task"),
                            label = "Add to task", 
                            icon("paper-plane"))
        ),
        rHandsontableOutput(ns("df")),
        fluidRow(id = "plot_options",
                 a("Scatter Plot", href = "#shiny-tab-plot_point"),
                 a("plot2"),
                 a("plot3"),
                 p("...")
        )
        )
}

## server
df_rawServer <- function(input, output, session, shared){
    ns <- session$ns
    shinyjs::hide(id = "plot_options")
    # update table
    output$df <- renderRHandsontable({
        rhandsontable(
            data.frame(matrix("", 8,8), stringsAsFactors = FALSE), selectCallback = TRUE, useTypes = FALSE) %>%
                hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    })
    onclick("to_task", shinyjs::show(id = "plot_options")) 
    observeEvent(input$to_task, {
        sendSweetAlert(
            session = session, type = "success", 
            title = "Data added", text = "Choose a plot type"
            )
    })
}
