# Description of this tab in markdown format
## xx_desc is loading to global env, please make sure it is unique
xx_desc <- 
"
#### Some Description of this data
- you should ...
    1. eg 1.
    2. eg 2.
- **Notice**: ...`this` ...


```
some code
```
"

## UI
df_rawUI <- function(id, description = xx_desc){
    ns <- NS(id)
    tagList(
        h2("Title for this kind of dataframe"),
        HTML(markdown::renderMarkdown(text = glue(description))),
        
        radioGroupButtons(
            inputId = ns("plot_source"), label = "Choose your plot file source:", 
            selected = "upload",
            choiceNames = c("Upload", "Example"), 
            choiceValues = c("upload", "eg"),
            justified = TRUE, status = "primary",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
        ),
        textInputGroup(textId = ns("df_path"), btnId = ns("upload"), title = "Specify your data path", label = "Upload"),
        column(width = 12, style = "padding-left: 0;",
               downloadButton(ns("down_config"), "Save"),
               actionButton(ns("to_task"),
                            label = "Add to task", 
                            icon("paper-plane"))
        ),
        rHandsontableOutput(ns("df")),
        fluidRow(id = ns("plot_options"),
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
        shinyjs::show("plot_options")
        sendSweetAlert(
            session = session, type = "success", 
            title = "Data added", text = "Choose a plot type"
            )
    })
}
