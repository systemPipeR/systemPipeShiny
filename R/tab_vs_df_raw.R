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
        actionButton(inputId = ns("validate"), label = "Start with this tab"),
        div(
            id = ns("tab_main"),
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
    )
}

## server
df_rawServer <- function(input, output, session, shared){
    df_init <- data.frame(matrix("", 8,8), stringsAsFactors = FALSE)
    ns <- session$ns
    shinyjs::hide(id = "plot_options")
    # start the tab
    shinyjs::hide(id = "tab_main")
    observeEvent(input$validate, {
        if (shinyCheckSpace(
            session = session#, 
            # cran_pkg = "pkg-1",
            # bioc_pkg = c("pkg-1", "pkg-2"), 
            # github = "haha/pkg-3"
        )) {
            shinyjs::show(id = "tab_main")
        }
    })
    # update table
    observeEvent(c(input$plot_source, input$df_path) , {
        if (input$plot_source == "upload" & input$df_path == "") shinyjs::hide("df") else shinyjs::show("df")
    })
  
    output$df <- renderRHandsontable({
      rhandsontable(df_init, selectCallback = TRUE, useTypes = FALSE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    })
#     
#     onclick("to_task", shinyjs::show(id = "plot_options")) 
#     check_results <- T
#     observeEvent(input$to_task, {
#         shinyjs::show("plot_options")
#         sendSweetAlert(
#             session = session, type = "success", 
#             title = "Data added", text = "Choose a plot type"
#             )
#         shared$count$df <- t.df()
#         writeLines(apply(shared$count$df, 1, paste, collapse = "\t"), shared$count$file)
#     })
#     observeEvent(c(input$plot_source, input$df_upload$datapath), ignoreInit = TRUE, {# only c work here, dont know why
#       if (selected_flag() == TRUE) { 
#         confirmSweetAlert(
#           session,inputId = "sweet_changecount_confirm", 
#           title = "Do you want to change data frame Source?", 
#           text = "If you change the data frame source or load a new file, Data frame data will be reset in this tab and 'Task' tab. You will LOSE unsaved data", type = "warning"
#         )
#       } else {
#         selected_flag(TRUE) 
#       }
#     })
#     # update df
#     observeEvent(input$sweet_changecount_confirm, ignoreNULL = TRUE, {
#       if (isTRUE(input$sweet_changecount_confirm)) {
#         t.df(
#           hot_count(count_df = input$df,
#                 count_p = input$df_upload$datapath, 
#                 count_p_old = count_p_old(), 
#                 choice = input$plot_source, 
#                 choice_old = selected_old() 
#           )
#         )
#         print("test")
#       }
#     })
# }
# 
# # load raw count file
# hot_count <- function(count_df, count_p=NULL, count_p_old=NULL, choice, choice_old){
#   count_p <- switch(choice,
#                     "upload" = count_p,
#                     "eg" = "inst/extdata/countDFeByg.xls"
#   )
#   if (is.null(count_p)) return("")
#   if ((choice != choice_old) | (count_p != count_p_old)) {
#     df.t <- as.matrix(read.table(count_p), stringsAsFactors = FALSE, header = FALSE)
#     } 
#   #names(df.t) <- paste0("X", 1:ncol(df.t))
#   return(df.t)
}

