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
    df_init <- data.frame(matrix("", 8,8), stringsAsFactors = FALSE)
    ns <- session$ns
    shinyjs::hide(id = "plot_options")
    selected_old <- reactiveVal("upload")
    selected_flag <- reactiveVal(TRUE)
    count_p_old <- reactiveVal("")
    t.df <- reactiveVal(df_init)
    # update table
    # output$df <- renderRHandsontable({
    #     rhandsontable(
    #         # data.frame(matrix("", 8,8), stringsAsFactors = FALSE), selectCallback = TRUE, useTypes = FALSE) %>%
    #         #     hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    #         data.frame(t.df(), selectCallback = TRUE, useTypes = FALSE) %>%
    #           hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    #     )
    # })
    
    output$df <- renderRHandsontable({
      rhandsontable(t.df(), selectCallback = TRUE, useTypes = FALSE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    })
    
    onclick("to_task", shinyjs::show(id = "plot_options")) 
    check_results <- T
    observeEvent(input$to_task, {
      shared$count$file <- tempfile(pattern = "countDFeByg", fileext = ".xls")
      if (all(check_results)) {  
        sendSweetAlert(
            session = session, type = "success", 
            title = "Data added", text = "Choose a plot type"
            )
        shared$count$df <- t.df()
        writeLines(apply(shared$count$df, 1, paste, collapse = "\t"), shared$count$file)
      }
    })
    observeEvent(c(input$plot_source, input$df_upload$datapath), ignoreInit = TRUE, {# only c work here, dont know why
      if (selected_flag() == TRUE) { 
        confirmSweetAlert(
          session,inputId = "sweet_changecount_confirm", 
          title = "Do you want to change data frame Source?", 
          text = "If you change the data frame source or load a new file, Data frame data will be reset in this tab and 'Task' tab. You will LOSE unsaved data", type = "warning"
        )
      } else {
        selected_flag(TRUE) 
      }
    })
    # update df
    observeEvent(input$sweet_changecount_confirm, ignoreNULL = TRUE, {
      if (isTRUE(input$sweet_changecount_confirm)) {
        t.df(
          hot_count(count_df = input$df,
                count_p = input$df_upload$datapath, 
                count_p_old = count_p_old(), 
                choice = input$plot_source, 
                choice_old = selected_old() 
          )
        )
        print("test")
      }
    })
}

# load raw count file
hot_count <- function(count_df, count_p=NULL, count_p_old=NULL, choice, choice_old){
  count_p <- switch(choice,
                    "upload" = count_p,
                    "eg" = "inst/extdata/countDFeByg.xls"
  )
  if (is.null(count_p)) return("")
  if ((choice != choice_old) | (count_p != count_p_old)) {
    df.t <- as.matrix(read.table(count_p), stringsAsFactors = FALSE, header = FALSE)
    } 
  #names(df.t) <- paste0("X", 1:ncol(df.t))
  return(df.t)
}
