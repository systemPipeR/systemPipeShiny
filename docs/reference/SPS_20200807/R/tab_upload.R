## UI
uploadUI <- function(id){
  ns <- NS(id)
  tagList(
    h2("Upload Count Table"),
    boxPlus(title = "Upload count table", width = 12, closable = FALSE,
            fileInput(ns("count_targets"), "Upload a targets file",
                      multiple = F,
                      accept = ".txt"),
            fileInput(ns("count_input"), "Upload a count table",
                      multiple = F,
                      accept = c(".xls")),
            checkboxInput(ns("demo"), "Use Sample Data"),
            verbatimTextOutput(ns("uncheck"))
    )
  )
}

## server
uploadServer <- function(input, output, session, shared = shared){
  observe({
    if (all(!is.null(input$count_input), !is.null(input$count_targets), !input$demo)){
      count_list <<- load_count(targets_file = input$count_targets$datapath, count_table = input$count_input$datapath)
      output$count_table <- renderRHandsontable({rhandsontable(count_df)})
      shared$count_table <- count_list$countDF
      shared$count_list <- count_list
    } else if (input$demo == T) {
      count_list <<- load_count(targets_file = 'data/targetsPE.txt', count_table = 'data/countDFeByg.xls')
      output$count_table <- renderRHandsontable({rhandsontable(count_df)})
      shared$count_table <- count_list$countDF
      shared$count_list <- count_list
    }
  })
}
