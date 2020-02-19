## UI
targetUI <- function(id){
    ns <- NS(id)
    tagList(
        tabBox(title = "TARGETS", id = ns("targets"),
               width = 12,
               target_tabUI(ns("upload"), subtab_name = "Upload targets", upload_text = "Choose target file"),
               target_tabUI(ns("example"), subtab_name = "Use example targets", upload_text = "You can't use this in this tab"),
               target_tabUI(ns("new"), subtab_name = "Start from empty", upload_text = "You can't use this in this tab")
        )
    )
}
## submodule UI
target_tabUI <- function(id, subtab_name, upload_text){
    ns <- NS(id)
    tabPanel(title = subtab_name,
             h2("Targets"),
             fluidRow(
                 column(3, 
                        fluidRow(
                            valueBox(width = 12,textOutput(ns("box_samples")), "Number of Samples", icon = icon("vials"))
                        ),
                        fluidRow(
                            valueBox(width = 12, textOutput(ns("box_ncol")), "Number of columns", icon = icon("columns"), color = "purple")
                        ),
                        fluidRow(
                            uiOutput(ns("box_missing_ui"))
                        ),
                        boxPlus("Missing files (first row is treated as column names)", width = 12,
                                selectInput(ns("column_check"), "Choose a column to check files:",
                                            choices = "Disabled before uploading targets"),
                                verbatimTextOutput(ns("missing_files"))
                        ),
                 ), 
                 column(9,
                          fileInput(
                          ns("target_file"), upload_text,
                          multiple = FALSE,
                          accept = c("text/tsv",
                                     "text/comma-separated-values,text/plain",
                                     ".tsv", ".txt"),
                          placeholder = if (str_detect(ns(""), "upload")) "Choose your target file path" else "Disabled"
                                    ),
                        radioButtons(
                          ns("but_pese"),
                          label = if (!str_detect(ns(""), "upload")) "Choose one:" else "Disabled",
                          choices = c("Pair-End","Single-End"),
                          inline = TRUE
                                    ),
                        fluidRow(
                          downloadButton(ns("down_targets"), "Save"),
                          actionButton(ns("to_task_target"),
                                       label = if (str_detect(ns(""), "upload")) "Add to task" else "Disabled", 
                                       icon("paper-plane"), 
                                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                          ),
                        h4("Targets header"),
                        p("You can edit your target file header below. All lines should start with #, a line of # <CMP> xxx is required."),
                        aceEditor(
                          outputId = ns("ace_target_header"),
                          selectionId = "selection",
                          theme = "Chrome",
                          value = if (!str_detect(ns(""), "upload")) ace_target_header_init else "",
                          placeholder = "Target header lines", height = "100px"
                        ),
                        p("You can edit your targets (metadata) below."),
                        p("Columns of 'FileName1', 'FileName2' are required for pair-end or 'FileName' for single-end. 'SampleName', 'Factor' are required for both."),
                        p("Columns names should be on the first row."),
                        rHandsontableOutput(ns("targets_df"))
                 )
             )
    )
}

ace_target_header_init <- 
"# Project ID: Arabidopsis - Pseudomonas alternative splicing study (SRA: SRP010938; PMID: 24098335)
# The following line(s) allow to specify the contrasts needed for comparative analyses, such as DEG identification. All possible comparisons can be specified with 'CMPset: ALL'.
# <CMP> CMPset1: M1-A1, M1-V1, A1-V1, M6-A6, M6-V6, A6-V6, M12-A12, M12-V12, A12-V12
# <CMP> CMPset2: ALL
"
## server
targetServer <- function(input, output, session, shared){
    callModule(targetMod, "upload", upload = TRUE, shared = shared)
    callModule(targetMod, "example", ifexample = TRUE, shared = shared)
    callModule(targetMod, "new", shared = shared)
}
## submodule server
targetMod <- function(input, output, session, upload = FALSE, ifexample=FALSE, shared){
  ns <- session$ns
  disable('column_check')
  disable("down_targets")
  if (upload) disable("but_pese") else {disable("target_file"); disable("to_task_target")}
  flags <- reactiveValues(new_file = FALSE)
  observeEvent(input$but_pese,{
    flags$pe <- if (!input$but_pese == "Pair-End") FALSE else TRUE
    flags$force_load <- TRUE
  })
  observeEvent(input$target_file, {
    flags$new_file <- TRUE
  })
    
  observeEvent({input$but_pese; input$targets_df; input$target_file; input$column_check}, {
    if ((!is.null(input$target_file)) | (!upload)){
      t.df <- hot_target(targets_df = input$targets_df, targets_p = input$target_file$datapath,
                         ifexample = ifexample, pe = flags$pe,
                         force_load = flags$force_load, new_file = flags$new_file)
      t.df.check <- t.df[-1, ] %>% as.data.frame()
      
      output$box_samples <- renderText({nrow(t.df.check)})
      output$box_ncol <- renderText({ncol(t.df.check)})
      if (upload) {
        updateSelectInput(session, "column_check", choices = names(t.df), selected = input$column_check)
        not_missing_index <- sapply(as.character(t.df.check[[input$column_check]]), file.exists)
        missing_names <- t.df.check[[input$column_check]][!not_missing_index]
        output$missing_files <-  renderPrint({cat(paste0(row.names(t.df.check)[!not_missing_index], " ", missing_names, collapse = '\n'))})
        box_missing_val <- "NA"
        if (input$column_check %in% names(t.df.check)){
          box_missing_val <- as.character(nrow(t.df.check) - sum(not_missing_index))
        }
        output$box_missing <- renderText({box_missing_val})
        output$box_missing_ui <- renderUI({
          valueBox(width = 12,
                   textOutput(ns("box_missing")),
                   "Missing files in selected column",
                   icon = if (box_missing_val %in% c("NA", "0")) icon("check") else icon("times"),
                   color = if (box_missing_val %in% c("NA", "0")) 'green' else 'red'
          )
        })
      }
      
      output$targets_df <- renderRHandsontable({
        rhandsontable(t.df, selectCallback = TRUE, useTypes = FALSE) %>%
          hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      })
    } else {
      # before uploading table
      output$box_samples <- renderText({"NA"})
      output$box_ncol <- renderText({"NA"})
    }
    # disale some ui
    if (!is.null(input$target_file)) {
      enable("down_targets")
      enable('column_check')
    } 
    if (!upload) {enable("down_targets")}
    
    flags$force_load <- FALSE
    flags$new_file <- FALSE
    return(flags)
  })
  
  output$down_targets <- downloadHandler(
    filename <- function() {
      "targets.txt"
    },
    content <- function(filename) {
      write.table(hot_to_r(input$targets_df), filename, sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
    })
  observeEvent(input$targets_df, {
    if (str_detect(ns(""), "upload")) shared$target_upload <- hot_to_r(input$targets_df)
  })
  
}


hot_target <- function(targets_df, targets_p=NULL, ifexample=FALSE, pe=TRUE, force_load = FALSE, new_file = FALSE){
    if (is.null(targets_df)) df <- load_target(targets_p, ifexample, pe) else df <- hot_to_r(targets_df)
    if (force_load | new_file) df <- load_target(targets_p, ifexample, pe)
    names(df) <- paste0("V", 1:ncol(df))
    return(df)
}

load_target <- function(targets_p, ifexample, pe) {
    empty <- FALSE; if ((!ifexample) & is.null(targets_p)) empty <- TRUE
    if (is.null(targets_p)) {if (pe) {targets_p <- 'inst/extdata/targetsPE.txt'} else {targets_p <- 'inst/extdata/targets.txt'}}
    df <- read.csv(targets_p, sep = '\t', comment.char = "#", stringsAsFactors = FALSE, header = FALSE)
    if (empty) df[2:nrow(df), ] <- ""
    return(df)
}

