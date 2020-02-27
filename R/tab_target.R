## UI
targetUI <- function(id){
    ns <- NS(id)
    tagList(
        tabBox(title = "TARGETS", id = ns("targets"),
               width = 12,
               target_tabUI(ns("upload"))
        )
    )
}
## submodule target UI
target_tabUI <- function(id){
    ns <- NS(id)
    tabPanel(title = "Upload targets",
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
                      )
               ), 
               column(9,
                      radioGroupButtons(
                          inputId = ns("target_source"), label = "Choose target source:", 
                          selected = "upload",
                          choiceNames = c("Upload", "Example PE", "Example SE"), 
                          choiceValues = c("upload", "pe", "se"),
                          justified = TRUE, status = "primary",
                          checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
                      ),
                      fileInput(
                          ns("target_upload"), "If upload, choose your target file here:",
                          multiple = FALSE,
                          accept = c(".tsv", ".txt"),
                          placeholder = "Choose your target file path"
                      ),
                      fluidRow(
                        downloadButton(ns("down_targets"), "Save"),
                        actionButton(ns("to_task_target"),
                                     label = "Add to task", 
                                     icon("paper-plane"), 
                                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      ),
                      h4("Targets header"),
                      p("You can edit your target file header below. All lines should start with #, a line of # <CMP> xxx is required."),
                      aceEditor(
                        outputId = ns("ace_target_header"),
                        selectionId = "selection",
                        theme = "Chrome",
                        value = "",
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
    callModule(targetMod, "upload", shared = shared)
}
## submodule server
targetMod <- function(input, output, session, shared){
    ns <- session$ns
    # some reactive values to pass around observe
    choice_old <- reactiveVal("upload")
    targets_p_old <- reactiveVal("")
    t.df <- reactiveVal(data.frame())
    # force to reload targets if change target_source or file uploaded
    observeEvent(c(input$target_source, input$target_upload), {# only c work here, dont know why
        t.df(
            hot_target(targets_df = input$targets_df,
                       targets_p = input$target_upload$datapath, 
                       targets_p_old = targets_p_old(),
                       choice = input$target_source,
                       choice_old = choice_old())
        )
        output$targets_df <- renderRHandsontable({
            rhandsontable(t.df(), selectCallback = TRUE, useTypes = FALSE) %>%
                hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        })
        if (!is.null(input$targets_df)) df <- hot_to_r(input$targets_df)
        if (choice_old() != input$target_source) choice_old(input$target_source)
        if (!is.null(input$target_upload$datapath)) targets_p_old(input$target_upload$datapath)
        if (choice_old() != "upload") disable("target_upload") else enable("target_upload")
    })
    # store interactively modified table, update left check bar
    observeEvent({input$targets_df; input$column_check}, {
        if (!is.null(input$targets_df)) t.df(hot_to_r(input$targets_df))
        output$targets_df <- renderRHandsontable({
            rhandsontable(t.df(), selectCallback = TRUE, useTypes = FALSE) %>%
                hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        })
        
        t.df.check <- t.df()[-1, ] %>% as.data.frame()
        output$box_samples <- renderText({nrow(t.df.check)})
        output$box_ncol <- renderText({ncol(t.df.check)})
        updateSelectInput(session, "column_check", choices = names(t.df()), selected = input$column_check)
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
    })
    # download button
    output$down_targets <- downloadHandler(
      filename <- function() {
        "targets.txt"
      },
      content <- function(filename) {
        write.table(hot_to_r(input$targets_df), filename, sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
      })
    observeEvent(input$targets_df, {
      if (str_detect(ns(""), "upload")) shared$targets$df <- hot_to_r(input$targets_df)
    })
    observeEvent(input$to_task_target, {
      shared$to_task$target <- input$to_task_target
      print(shared$to_task$target)
    })

}


hot_target <- function(targets_df, targets_p=NULL, targets_p_old=NULL, choice, choice_old){

    targets_p <- switch(choice,
                        "upload" = targets_p,
                        "pe" = "inst/extdata/targetsPE.txt",
                        "se" = "inst/extdata/targets.txt"
                        )
    if (is.null(targets_p)) return(data.frame(matrix("", 8,8), stringsAsFactors = FALSE))
    if ((choice != choice_old) | (targets_p != targets_p_old)) {
        df.t <- read.csv(targets_p, sep = '\t', comment.char = "#", stringsAsFactors = FALSE, header = FALSE)
    } 
    names(df.t) <- paste0("X", 1:ncol(df.t))
    return(df.t)
}


