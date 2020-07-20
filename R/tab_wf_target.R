
## submodule target UI

#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinyAce aceEditor
#' @importFrom shinydashboard valueBox
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyWidgets radioGroupButtons
wf_targetUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Targets"),
        fluidRow(
            column(3,
                   fluidRow(
                       shinydashboard::valueBox(width = 12,textOutput(ns("box_samples")), "Number of Samples", icon = icon("vials"))
                   ),
                   fluidRow(
                       shinydashboard::valueBox(width = 12, textOutput(ns("box_ncol")), "Number of columns", icon = icon("columns"), color = "purple")
                   ),
                   fluidRow(
                       uiOutput(ns("box_missing_ui"))
                   ),
                   shinydashboardPlus::boxPlus("Missing files (first row is treated as column names)", width = 12,
                           p("Write down your path prefix if you use relative path in targets"),
                           clearableTextInput(ns("target_data_path"), label = "Add path prefix", placeholder = "long path"),
                           if (getOption('sps')$mode == 'server') {
                               h5("File checking is disabled on 'server' mode")
                           } else {
                               tagList(
                                   selectInput(ns("column_check"), "Choose a column to check files:",
                                               choices = "Disabled before uploading targets"),
                                   verbatimTextOutput(ns("missing_files"))
                               )
                           }

                   ),
                   tags$style(
                       glue(
                           '#@{ns("missing_files")}@{
                            height: 800px;
                          }',
                           .open = "@{", .close = "}@"
                       )
                   )
            ),
            column(9,
                   shinyWidgets::radioGroupButtons(
                       inputId = ns("target_source"), label = "Choose target source:",
                       selected = "upload",
                       choiceNames = c("Upload", "Example PE", "Example SE"),
                       choiceValues = c("upload", "pe", "se"),
                       justified = TRUE, status = "primary",
                       checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
                   ),
                   dynamicFile(ns("target_upload"),
                               "If upload, choose your target file here:",
                               multiple = FALSE),
                   column(12, style = "padding: 0;",
                          downloadButton(ns("down_targets"), "Save"),
                          actionButton(ns("to_task_target"),
                                       label = "Add to task",
                                       icon("paper-plane"))
                   ),
                   h4("Targets header"),
                   p("You can edit your target file header below. All lines should start with #, a line of # <CMP> xxx is required."),
                   shinyAce::aceEditor(
                       outputId = ns("ace_target_header"),
                       theme = "Chrome",
                       value = "",
                       placeholder = "Target header lines", height = "100px"
                   ),
                   p("You can edit your targets (metadata) below."),
                   p("Columns of 'FileName1', 'FileName2' are required for pair-end or 'FileName' for single-end. 'SampleName', 'Factor' are required for both."),
                   p("Columns names should be on the first row."),
                   rhandsontable::rHandsontableOutput(ns("targets_df"), height = "800px")
            )

        )
    )
}

## submodule server
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_context_menu hot_to_r
#' @importFrom shinyAce updateAceEditor
#' @importFrom shinytoastr toastr_info
#' @importFrom shinyWidgets confirmSweetAlert updateRadioGroupButtons sendSweetAlert
#' @importFrom tibble as_tibble
#' @importFrom vroom vroom cols
#' @importFrom shinyAce is.empty
#' @importFrom shinyjs disable enable
wf_targetServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        ace_target_header_init <-
            "# Project ID: Arabidopsis - Pseudomonas alternative splicing study (SRA: SRP010938; PMID: 24098335)
    # The following line(s) allow to specify the contrasts needed for comparative analyses, such as DEG identification. All possible comparisons can be specified with 'CMPset: ALL'.
    # <CMP> CMPset1: M1-A1, M1-V1, A1-V1, M6-A6, M6-V6, A6-V6, M12-A12, M12-V12, A12-V12
    # <CMP> CMPset2: ALL"
        df_init <- data.frame(matrix("", 8,8), stringsAsFactors = FALSE) %>% tibble::as_tibble()
        ns <- session$ns
        # some reactive values to pass around observe
        selected_old <- reactiveVal("upload")
        selected_flag <- reactiveVal(TRUE)
        targets_p_old <- reactiveVal("")
        t.df <- reactiveVal(df_init)
        target_upload <- dynamicFileServer(input, session, id = "target_upload")
        # update table
        output$targets_df <- rhandsontable::renderRHandsontable({
            rhandsontable::rhandsontable(t.df(), selectCallback = TRUE, useTypes = FALSE) %>%
                rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        })

        observeEvent(c(input$target_source, not_empty(target_upload())), ignoreInit = TRUE, ignoreNULL = TRUE, {# only c work here, dont know why
            if (selected_flag() == TRUE) {
                shinyWidgets::confirmSweetAlert(
                    session,inputId = "sweet_changetarget_confirm",
                    title = "Do you want to change target Source?",
                    text = "If you change target source or load new file, target data will be reset in this tab and 'Task' tab. You will LOSE unsaved data", type = "warning"
                )
            } else {
                selected_flag(TRUE)
            }
        })
        observeEvent(input$sweet_changetarget_confirm, ignoreNULL = TRUE,{
            if (isTRUE(input$sweet_changetarget_confirm)) {
                # update df
                t.df(
                    hot_target(targets_df = input$targets_df,
                               targets_p = target_upload()$datapath,
                               targets_p_old = targets_p_old(),
                               choice = input$target_source,
                               choice_old = selected_old(),
                               df_init = df_init)
                )
                # header
                header_lines <- ""
                if (!is.null(target_upload()$datapath)) {
                    header_lines <- readLines(target_upload()$datapath, warn = FALSE) %>% .[str_detect(.,"^#")] %>% paste(collapse = "\n")
                    if (length(header_lines) == 0) header_lines <- ""
                    targets_p_old(target_upload()$datapath)
                }
                if (input$target_source != "upload") header_lines <- ace_target_header_init
                shinyAce::updateAceEditor(session, editorId = "ace_target_header", value = header_lines)
                # other server end updates
                shinytoastr::toastr_info(paste0("Changed target source to ", input$target_source, ". Target reset"),
                            closeButton = TRUE, position = "bottom-right", timeOut = 2000)
                shared$wf_flags$targets_ready = FALSE
                if (input$target_source != "upload") shinyjs::disable("target_upload") else shinyjs::enable("target_upload")
                selected_old(input$target_source)
            } else {
                #if cancelled alert
                shinyWidgets::updateRadioGroupButtons(session, "target_source", selected = selected_old(),
                                        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
                )
                selected_flag(FALSE)
            }
        })
        # left side checkers behaviors
        observeEvent({input$targets_df; input$column_check}, {
            if (!is.null(input$targets_df)) {
                t.df(rhandsontable::hot_to_r(input$targets_df))
            }
            output$targets_df <- rhandsontable::renderRHandsontable({
                rhandsontable::rhandsontable(t.df(), selectCallback = TRUE, useTypes = FALSE) %>%
                    rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
            })

            t.df.check <- t.df()[-1, ] %>% as.data.frame()
            output$box_samples <- renderText({nrow(t.df.check)})
            output$box_ncol <- renderText({ncol(t.df.check)})
            updateSelectInput(session, "column_check", choices = names(t.df()), selected = input$column_check)
            long_path <- if (shinyAce::is.empty(input$target_data_path)) "." else input$target_data_path
            cheching_path <- file.path(long_path, as.character(t.df.check[[input$column_check]]))
            not_missing_index <- sapply(cheching_path, file.exists)
            missing_names <- cheching_path[!not_missing_index]
            output$missing_files <-  renderPrint({cat(paste0(row.names(t.df.check)[!not_missing_index], " ", missing_names, collapse = '\n'))})
            box_missing_val <- "NA"
            if (input$column_check %in% names(t.df.check)) {
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
                writeLines(c(isolate(input$ace_target_header), apply(rhandsontable::hot_to_r(input$targets_df), 1, paste, collapse = "\t")), filename)
            })
        # add to task
        observeEvent(input$to_task_target, {
            shared$targets$file <- tempfile(pattern = "target", fileext = ".txt")
            # check col_names, header lines
            header_lines <- isolate(input$ace_target_header)
            check_results <- check_target(col_names = t.df()[1, ], headerlines = header_lines)
            if (all(check_results)) {
                shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "Added to Task",
                    text = "All target check passed, target added to task\n You can see workflow status by clicking top right",
                    type = "success"
                )
                shared$targets$df <- t.df()
                writeLines(c(header_lines, apply(shared$targets$df, 1, paste, collapse = "\t")), shared$targets$file)
                shared$wf_flags$targets_ready = TRUE
            } else {
                sendSweetAlert(
                    session = session,
                    title = "Some requirements are missing",
                    text = tags$b(
                        HTML(paste0("<i class='fa fa-file'></i>Your target should have ", names(check_results[check_results == FALSE]), collapse = "<br>")),
                        style = "color: #FA5858;"
                    ),
                    html = TRUE,
                    type = "error"
                )
            }
        })
    }
    # load target file
    hot_target <- function(targets_df, targets_p=NULL, targets_p_old=NULL, choice, choice_old, df_init){
        targets_p <- switch(choice,
                            "upload" = targets_p,
                            "pe" = "inst/extdata/targetsPE.txt",
                            "se" = "inst/extdata/targets.txt"
        )
        if (is.null(targets_p)) return(df_init)
        if ((choice != choice_old) | (targets_p != targets_p_old)) {
            df.t <- shinyCatch(vroom::vroom(
                targets_p,  delim = "\t",
                comment = "#", n_max = 10000,
                col_names = FALSE, col_types = vroom::cols()
            ))
            if(is.null(df.t)){warning("Can't read file, return empty"); return(df_init)}
        }
        names(df.t) <- paste0("X", 1:ncol(df.t))
        return(df.t)
    }

    # target checkers
    check_target <- function(col_names, headerlines) {
        checker1 <- function(col_names) all(c("FileName1", "FileName2") %in% col_names ) | "FileName" %in% col_names
        checker2 <- function(col_names)  "SampleName" %in% col_names
        checker3 <- function(col_names)  "Factor" %in% col_names
        checker4 <- function(headerlines) any(str_detect(headerlines, "#.?<CMP>"))
        check_results <- sapply(c(checker1, checker2, checker3), function(x) x(col_names)) %>%
            append(checker4(headerlines))
        names(check_results) <- c("both 'FileName1' 'FileName2' or 'FileName'", "SampleName", "Factor", "header with # &ltCMP&gt")
        return(check_results)
    }
    moduleServer(id, module)
}
