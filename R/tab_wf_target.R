
## submodule target UI

#' @importFrom shinyAce aceEditor
#' @importFrom shinydashboard valueBox
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyWidgets radioGroupButtons
#' @noRd
wf_targetUI <- function(id){
    ns <- NS(id)
    tagList(
        # actionButton(ns("set"), "set"),
        div(
            id = "wf_targets_displayed",
            style = "display:none",
            tabTitle("Targets"),
            renderDesc(id = ns("desc"),
            '
            #### Targets file
            The targets (metadata) file defines all input files\' path and other sample
            information of an analysis workflow. Read "Structure of targets file"
            section on
            [our website](https://systempipe.org/spr/systempiper/gettingstarted/#structure-of-targets-file)
            to better undertand the structure of this file.
            This file is similar to the `colData` slot in an `SummarizedExperiment`
            object which stores sample ID and other meta information.

            #### Using template workflow targets
            If the workflow environment has been set up correctly in the previous step,
            by default SPS loads the targets file that is been used in the workflow file.

            Note: most template workflows comes with two targets files: *targets.txt*
            and *targetsPE.txt*, one for single-end one for pair-end. Here SPS loads
            the one that is been used in the workflow R markdown file, in most cases
            the "*targetsPE.txt*". If you want to use the single-end targets file,
            you can upload it. **SPS will replace the default file with this newly uploaded file.**

            #### Using existing targets
            If you have selected to use an existing workflow in the previous step,
            it is required that you manually select (upload) the targets file you want to use.
            SPS cannot guess the file name of your existing targets file name.

            #### Sample statistics
            The boxes on the left display targets sample statistics and if
            you select a column from the left side dropdown menu, it will also
            help you to check for file existence of the column you choose.

            The file existence function assumes the workflow environment directory as
            the root to check for files. You could specify another place as root by
            typing the path in the input box.

            #### Check the format
            "Add to task" button can help you to check if you have the required
            targets file format, i.e. column names, headers. If anything is missing,
            you will see the instruction in the pop-up box.
            '),
            spsHr(),
            boxPlus(
                title = "Confirm to use this targets file",
                closable = FALSE, collapsible = TRUE,
                width = 12,
                class = "center-block",
                HTML(
                "
                <ul>
                  <li>
                    When you have finished editing your targets table and
                    targets header below, clicking on the <b>Add to task</b>
                    will check the targets format for you.
                  </li>
                  <li>
                    If everthing is correct, this targets file will be write
                    back to the workflow environment folder and will be used in step <b>5</b>.
                  </li>
                  <li>
                    You can also <b>Save</b> it as
                    an individual file from the browser.</p>
                  </li>
                </ul>
                "),
                div(class = "text-danger",
                    tags$ul(
                        id = ns("warn_other"),
                        HTML(
                            "<li>Upon passing 'Add to task' checks, the original targets file
                            in the workflow folder will be overwritten. Rename/back up it if you do
                            not wish it to be replaced.</li>"
                        ))
                ),
                fluidRow(
                    style = "padding-left: 40%",
                    actionButton(ns("to_task_target"),
                                 label = "Add to task",
                                 icon("paper-plane")) %>%
                        bsHoverPopover(
                            "Check targets format and add to workflow task",
                            "When you have finished editing your targets table
                            and targets header below, clicking on the Add to
                            task will check the targets format for you",
                            "bottom"
                        ),
                    downloadButton(ns("down_targets"), "Save") %>%
                        bsHoverPopover(
                            "Download current targets",
                            "You can download current targets file from the
                            browser. It is the best to use 'Add to task'
                            to check the format first then come back to this
                            page to download the file.",
                            "bottom"
                        )
                )
            ),
            fluidRow(
                column(3,
                       fluidRow(
                           shinydashboard::valueBox(
                               width = 12,
                               textOutput(ns("box_samples")),
                               "Number of Samples",
                               icon = icon("vials"))
                       ),
                       fluidRow(
                           shinydashboard::valueBox(
                               width = 12,
                               textOutput(ns("box_ncol")),
                               "Number of columns",
                               icon = icon("columns"),
                               color = "purple")
                       ),
                       fluidRow(
                           uiOutput(ns("box_missing_ui"))
                       ),
                       shinydashboardPlus::boxPlus(
                           "Missing files (first row is treated as column names)",
                           width = 12,
                           p("Write down the root path if you are not using workflow environment default root to store data."),
                           textInput(ns("target_data_path"),
                                              label = "Add path prefix",
                                              placeholder = "long path"),
                           selectInput(ns("column_check"),
                                       "Choose a column to check files:",
                                       choices = "Disabled before uploading targets"),
                           verbatimTextOutput(ns("missing_files"))
                       ),
                       tags$style(
                           glue(
                               '#@{ns("missing_files")}@{
                            height: 400px;
                          }',
                               .open = "@{", .close = "}@"
                           )
                       )
                ),
                column(9,
                       shinyWidgets::radioGroupButtons(
                           inputId = ns("target_source"),
                           label = "Choose target source:",
                           selected = "default",
                           choiceNames = c("Default", "Upload"),
                           choiceValues = c("default", "upload"),
                           justified = TRUE, status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                            no = icon(""))
                       ),
                       fileInput(ns("targets_upload"),
                                   "If upload, choose your target file here:",
                                   multiple = FALSE),
                       h4("Targets header"),
                       p("You can edit your target file header below.
                     All lines should start with #, a line of # <CMP>
                     xxx is required."),
                       shinyAce::aceEditor(
                           outputId = ns("ace_target_header"),
                           theme = "Chrome",
                           value = "",
                           placeholder = "Target header lines", height = "100px"
                       ),
                       p("You can edit your targets (metadata) below."),
                       p("Columns of 'FileName1', 'FileName2' are required
                     for pair-end or 'FileName' for single-end.
                     'SampleName', 'Factor' are required for both."),
                       p("Columns names should be on the first row."),
                       rhandsontable::rHandsontableOutput(ns("targets_df"),
                                                          height = "500px")
                )
            )
        ),
        div(
            id = "wf_targets_disable",
            h3("Generate a workflow environment at Step 1 first",
               style = "text-center text-warning")
        )
    )
}

## submodule server
#' @importFrom shinyAce updateAceEditor
#' @importFrom shinytoastr toastr_info
#' @importFrom shinyWidgets confirmSweetAlert updateRadioGroupButtons
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom dplyr as_tibble
#' @importFrom vroom vroom cols
#' @importFrom shinyAce is.empty
#' @importFrom shinyjs disable enable
#' @noRd
wf_targetServer <- function(id, shared){
    module <- function(input, output, session){
        # set up
        ns <- session$ns
        ace_target_header_init <- ""
        data_init <- data.frame(matrix("", 8,8), stringsAsFactors = FALSE) %>%
            dplyr::as_tibble()
        observeEvent(input$target_source, {
            shinyjs::toggleElement("targets_upload", anim = TRUE, condition = input$target_source == "upload")
        })
        # some reactive values to pass around observe
        selected_old <- reactiveVal("upload")
        selected_flag <- reactiveVal(TRUE)
        targets_p_old <- reactiveVal("")
        t.df <- reactiveVal(data_init)
        # load table is wf env is ready
        #######
        # observeEvent(input$set, {
        #     shared$wf$flags$env_ready = T
        #     shared$wf$targets_path = "upload_required"
        #     shared$wf$env_path = normalizePath("riboseq")
        #     shared$wf$env_option = "exist"
        # })
        #####
        observeEvent(c(shared$wf$flags$env_ready), {
            req(shared$wf$flags$env_ready)
            req(shared$wf$targets_path)
            t.df(shinyCatch(vroom::vroom(
                shared$wf$targets_path,  delim = "\t",
                comment = "#", altrep = FALSE,
                col_names = FALSE, col_types = vroom::cols()
            ),
            blocking_level = "error"))
            updateAceEditor(
                session, "ace_target_header",
                value = shinyCatch(
                    readLines(shared$wf$targets_path, warn = FALSE) %>%
                        .[str_detect(.,"^#")] %>% paste(collapse = "\n"),
                    blocking_level = "error"
                )
            )

        })
        # update table
        output$targets_df <- rhandsontable::renderRHandsontable({
            rhandsontable::rhandsontable(t.df(),
                                         selectCallback = TRUE,
                                         useTypes = FALSE) %>%
                rhandsontable::hot_context_menu(allowRowEdit = TRUE,
                                                allowColEdit = TRUE)
        })
        observeEvent(c(input$target_source, not_empty(input$targets_upload)),
                     ignoreInit = TRUE, ignoreNULL = TRUE, {
            req(not_empty(input$targets_upload))
            if (selected_flag() == TRUE) {
                shinyWidgets::confirmSweetAlert(
                    session,inputId = "sweet_changetarget_confirm",
                    title = "Do you want to change target Source?",
                    text = "If you change target source or load new file,
                            target data will be reset. You will LOSE unsaved data",
                    type = "warning"
                )
            } else {
                selected_flag(TRUE)
            }
        })
        observeEvent(input$sweet_changetarget_confirm, ignoreNULL = TRUE,{
            if (isTRUE(input$sweet_changetarget_confirm)) {
                # load target file
                t.df(shinyCatch({
                    targets_path <- if(input$target_source == "default") shared$wf$targets_path else input$targets_upload$datapath
                    if(is.null(targets_path)){
                        df <- data_init
                    } else {
                        df <- vroom::vroom(
                            targets_path,  delim = "\t",
                            comment = "#", n_max = 10000,
                            col_names = FALSE, col_types = vroom::cols()
                        )
                    }
                    if(is.null(df)) {warning("Can't read file, return empty"); df <- data_init}
                    names(df) <- paste0("X", seq_len(ncol(df)))
                    df
                }, blocking_level = "error"))
                # header
                header_lines <- ""
                if (!is.null(input$targets_upload$datapath)) {
                    header_lines <- readLines(input$targets_upload$datapath,
                                              warn = FALSE) %>%
                        .[str_detect(.,"^#")] %>% paste(collapse = "\n")
                    if (length(header_lines) == 0) header_lines <- ""
                    targets_p_old(input$targets_upload$datapath)
                }
                if (input$target_source != "upload")
                    header_lines <-
                    readLines(shared$wf$targets_path, warn = FALSE) %>%
                    .[str_detect(.,"^#")] %>% paste(collapse = "\n")
                shinyAce::updateAceEditor(
                    session,
                    editorId = "ace_target_header",
                    value = header_lines,
                )
                # other server end updates
                shinytoastr::toastr_info(
                    paste0("Changed target source to ",
                           input$target_source,
                           ". Target reset"),
                    closeButton = TRUE,
                    position = "bottom-right",
                    timeOut = 2000
                )
                shared$wf_flags$targets_ready = FALSE
                selected_old(input$target_source)
            } else {
                #if canceled alert
                shinyWidgets::updateRadioGroupButtons(
                    session, "target_source",
                    selected = selected_old(),
                    checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                     no = icon(""))
                )
                selected_flag(FALSE)
            }
        })
        # update left side long path
        observeEvent(shared$wf$env_path, {
            req(emptyIsFalse(shared$wf$env_path))
            updateTextInput(session, "target_data_path", value = file.path(shared$wf$env_path))
        })
        # left side checkers behaviors
        observeEvent({input$targets_df; input$column_check}, ignoreInit = TRUE, {
            req(t.df())
            if (!is.null(input$targets_df)) {
                t.df(rhandsontable::hot_to_r(input$targets_df))
            }
            output$targets_df <- rhandsontable::renderRHandsontable({
                rhandsontable::rhandsontable(t.df(),
                                             selectCallback = TRUE,
                                             useTypes = FALSE) %>%
                    rhandsontable::hot_context_menu(allowRowEdit = TRUE,
                                                    allowColEdit = TRUE)
            })

            t.df.check <- t.df()[-1, ] %>% as.data.frame()
            output$box_samples <- renderText({nrow(t.df.check)})
            output$box_ncol <- renderText({ncol(t.df.check)})
            updateSelectInput(
                session, "column_check",
                choices = names(t.df()),
                selected = input$column_check
            )
            long_path <- if (shinyAce::is.empty(input$target_data_path)) {"."
                } else input$target_data_path
            cheching_path <- file.path(
                long_path, as.character(t.df.check[[input$column_check]])
            )
            not_missing_index <- vapply(cheching_path,
                                        file.exists,
                                        logical(1))
            missing_names <- cheching_path[!not_missing_index]
            output$missing_files <-  renderPrint({
                cat(paste0(row.names(t.df.check)[!not_missing_index],
                           " ",
                           missing_names, collapse = '\n'))
            })
            box_missing_val <- "NA"
            if (input$column_check %in% names(t.df.check)) {
                box_missing_val <-
                    as.character(nrow(t.df.check) - sum(not_missing_index))
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
                "targetsPE.txt"
            },
            content <- function(filename) {
                writeLines(
                    c(isolate(input$ace_target_header),
                      apply(rhandsontable::hot_to_r(input$targets_df),
                            1, paste,
                            collapse = "\t")),
                    filename)
        })
        # add to task
        observeEvent(input$to_task_target, {
            # check col_names, header lines
            req(t.df())
            t.df(rhandsontable::hot_to_r(input$targets_df))
            header_lines <- isolate(input$ace_target_header)
            check_results <- check_target(col_names = t.df()[1, ],
                                          headerlines = header_lines)
            if (all(check_results)) {
                shinyCatch({
                    old_mtime <- file.mtime(shared$wf$targets_path)
                    # create back up folder
                    dir.create(file.path(shared$wf$env_path, "backup"), recursive = TRUE, showWarnings = FALSE)
                    # if targets file exists, back it up
                    if(file.exists(shared$wf$targets_path))
                        file.copy(
                            shared$wf$targets_path,
                            file.path(
                                shared$wf$env_path,
                                "backup",
                                paste0(
                                    glue("bk{Sys.time() %>% format('%Y%m%d%H%M%S')}"),
                                    basename(shared$wf$targets_path))
                            ),
                            overwrite = TRUE
                        )
                    # overwrite current file
                    writeLines(
                        c(header_lines, apply(t.df(), 1, paste, collapse = "\t")),
                        shared$wf$targets_path
                    )
                    if(identical(old_mtime, file.mtime(shared$wf$targets_path)))
                        stop("File ", shared$wf$targets_path, " can not be created or not modified")
                    shared$wf$targets_path <- normalizePath(shared$wf$targets_path)
                }, blocking_level = "error")
                shared$wf$flags$targets_ready <- isolate(shared$wf$flags$targets_ready) + 1
                shinyWidgets::confirmSweetAlert(
                    session = session,
                    inputId = ns("confirm_next"),
                    title = "Targets file setup done!",
                    closeOnClickOutside = FALSE,
                    html = TRUE,
                    type = "success",
                    text = HTML(glue(
                    "
                    <ul class='text-left'>
                      <li><b>The targets file is located at</b>: {shared$wf$targets_path}</li>
                    </ul>
                    <h3>Do you want to proceed to the next step?</h3>
                    "
                    ))
                )
            } else {
                sendSweetAlert(
                    session = session,
                    title = "Some requirements are missing",
                    text = tags$b(
                        HTML(paste0(
                            "<i class='fa fa-file'></i>Your target should have ",
                            names(check_results[check_results == FALSE]),
                            collapse = "<br>")
                        ),
                        style = "color: #FA5858;"
                    ),
                    html = TRUE,
                    type = "error"
                )
            }
        })
        # going to next tab
        observeEvent(input$confirm_next, {
            req(input$confirm_next)
            shinyjs::runjs("$('#wf-wf_panel-2-heading > h4').trigger('click');")
        })
    }
    # target checkers
    check_target <- function(col_names, headerlines) {
        checker1 <- function(col_names) {
            all(c("FileName1", "FileName2") %in% col_names ) |
            "FileName" %in% col_names
        }
        checker2 <- function(col_names)  "SampleName" %in% col_names
        checker3 <- function(col_names)  "Factor" %in% col_names
        checker4 <- function(headerlines) {
            any(str_detect(headerlines, "#\\s{0,}<CMP>"))}
        check_results <- vapply(c(checker1, checker2, checker3),
                                function(x) x(col_names),
                                logical(1)) %>%
            append(checker4(headerlines))
        names(check_results) <- c("both 'FileName1' 'FileName2' or 'FileName'",
                                  "SampleName", "Factor",
                                  "header with # &ltCMP&gt")
        return(check_results)
    }
    moduleServer(id, module)
}
