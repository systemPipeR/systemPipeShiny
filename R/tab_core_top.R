## UI

#' @importFrom pushbar pushbar_deps pushbar
#' @noRd
core_topUI <- function(id){
    ns <- NS(id)
    init <-
"print('Ready to run a workflow')"
    tagList(
        pushbar::pushbar_deps(),
        pushbar::pushbar(
            id = ns("wf_push"),
            from = "top",
            style= "background:#ECF0F5;padding:2%;min-height:100%; overflow:auto;",
            tagList(
                div(class = "wf-bg",
                    tags$img(src = 'sps/img/spr.png')
                ),
                fluidRow(
                    div(style = " text-align: center;",
                        shinyWidgets::actionBttn(ns("close_push"),
                                                 style = "simple",
                                                 label = "Close this workflow session",
                                                 icon = icon("times"),
                                                 color = "danger",
                                                 size = "sm")) %>%
                        bsHoverPopover(
                            "Close this session",
                            "Clicking here will set your working directory back
                            to the app directory and may cause the workflow
                            fail to run.",
                            "bottom"
                        )
                ),
                fluidRow(
                    boxPlus(
                        width = 6, title = "Source Code",
                        closable = FALSE, collapsible = TRUE,
                        status = "primary", solidHeader = TRUE,
                        footer = tags$p("There is about one second delay sending code from UI to
                                  backend. Wait a second after editing code before and hitting 'Evaluate'."),
                        aceEditor(
                            ns("code"), mode = "r", height = "200px", value = init,
                            autoComplete = "live",
                            autoCompleters = c("static", "text"),
                            autoCompleteList = getNamespaceExports("systemPipeR") %>%
                                {.[!str_detect(., "^\\.")]} %>%
                                {paste0("systemPipeR::", .)} %>%
                                list(systemPipeR = .)),
                        actionButton(ns("eval"), "Evaluate")
                    ),
                    boxPlus(
                        width = 6, title = "Running Logs",
                        status = "success", solidHeader = TRUE,
                        closable = FALSE, collapsible = TRUE,
                        footer = tagList(
                            p("This panel always displays the most recent modified log in `.SYSproject` folder, updates every 10s."),
                            div(
                                style="height:5px; width:100%",
                                shinyWidgets::progressBar(
                                    ns("update_log"), value = 0,
                                    status = "primary", striped = TRUE,
                                    size = "xs"
                                )
                            )
                        ),
                        verbatimTextOutput(ns("logs")) %>%
                            {.$attribs[['style']] <- "scroll: auto; height: 240px; margin:0;"; .}
                    )

                ),
                fluidRow(
                    boxPlus(
                        width = 6, title = "Output",
                        status = "primary", solidHeader = TRUE,
                        closable = FALSE, collapsible = TRUE,
                        verbatimTextOutput(ns("output")) %>%
                            {.$attribs[['style']] <- "scroll: auto; height: 200px; margin:0;"; .}
                    ),
                    boxPlus(
                        width = 6, title = "Error and Warnings",
                        closable = FALSE, collapsible = TRUE,
                        status = "danger", solidHeader = TRUE,
                        class = "text-danger",
                        verbatimTextOutput(ns("e_and_w")) %>%
                            {
                                .$attribs[['style']] <- "scroll: auto; height: 200px; margin:0;"
                                .$attribs[['class']] <- paste(.$attribs[['class']],"text-danger")
                                .
                            }
                    )
                )
            )
        )
    )
}

## server

#' @importFrom pushbar setup_pushbar pushbar_open pushbar_close
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyjqui orderInput
#' @importFrom shinyWidgets actionBttn confirmSweetAlert
#' @importFrom shinyjs toggleState
#' @importFrom fs dir_ls
#' @noRd
core_topServer <- function(id, shared){
    module <- function(input, output, session){
        pushbar::setup_pushbar(blur = TRUE, overlay = TRUE)
        ns <- session$ns
        # close
        observeEvent(input$close_push, {
            pushbar::pushbar_close()
            setwd(shared$wf$wd_old)
            shared$wf$wf_session_open <- FALSE
            print(getwd())
        })
        observeEvent(shared$wf$wf_session_open, {
            req(shared$wf$wf_session_open)
            updateAceEditor(
                session = session, editorId = "code",
                value = glue(
                '
                sysargslist <- systemPipeR::initWF(script="{shared$wf$wf_path}", overwrite = TRUE)
                sysargslist <- systemPipeR::configWF(x=sysargslist)
                sysargslist <- systemPipeR::runWF(sysargslist = sysargslist, steps = "ALL")
                sysargslist
                ')
            )
        }, ignoreInit = TRUE)
        ## run code
        err <- reactiveVal(NULL)
        res <- reactiveVal(NULL)
        observeEvent(input$eval, {
            err(NULL); res(NULL)
            on.exit(shinyjs::enable("eval"))
            shinyjs::disable("eval")
            shinyjs::html("e_and_w", html = "")
            req(input$code)
            res(tryCatch(
                withCallingHandlers(
                    capture.output(eval(parse(text = isolate(input$code)))),
                    warning = function(w){
                        shinyjs::html("e_and_w", html = paste0("WARNING: ", w$message, "\n"), add = TRUE)
                    }
                ),
                error = function(e){
                    shinyjs::html("e_and_w", html = paste0("ERROR: ", e$message, "\n"), add = TRUE)
                    err(1)
                    return("Fail")
                }
            ))
            if(is.null(err())){
                toastr_success(
                    "Check Output panel for results",
                    title = "Workflow running successful",
                    position = "bottom-right",
                    timeOut = 3000
                )
            } else {
                toastr_error(
                    "Check Error and Log panel for details",
                    title = "Workflow failed",
                    position = "bottom-right",
                    timeOut = 0
                )
            }

        })
        output$output <- renderPrint({
            cat(res(), sep = "\n")
        })
        ## capture logs
        logs <- reactiveVal(NULL)
        log_old <- reactiveVal("")
        observe({
            invalidateLater(10000, session)
            req(dir.exists(file.path(shared$wf$env_path, ".SYSproject")))
            Sys.sleep(1)
            updateProgressBar(session, "update_log", 0)
            log_file <- shinyCatch({
                fs::dir_ls(file.path(shared$wf$env_path, ".SYSproject"), glob = "*_logWF_*", type = "file") %>%
                    {.[file.mtime(.) %>% which.max()]}
            },blocking_level = "error", shiny = F)
            req(length(log_file) > 0)
            log_mtime <- file.mtime(log_file)
            req(!identical(log_mtime, log_old()))
            logs(readLines(log_file))
            log_old(log_mtime)
            Sys.sleep(1)
            updateProgressBar(session, "update_log", 100)
        })
        output$logs <- renderPrint({
            cat(logs(), sep = "\n")
        })
    }
    moduleServer(id, module)
}

#
# aaa = tryCatch(
#     withCallingHandlers(
#         capture.output(eval(parse(text = 'stop(112)'))),
#         warning = function(w){
#             warn(w$message)
#         }
#     ),
#     error = function(e){
#         err(e$message)
#     }
# )
# cat(isolate(warn()), isolate(err()), sep = "\n")
# isolate({warn$warnings[[as.character(length(warn$warnings))]] <- 1; warn$warnings})

