## UI
########### loaded but not in use under current SPS version

#' @noRd
core_topUI <- function(id){
    ns <- NS(id)
    init <-
"print('Ready to run a workflow')"
    tagList(
        tags$script(src = "sps/js/split1.6.0.js"),
        pushbar::pushbar_deps(),
        pushbar::pushbar(
            id = ns("wf_push"),
            from = "top",
            style= "background:#ECF0F5; padding:5px; min-height:100vh; overflow-y:auto; overflow-x: hidden;",
            tagList(
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
                            "Clicking here kills the workflow R session, clear all data
                            stored in that R session and  reset
                            your working directory to SPS app directory. If you have
                            workflow jobs running, they will be forced to terminate.",
                            "bottom"
                        )
                ),
                div(
                    style = "height: 95vh;",
                    div(
                        id = ns("leftcol"), class = "split split-horizontal", style = "overflow: hidden;",
                        # source code ----
                        div( id = ns("source_code"), class ="split split-content",
                             div(
                                 class = "split-header",
                                 h3("Source Code", class = "split-title"),
                                 div(class = "split-tool", actionButton("a", "", icon = icon("minus"), class = ""))
                             ),
                             div(
                                 class = "split-body", style = "position: relative;",
                                 aceEditor(
                                     ns("code"), mode = "r", height = "30vh", value = init,
                                     autoComplete = "live",
                                     autoCompleters = c("static", "text"),
                                     autoScrollEditorIntoView = TRUE,
                                     maxLines = 9999,
                                     debounce = 10,
                                     autoCompleteList = getNamespaceExports("systemPipeR") %>%
                                         {.[!str_detect(., "^\\.")]} %>%
                                         {paste0("systemPipeR::", .)} %>%
                                         list(systemPipeR = .)) %>%
                                     {.[[2]][["attribs"]][["style"]] = "height: 30vh; min-height: 30vh;"; .},
                                 fluidRow(
                                     column(6,
                                            actionButton(ns("eval"), "Run"),
                                            div(id = ns("loading_eval"), style = "display: none;",
                                                spsLoader())
                                     ),
                                     column(6,
                                            tags$b(class = "pull-right", id = ns("rs_status"), "No active R session")
                                     )
                                 ),
                                 tags$p("There is about one second delay sending code from UI to
                                backend. Wait a second after editing code before hitting 'Run'.")
                             )
                        ),
                        # console ----
                        div( id = ns("console"), class ="split split-content",
                             div(
                                 class = "split-header",
                                 h3("Console Output", class = "split-title"),
                                 div(
                                     class = "split-tool",
                                     actionButton(
                                         ns("console_stop"), "",
                                         class = "shinyjs-hide",
                                         icon = icon("stop"),
                                         `data-toggle`="tooltip",
                                         `data-placement`="bottom",
                                         title="Interrupt session"
                                     ),
                                     tags$style(glue('
                                     #@{ns("console_stop")}@ .fa-stop {color: #d03e3b;}
                                     ', .open = "@{", .close = "}@")),
                                     div(
                                         class = "dropdown",
                                         # `data-toggle`="tooltip",
                                         # `data-placement`="top",
                                         # title = "console options",
                                         style = "display: inline;",
                                         tags$button(
                                             class="btn btn-default dropdown-toggle",
                                             type = "button",
                                             id = ns("out_options"),
                                             `data-toggle` = "dropdown",
                                             `aria-haspopup`= "true",
                                             `aria-expanded`= "false",
                                             tags$i(class = "fa fa-gear")
                                         ),
                                         tags$ul(
                                             class="dropdown-menu",
                                             `aria-labelledby` = ns("out_options"),
                                             tags$li(class = "dropdown-header", "Stop on error"),
                                             tags$li(
                                                 style = "margin-left: 15px;",
                                                 shinyWidgets::switchInput(
                                                     size = "mini",
                                                     value = TRUE,
                                                     inputId = ns("stop_on_err"),
                                                     offStatus = "danger",
                                                     width = "50%"
                                                 )
                                             )
                                         )
                                     ),
                                     actionButton(
                                         ns("clear_console"), "",
                                         icon = icon("trash"),
                                         `data-toggle`="tooltip",
                                         `data-placement`="bottom",
                                         title="clear console"
                                     ),
                                     actionButton("a", "", icon = icon("minus"), class = "")
                                 )
                             ),
                             div(
                                 class = "split-body",
                                 verbatimTextOutput(ns("output")) %>%
                                     {.$attribs[['style']] <- " margin:0; overflow: visible;"; .}
                             )
                        )
                    ),
                    div(
                        id = ns("rightcol"), class ="split split-horizontal",
                        # wf log ----
                        div(
                            id = ns("wf_log"), class ="split split-content",
                            div(
                                class = "split-header",
                                h3("Workflow Log", class = "split-title"),
                                div(
                                    class = "split-tool",
                                    actionButton("a", "", icon = icon("minus"), class = "")
                                )
                            ),
                            div(class = "split-body",
                                verbatimTextOutput(ns("logs")) %>%
                                    {.$attribs[['style']] <- " margin:0; overflow: visible;"; .},
                                p("This panel always displays the most recent
                                      modified log in `.SYSproject` folder, updates every 10s."),
                                p("Only systemPipeR workflow logs will be displayed here. Check
                                      console for any other random R code results"),
                                div(
                                    style="height:5px; width:100%",
                                    shinyWidgets::progressBar(
                                        ns("update_log"), value = 0,
                                        status = "primary", striped = TRUE,
                                        size = "xs"
                                    )
                                )
                            )
                        ),
                        # plot ----
                        div(
                            id = ns("plot_panel"), class ="split split-content",
                            div(
                                class = "split-header",
                                h3("Plots", class = "split-title"),
                                div(
                                    class = "split-tool",
                                    # div(
                                    #     class = "dropdown",
                                    #     style = "display: inline;",
                                    #     tags$button(
                                    #         class="btn btn-default dropdown-toggle",
                                    #         type = "button",
                                    #         id = ns("plot_options"),
                                    #         `data-toggle` = "dropdown",
                                    #         `aria-haspopup`= "true",
                                    #         `aria-expanded`= "false",
                                    #         tags$i(class = "fa fa-gear")
                                    #     ),
                                    #     tags$ul(
                                    #         class="dropdown-menu",
                                    #         `aria-labelledby` = ns("plot_options"),
                                    #         tags$b("Delete/Clear also remove server-end files"),
                                    #         tags$li(
                                    #             style = "margin-left: 15px;",
                                    #             shinyWidgets::switchInput(
                                    #                 size = "small",
                                    #                 value = FALSE,
                                    #                 inputId = ns("stop_on_err"),
                                    #                 offStatus = "danger",
                                    #                 width = "50%"
                                    #             )
                                    #         )
                                    #     )
                                    # ),
                                    actionButton("a", "", icon = icon("minus"), class = "")
                                )
                            ),
                            div(
                                style = "position: relative; height: 100%;",
                                div(
                                    class = "rs_plot",
                                    div(class = "rs_plot_arrow",
                                        actionButton("rs-plot-left", "",
                                                     icon = icon("chevron-left")
                                        )
                                    ),
                                    div(class = "text-center rs_plot_container",
                                        id = "rs_plot",
                                        tags$img()
                                    ),
                                    div(class = "rs_plot_arrow arrow_right",
                                        actionButton("rs-plot-right", "",
                                                     icon = icon("chevron-right")
                                        )
                                    )
                                ),
                                fluidRow(
                                    class = "rs_plot_footer",
                                    style = "position: relative; height: 5%",
                                    actionButton(
                                        "rs-plot-trash", "", icon = icon("trash"),
                                        `data-toggle`="tooltip",
                                        `data-placement`="top",
                                        title="delete current plot"
                                    ),
                                    actionButton(
                                        "rs-plot-clear", "", icon = icon("broom"),
                                        `data-toggle`="tooltip",
                                        `data-placement`="top",
                                        title="clear all plots"
                                    ),
                                    h5("0 / 0"),
                                    actionButton(
                                        "rs-plot-canvas", "", icon = icon("paint-brush"),
                                        `data-toggle`="tooltip",
                                        `data-placement`="top",
                                        title="Send to canvas",
                                        onclick=paste0('toCanvas("', "#rs_plot img", '", "', "core_canvas", '");
                                                        if(toastr) toastr.info("Sceenshot sent to Canvas", "", {positionClass: "toast-bottom-right", timeOut: 2000});
                                        ')
                                    ),
                                    actionButton(
                                        "rs-plot-png", "", icon = icon("file-image"),
                                        `data-toggle`="tooltip",
                                        `data-placement`="top",
                                        title="Save as image",
                                        onclick = paste0('toPng("', '#rs_plot img', '")')
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}

## server

#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyWidgets actionBttn confirmSweetAlert
#' @importFrom shinyjs toggleState
#' @noRd
core_topServer <- function(id, shared){
    module <- function(input, output, session){
        pushbar::setup_pushbar(blur = TRUE, overlay = TRUE)
        ns <- session$ns
        # update session status ----
        rs_status_text <- reactiveVal("")
        update_rs <- observe({
            invalidateLater(1000)
            text_new <- utils::capture.output(shared$wf$rs$print())
            req(rs_status_text() != text_new)
            shinyjs::html("rs_status", text_new)
            if(shared$wf$rs$get_state() == "busy"){
                shinyjs::addCssClass("rs_status", "text-warning sps-blink")
                shinyjs::removeCssClass("rs_status", "text-success")
                shinyjs::show("console_stop", anim = TRUE, animType = "fade")
            } else {
                shinyjs::addCssClass("rs_status", "text-success")
                shinyjs::removeCssClass("rs_status", "text-warning sps-blink")
                shinyjs::hide("console_stop", anim = TRUE, animType = "fade")
            }
            rs_status_text(text_new)
        }, suspended = TRUE)
        ## toggle rs indicator text switch
        observeEvent(shared$wf$rs_info$created, ignoreInit = TRUE, {
            if(shared$wf$rs_info$created) {
                update_rs$resume()
            }
            else {
                update_rs$suspend()
                shinyjs::html("rs_status", "no R session active")
                shinyjs::removeCssClass("rs_status", "text-success")
                shinyjs::removeCssClass("rs_status", "text-warning")
            }
        })
        # close session ----
        observeEvent(input$close_push, ignoreInit = TRUE, {
            if(shared$wf$rs$get_state() == "busy") shinytoastr::toastr_warning(
                "Session is still busy", position = "bottom-right"
            )
            shinyWidgets::confirmSweetAlert(
                session,
                inputId = ns("confirm_close"),
                title = "Close current session?",
                text = if (shared$wf$rs$get_state() == "busy") "Session is running! Any running workflow or R code will be stopped, and it may cause error. Unsaved data will be lost."
                       else "Unsaved data will be lost.",
                type = if(shared$wf$rs$get_state() == "busy") "warning" else "info"
            )
        })

        observeEvent(input$confirm_close, ignoreInit = TRUE, {
            req(input$confirm_close)
            # destroy observers
            update_rs$suspend()
            out_watcher$suspend()
            plot_watcher$suspend()
            # update status
            shared$wf$wf_session_open <- FALSE
            try({shared$wf$rs$close(); shared$wf$rs$finalize()}, silent = TRUE)
            shared$wf$rs_info$created <- FALSE
            shared$wf$rs <- NULL
            shared$wf$rs_info$log_name <- NULL
            shared$wf$rs_info$log_path <- NULL
            # close session
            pushbar::pushbar_close()
            shinytoastr::toastr_info(
                glue("Workflow session {shared$wf$rs_info$pid} closed."),
                position = "bottom-right", timeOut = 2000)
            shared$wf$rs_info$pid <- NULL
            options(width = if(!is.null(spsOption("console_width"))) spsOption("console_width") else 80)
        })

        # init editor code ----
        observeEvent(shared$wf$wf_session_open, {
            req(shared$wf$wf_session_open)
            if(!emptyIsFalse(shared$wf$wf_path))
                toastr_warning("Workflow path not detected", position = "bottom-right", timeOut = 3000)
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
        ## run code ----
        err <- reactiveVal(NULL)
        res <- reactiveVal(NULL)
        last_stdout <- reactiveVal(NULL)
        log_start_n <- reactiveVal(0)
        log_path <- reactive(shared$wf$rs_info$log_path)
        code_que <- reactiveVal(list())
        code_submitted <- reactiveVal(FALSE)
        # need to create a que hold code
        observeEvent(input$eval, ignoreInit = TRUE, {
            req(shared$wf$rs_info$created)
            err(NULL); res(NULL)
            if(!emptyIsFalse(input$code))
                toastr_error("No code to run", position = "bottom-right", timeOut = 3000)
            req(input$code)
            shinyjs::hide("eval")
            shinyjs::show("loading_eval")
            ## add code to queue
            code_que(tryCatch(
                rlang::parse_exprs(input$code),
                error = function(e){
                    err_msg <- str_split(e$message, "\n") %>% unlist()
                    err_msg[1] <- paste0("Code parsing ERROR, illegal R code in : ", err_msg[1])
                    for(i in err_msg){
                        insertUI(
                            selector = paste0("#", ns("output")),
                            ui = p(class = "text-danger", i))
                    }
                    shinyjs::hide("loading_eval")
                    shinyjs::show("eval")
                }
            ))
        })
        # start to send code ----
        observeEvent(code_que(), ignoreInit = TRUE, {
            if(!emptyIsFalse(code_que())){
                shinyjs::hide("loading_eval")
                shinyjs::show("eval")
            }
            req(emptyIsFalse(code_que()))
            ## update console
            shinyCatch({
                code <- rlang::expr_text(code_que()[[1]]) %>% str_split("\n") %>% unlist()
                code[1] = paste0("> ", code[1])
                for(i in code){
                    insertUI(
                        selector = paste0("#", ns("output")),
                        ui = p(class = "text-primary", i))
                }
            }, blocking_level = "error")
            ## send code to child
            shared$wf$rs$call(function(code) {
                on.exit({
                    if(!is.null(ggplot2::last_plot())) {
                        grid::grid.draw(ggplot2::last_plot())
                        dev.off()
                        ggplot2::set_last_plot(NULL)
                        .cur_plot <<- NULL
                    }
                    if(dev.cur() != 1){
                        if(!identical(.cur_plot, recordPlot())){
                            .cur_plot <<- recordPlot()
                            png(file.path(
                                .rs_dir,
                                paste0(
                                    "plot",
                                    stringr::str_pad(.plot_num, 3, pad = "0"),
                                    "_001.png")
                            ))
                            replayPlot(.cur_plot); dev.off()
                            .plot_num <<- .plot_num + 1
                        }
                    }
                })
                eval(code, env = .GlobalEnv)
            }, args = list(
                code = code_que()[[1]]
            ))
            code_submitted(TRUE)
        })
        # turn on output watcher
        observeEvent(code_submitted(), {
            req(code_submitted())
            # if(emptyIsFalse(que()))  out_watcher$suspend()
            out_watcher$resume()
        })
        # watch for console output ----
        out_watcher <- observe(suspended = TRUE, {
            invalidateLater(1000)
            req(shared$wf$rs)
            if(shared$wf$rs$poll_process(1) == "timeout"){
                ## while code still running
                log_current_n <- R.utils::countLines(log_path())
                new_lines <- NULL
                if(log_start_n() < log_current_n) new_lines <- readr::read_lines(log_path(), skip = log_start_n())
                for(i in new_lines){
                    insertUI(selector = paste0("#", ns("output")), ui = p(i))
                }
                log_start_n(log_current_n)
            } else {
                ## when code finished
                rs_out <- shared$wf$rs$read()
                res <- rs_out$result
                if(inherits(res, "ggplot")) res <- NULL # prevent ggplot to be rendered on parent
                res <- utils::capture.output(rs_out$result)
                ## mute output for certain functions
                if(rlang::expr_text(code_que()[1][[1]]) %>%
                   stringr::str_detect(
                       paste0(
                           "^",
                           "(print\\()|",
                           "(library\\()"
                       ))
                 ){
                    res <- NULL # handle print resulting duplicates on console
                }

                if(length(res) < 1) res <- NULL # prevent character(0) capture
                else if(length(res) > 1) NULL # do nothing if > 1
                else if(res == "NULL") res <- NULL # mute NULL output
                if(length(res) > 10){
                    insertUI(
                        selector = paste0("#", ns("output")),
                        ui = p(tags$b(class = "text-warning", "You are outputing too many lines to console, only display first 10"))
                    )
                    res <- res[seq(10)]
                }
                for(i in res) insertUI(selector = paste0("#", ns("output")), ui = p(class = "text-info", paste0(i)))
                if(!is.null(rs_out$error)){
                    err <- rs_out$error %>% utils::capture.output()
                    for(i in err) insertUI(selector = paste0("#", ns("output")), ui = p(class = "text-danger", paste0(i)))
                    toastr_error(title = "ERROR", message = "Check console output", position = "bottom-right", timeOut = 3000)
                    if(input$stop_on_err){
                        code_que(list())
                    }
                }
                code_que(isolate(code_que())[-1]) # clear top line in queue
                code_submitted(FALSE) # turn off output watcher
            }
        })
        # rs interrupt ----
        observeEvent(input$console_stop, {
            req(shared$wf$rs$get_state() == "busy")
            shared$wf$rs$interrupt()
            code_que(list())
        })
        # update plots ----
        rs_plots <- reactiveValues(plots_new = c(), plots_done = c(), plots_done_md5 = c())
        plot_watcher <- observe(suspended = TRUE, {
            invalidateLater(3000)
            plots <- shinyCatch({fs::dir_ls(shared$wf$rs_info$rs_dir, glob = "*.png", type = "file")}, blocking_level = "error")
            # watch for finished plots
            plots_size <- fs::file_size(plots)
            plots_done_new <- plots[plots_size > 0]
            req(!identical(plots_done_new, rs_plots$plots_done))

            plots_done_new <- plots_done_new[!plots_done_new %in% rs_plots$plots_done]
            req(length(plots_done_new) > 0) # stop if no new plot
            plots_done_pass <- plots_done_new[!plots_done_new %in% rs_plots$plots_new]
            ## check for previously unfinished but currently done plots
            plots_check <- rs_plots$plots_new[plots_done_new %in% rs_plots$plots_new]
            if(length(plots_check) > 0){
                plots_check_md5 <- lapply(plots_check, function(x) openssl::md5(file(x)) %>% as.character) %>%
                    unlist()
                ### remove them if one copy is in the stack
                file.remove(plots_check[plots_check_md5 %in% rs_plots$plots_done_md5])
                plots_check <- plots_check[!plots_check_md5 %in% rs_plots$plots_done_md5]
            }
            # watch for not finished plots
            rs_plots$plots_new <- plots[plots_size == 0]
            ## combine all new plots
            plots_done_all <- c(plots_done_pass, plots_check)
            req(length(plots_done_all) > 0)
            ## update done plots
            rs_plots$plots_done <- c(isolate(rs_plots$plots_done), plots_done_all)
            ## update md5s
            md5s <- lapply(plots_done_all, function(x) openssl::md5(file(x)) %>% as.character) %>%
                unlist()
            rs_plots$plots_done_md5 <- c(isolate(rs_plots$plots_done_md5), md5s)
            ## send to front
            session$sendCustomMessage("rs_plot", as.list(plots_done_all %>% basename() %>% paste0("rs/", .)))
        })
        # start to watch plot when rs is active
        observeEvent(shared$wf$rs_info$created, {
            if(shared$wf$rs_info$created){
                plot_watcher$resume()
            } else {
                plot_watcher$suspend()
            }
        })
        # clear console
        observeEvent(input$clear_console, ignoreInit = TRUE, {
            removeUI(selector = paste0("#", ns("output"), " > *"), multiple = TRUE, immediate = TRUE)
        })
        ## capture SPR logs----
        observeEvent(1, {
            output$logs <- renderPrint({
                cat(rep("\n", 10))
            })
        }, once = TRUE)
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
            },blocking_level = "error", shiny = FALSE)
            req(length(log_file) > 0)
            log_mtime <- file.mtime(log_file)
            req(!identical(log_mtime, log_old()))
            logs(readLines(log_file))
            log_old(log_mtime)
            output$logs <- renderPrint({
                cat(logs(), sep = "\n")
            })
            Sys.sleep(1)
            updateProgressBar(session, "update_log", 100)
        })

        # kill/clean wf session on end ----
        session$onEnded(function(){
            try({shared$wf$rs$close(); shared$wf$rs$finalize()}, silent = TRUE)
            options(width = {wd <- spsOption("console_width") ;if(is.numeric(wd)) wd else 80})
        })
    }
    moduleServer(id, module)
}


