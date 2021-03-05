# UI
wf_runUI <- function(id){
    ns <- NS(id)
    tagList(
        # actionButton(ns("set"), "set"),
        div(
            id = "wf_run_displayed",
            style = "display:none",
            tabTitle("Run Workflow"),
            renderDesc(id = ns("desc"),
                       '
        #### Running the workflow
        Directly running the prepared workflow from SPS will be supported soon.
        At this point, you should have prepared all the three very important
        files for SPR workflow running. Open up the progress tracking panel
        from top right corner and you should see everything is green. That
        means you are ready to go.

        Copy these files to your SPR workflow project root and follow SPR
        instructions.

        When you are done with workflow running and have obtained some results,
        come back to SPS to make some beautiful plots in the "Visualization".

        **Most SPR workflows needs to run in *Unix-like* system. Windows will fail to run except the example workflow**
        '),
            spsHr(),
            boxPlus(
                width = 8,
                collapsible = FALSE,
                closable = FALSE,
                title = "Initiate a workflow environment",
                tags$ul(
                    id = ns("example_tip"),
                    HTML("<li>A workflow is ready to run.</li>
                         <li>Or you can download all prepared files to run at other places.</li>")
                ),
                fluidRow(
                    class = "text-center",
                    actionButton(ns("run_session"), "Run workflow", style = "margin-top: 25px;") %>%
                        bsHoverPopover(
                            "Start a workflow running",
                            "Clicking here will direct you to a workflow running
                            session and set the working directory to the workflow
                            project. Once the session starts, you cannot interact
                            with other part of SPS.",
                            "top"
                        ),
                    downloadButton(ns("download"), "Download Bundle", style = "margin-top: 25px;") %>%
                        bsHoverPopover(
                            "Download Workflow Bundle",
                            "Clicking here zips everything in current workflow folder and allow you
                            to run the workflow elsewhere. You can even run the workflow here to produce
                            some simple results and then quit the session and come to here to download
                            everything including results. Current limit is 300Mb.",
                            "top"
                        ),
                    div(id = ns("loading_down"), style = "display: none;",
                        spsLoader())
                )
            ),
            boxPlus(
                title = "Required files in task",
                width = 4,
                closable = FALSE,
                collapsible = FALSE,
                strong(id = ns("intask_targets_title"), "Targets file:"),
                p(id = ns("intask_targets"), "No file submitted"),
                strong(id = ns("intask_wf_title"),"Workflow file:"),
                p(id = ns("intask_wf"), "No file submitted")
            )
        ),
        div(
            id = "wf_run_disable",
            h3("Complete step 1-3 first.",
               style = "text-center text-warning")
        )
    )

}

# server
wf_runServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "wf_run"
        # toggle elements in wf init panel
        observeEvent(input$choose_wf, {
            shinyjs::toggleElement(
                id = "exist_browse", anim = TRUE,
                condition = input$choose_wf == "exist")
            shinyjs::toggleElement(
                id = "gen_warning", anim = TRUE,
                condition = input$choose_wf != "eg")
        })
        ####### run page shortcut
        # observeEvent(input$set, ignoreInit = TRUE, {
        #     shared$wf$all_ready <- TRUE
        #     shared$wf$env_path <- "."
        #     shared$wf$rs <- shinyCatch(callr::r_session$new(), blocking_level = "error")
        #     shared$wf$rs$supervise(TRUE)
        #     shared$wf$rs_info$pid <-  shared$wf$rs$get_pid()
        #     shared$wf$rs_info$created <- TRUE
        #     shared$wf$rs_info$log_name <- paste0("SPS", shared$wf$rs_info$pid, ".log")
        #     shared$wf$rs_info$log_path <- file.path(shared$wf$env_path, ".SYSproject", shared$wf$rs_info$log_name)
        #     shared$wf$rs_info$rs_dir <- file.path(shared$wf$env_path, "results", paste0("rs", shared$wf$rs_info$pid))
        #     spsOption("console_width",  getOption("width"))
        #     options(width = 80)
        #     dir.create(dirname(shared$wf$rs_info$log_path), recursive = TRUE, showWarnings = FALSE)
        #     dir.create(shared$wf$rs_info$rs_dir, recursive = TRUE, showWarnings = FALSE)
        #     if(!file.exists(shared$wf$rs_info$log_path)) file.create(shared$wf$rs_info$log_path)
        #     addResourcePath("rs", shared$wf$rs_info$rs_dir)
        #     # init r session settings
        #     shared$wf$rs$call(function(log_path, rs_dir) {
        #         options(device = function(){
        #             png(file.path(.rs_dir, paste0("plot", stringr::str_pad(.plot_num, 3, pad = "0"), "_%03d.png")))
        #             if(dev.cur() == 1) dev.new()
        #             dev.control("enable")
        #             .plot_num <<- .plot_num + 1
        #         })
        #         .rs_dir <<- rs_dir
        #         .plot_num <<- 1
        #         .cur_plot <<- NULL
        #         log_file <- file(log_path, "awt")
        #         sink(log_file, append = TRUE, type = "o")
        #         sink(log_file, append = TRUE, type = "m")
        #     }, args = list(
        #         log_path = shared$wf$rs_info$log_path,
        #         rs_dir = shared$wf$rs_info$rs_dir
        #     ))
        #     while(shared$wf$rs$poll_process(1000) == "timeout") next
        #     shared$wf$rs$read()
        #     # everything done, open session
        #     shinyjs::runjs('pushbar.__proto__.handleKeyEvent = function(){return false};') # disable ESC key
        #     pushbar::pushbar_open(id = "core_top-wf_push")
        #     shared$wf$wd_old <- spsOption("app_path")
        #     setwd(shared$wf$env_path)
        # })
        ########
        # right side display in task files
        observeEvent(shared$wf$all_ready,{
            req(shared$wf$all_ready)
            shinyjs::html("intask_targets", shared$wf$targets_path)
            shinyjs::html("intask_targets_title", "Targets file (Ready):")
            shinyjs::addCssClass("intask_targets_title", "text-success")
            shinyjs::html("intask_wf", shared$wf$wf_path)
            shinyjs::html("intask_wf_title", "Workflow file (Ready):")
            shinyjs::addCssClass("intask_wf_title", "text-success")
        })
        ## open  wf push bar and r session
        observeEvent(input$run_session, ignoreInit = TRUE, {
            # set up child session
            shared$wf$rs <- shinyCatch(callr::r_session$new(), blocking_level = "error")
            shared$wf$rs$supervise(TRUE)
            # update SPS wf status
            shared$wf$rs_info$pid <-  shared$wf$rs$get_pid()
            shared$wf$rs_info$created <- TRUE
            shared$wf$rs_info$log_name <- paste0("SPS", shared$wf$rs_info$pid, ".log")
            shared$wf$rs_info$log_path <- file.path(shared$wf$env_path, "log",
                                                    shared$wf$rs_info$log_name)
            shared$wf$rs_info$rs_dir <- file.path(shared$wf$env_path, "results", paste0("rs", shared$wf$rs_info$pid))
            # create log folder and file
            dir.create(dirname(shared$wf$rs_info$log_path), recursive = TRUE, showWarnings = FALSE)
            dir.create(shared$wf$rs_info$rs_dir, recursive = TRUE, showWarnings = FALSE)
            if(!file.exists(shared$wf$rs_info$log_path)) file.create(shared$wf$rs_info$log_path)
            # set up console width and store current width in shared
            spsOption("console_width",  getOption("width"))
            options(width = 80)
            # init r session settings
            shared$wf$rs$call(function(log_path, rs_dir, new_wd) {
                options(device = function(){
                    png(file.path(.rs_dir, paste0("plot", stringr::str_pad(.plot_num, 3, pad = "0"), "_%03d.png")))
                    if(dev.cur() == 1) dev.new()
                    dev.control("enable")
                    .plot_num <<- .plot_num + 1
                })
                setwd(new_wd)
                .rs_dir <<- rs_dir
                .plot_num <<- 1
                .cur_plot <<- NULL
                log_file <- file(log_path, "awt")
                sink(log_file, append = TRUE, type = "o")
                sink(log_file, append = TRUE, type = "m")
            }, args = list(
                log_path = normalizePath(shared$wf$rs_info$log_path),
                rs_dir = normalizePath(shared$wf$rs_info$rs_dir),
                new_wd = normalizePath(shared$wf$env_path)
            ))
            while(shared$wf$rs$poll_process(1000) == "timeout") next
            shared$wf$rs$read()
            # setup img resource path for html
            addResourcePath("rs", shared$wf$rs_info$rs_dir)
            # disable pushbar close by ESC key
            shinyjs::runjs('pushbar.__proto__.handleKeyEvent = function(){return false};')
            # open up session
            pushbar::pushbar_open(id = "core_top-wf_push")
            shared$wf$wf_session_open <- TRUE
        })
        # download bundle
        output$download <- downloadHandler(
            filename = function() {
                "SPR_workflow_by_SPS.zip"
            },
            content = function(filename) {
                on.exit({
                    shinyjs::hide(ns("loading_down"))
                    shinyjs::show(ns("download"))
                    pg$close()
                })
                shinyjs::hide(ns("download"))
                shinyjs::show(ns("loading_down"))
                pg <- shiny::Progress$new()
                pg$set(0)
                pg$set(message = "Checking folder size")
                shinyCatch({
                    all_size <- fs::dir_ls(shared$wf$env_path, all = TRUE, recurse = TRUE) %>%
                        fs::file_size() %>% sum()
                    if(all_size > 5e+8) stop("Workflow folder toooooooooo large")
                    if(all_size > 3e+8) stop("Workflow folder too large")
                }, blocking_level = "error")
                pg$set(10)
                pg$set(message = "Start to zip, please wait")
                zip::zip(zipfile=filename, files=shared$wf$env_path, mode = "cherry-pick")
            },
            contentType = "application/zip"
        )
    }
    moduleServer(id, module)
}

