# UI
wf_runUI <- function(id){
    ns <- NS(id)
    tagList(
        # actionButton(ns("set1"), "current page"),
        # actionButton(ns("set2"), "run page"),
        spsDepend("css-loader"),
        div(
            id = "wf_run_displayed",
            style = "display:none",
            tabTitle("Run Workflow"),
            renderDesc(id = ns("desc"),
            '
            #### Running the workflow
            Congradulations! Upon this point, your workflow is ready to run. You can
            choose run it directly from SPS by clicking the `Run workflow` or download
            your workflow bundle in different format and use R console, Rstudio to
            run it.

            If you choose to download, unzip the SPR bundle and follow SPR
            [instructions{blk}](https://systempipe.org/sp/).
            ****

            ##### After running
            If you have generated some results after running the workflow directly
            in SPS, you can come back here and download again. This time, all newly
            created files, logs, results will be included in the bundle.
            ****

            #### Connect with other SPS utilities
            When you are done with workflow running and have obtained some results,
            come back to SPS to make some beautiful plots in the SPS visualization
            modules.
            ****

            #### Other info
            **Most SPR workflows needs to run in *Unix-like* system. Windows will fail to run.**
            Always make sure you have all the commandline tools installed for the workflows
            you want to want. SPS will not be responsible to check those tools for you.
            '),
            spsHr(),
            box(
                width = 4,
                collapsible = FALSE,
                closable = FALSE,
                id = ns("box-left"),
                title = "Initiate a workflow environment",
                tags$ul(
                    id = ns("example_tip"),
                    HTML("<li>A workflow is ready to run.</li>
                             <li>Or you can download all prepared files to run at other places.</li>")
                ),
                fluidRow(
                    class = "text-center",
                    actionButton(ns("run_session"), "Run workflow", style = "margin-top: 25px;") %>%
                        bsPop(
                            "Run workflow directly in SPS",
                            "Clicking here will direct you to a workflow running
                            session and set the working directory to the workflow
                            project. Once the session starts, you cannot interact
                            with other part(modules) of SPS.",
                            "top"
                        )
                )
            ),
            box(
                width = 4,
                id = ns("box-mid"),
                collapsible = FALSE,
                closable = FALSE,
                title = "Choose download formats",
                p("Additional formats to download"),
                checkboxInput(ns("sal2bash"), "Export as bash scripts") %>%
                    bsPop("Export to bash scripts",
                          "SPR workflow can be exported as bash scripts to run. It
                          means after exporting you no longer need SPR or SPS. You
                          can execute the workflow by running the bash script directly.
                          If you choose this option, a 'spr_wf.sh' file and 'spr_bash'
                          folder will be included. cd to unzipped directory and
                          run 'bash spr_wf.sh' to execute the workflow."),
                checkboxInput(ns("sal2rmd"), "Export as workflow templates") %>%
                    bsPop("Export current project to a template",
                          "Exporting current SPR project to a template will enable you
                          to share the workflow with others. Other poeple can easily
                          reproduce your workflow with the same or different data sets."),
                fluidRow(
                    class = "text-center",
                    downloadButton(ns("download"), "Download Bundle", style = "margin-top: 25px;") %>%
                        bsPop(
                            "Download Workflow Bundle",
                            "Clicking here zips everything in current workflow folder and allow you
                                to run the workflow elsewhere. You can even run the workflow here to produce
                                some simple results and then quit the session and come to here to download
                                everything including results.",
                            "top"
                        )
                )
            ),
            box(
                title = "Required files in task",
                width = 4, id = ns("box-right"),
                closable = FALSE,
                collapsible = FALSE,
                strong(id = ns("intask_targets_title"), "Targets file:"),
                p(id = ns("intask_targets"), "No file submitted"),
                strong(id = ns("intask_wf_title"),"Workflow file:"),
                p(id = ns("intask_wf"), "No file submitted")
            ),
            heightMatcher(ns("box-left"), ns("box-mid")),
            heightMatcher(ns("box-right"), ns("box-mid"))
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
        ####### run page shortcut
        # observeEvent(input$set1, ignoreInit = TRUE, {
        #     shared$wf$all_ready <- TRUE
        #     shared$wf$env_path <- "spr_example_wf"
        #     shared$wf$sal <- my_sal
        # })
        # observeEvent(input$set2, ignoreInit = TRUE, {
        #     shared$wf$all_ready <- TRUE
        #     shared$wf$env_path <- "."
        #     shared$wf$rs <- shinyCatch(callr::r_session$new(), blocking_level = "error")
        #     shared$wf$rs$supervise(TRUE)
        #     shared$wf$rs_info$pid <-  shared$wf$rs$get_pid()
        #     shared$wf$rs_info$created <- TRUE
        #     shared$wf$rs_info$log_name <- paste0("SPS", shared$wf$rs_info$pid, ".log")
        #     shared$wf$rs_info$log_path <- file.path(shared$wf$env_path, ".SPRproject", shared$wf$rs_info$log_name)
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
            shared$wf$rs_info$log_path <- file.path(shared$wf$env_path, ".SPRproject",
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
        down_bundle_loader <- addLoader$new("download", type = "facebook", color = "white")
        output$download <- downloadHandler(
            filename = function() {
                "SPR_workflow_by_SPS.zip"
            },
            content = function(filename) {
                on.exit({
                    down_bundle_loader$hide()
                    shinyjs::enable(ns("download"))
                    pg$close()
                })
                down_bundle_loader$show()
                shinyjs::disable(ns("download"))
                pg <- shiny::Progress$new()
                pg$set(0)
                pg$set(message = "Checking folder size")
                if(emptyIsFalse(input$sal2bash)) shinyCatch(blocking_level = "error", {
                    pg$set(25, message = "Creating bash files")
                    sal <- shared$wf$sal
                    systemPipeR::sal2bash(sal, out_dir = shared$wf$env_path)
                })
                if(emptyIsFalse(input$sal2rmd)) shinyCatch(blocking_level = "error", {
                    pg$set(50, message = "Creating new workflow template file")
                    systemPipeR::sal2rmd(
                        shared$wf$sal, verbose = FALSE,
                        desc = "This is a workflow template generated from SPS app.",
                        file.path(shared$wf$env_path, "exported_SPR_template.Rmd")
                    )
                })
                # shinyCatch({
                #     all_size <- fs::dir_ls(shared$wf$env_path, all = TRUE, recurse = TRUE) %>%
                #         fs::file_size() %>% sum()
                #     if(all_size > 3e+8) stop("Workflow folder too large, over limit")
                # }, blocking_level = "error")
                pg$set(70, message = "Start to zip, please wait")
                zip::zip(zipfile=filename, files=shared$wf$env_path, mode = "cherry-pick")
            },
            contentType = "application/zip"
        )
    }
    moduleServer(id, module)
}

