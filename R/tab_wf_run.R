# UI
wf_runUI <- function(id){
    ns <- NS(id)
    tagList(
        actionButton(ns("set"), "set"),
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
                fluidRow(
                    class = "text-center",
                    actionButton(ns("run_session"), "Run workflow", style = "margin-top: 25px;") %>%
                        bsHoverPopover(
                            "Start a workflow running",
                            "Clicking here will direct you to a workflow running
                            session and set the working directory to the workflow
                            project. Once the session starts, you cannot interact
                            with other part of SPS.",
                            "bottom"
                        )
                ),
                tags$ul(
                    id = ns("example_tip"),
                    HTML("<li>A workflow is ready to run.</li>")
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
        observeEvent(input$set, {
            shared$wf$all_ready <- TRUE
        })
        # right side display in task files
        observeEvent(shared$wf$all_ready, {
            shinyjs::html("intask_targets", shared$wf$targets_path)
            shinyjs::html("intask_targets_title", "Targets file (Ready):")
            shinyjs::addCssClass("intask_targets_title", "text-success")
            shinyjs::html("intask_wf", shared$wf$wf_path)
            shinyjs::html("intask_wf_title", "Workflow file (Ready):")
            shinyjs::addCssClass("intask_wf_title", "text-success")
        })
        ## open close wf push bar
        observeEvent(input$run_session, ignoreInit = TRUE, {
            pushbar::pushbar_open(id = "core_top-wf_push")
            shared$wf$wf_session_open <- TRUE
            shared$wf$wd_old <- spsOption("app_path")
            setwd(shared$wf$env_path)
            print(getwd())
        })

        # listen to log change
    }
    moduleServer(id, module)
}
#
# r_out <- list()
# r_cmd <- rlang::parse_exprs('print(1)\nSys.sleep(1)\nprint(2)')
#
# capture.output( %>% {
#     for(i in seq_along(r_cmd)){
#         r_out[[i]] <-
#         eval_tidy(i)
#     }
# })
#

# echo "string" | out-file -encoding ASCII file.txt
