# UI
wf_runUI <- function(id){
    ns <- NS(id)
    tagList(
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

        When you are done with workflow running and obtained some results,
        come back to SPS to make some beautiful plots in the "Visualization".

        **This module needs to run in *Unix-like* system. Windows will fail to run even with the example workflow**
        '),
        spsHr(),
        boxPlus(
            width = 8,
            collapsible = FALSE,
            closable = FALSE,
            title = "Initiate a workflow environment",
            fluidRow(
                column(
                    6,
                    shinyWidgets::pickerInput(
                        inputId = ns("choose_wf"),
                        label = "Choose a workflow template",
                        choices = c(Example="eg", RNAseq="rnaseq", Varseq="varseq",
                                    Riboseq="riboseq", Chipseq="chipseq", Existing="exist"),
                        options = list(style = "btn-primary")
                    )
                ),
                column(
                    6,
                    actionButton(ns("gen_env"), "Gen workflow", style = "margin-top: 25px;") %>%
                        bsHoverPopover(
                            "Start a workflow environment",
                            "Clicking here will direct you to a workflow running
                            session and set the working directory to the workflow
                            project. Once the session starts, you cannot interact
                            with other part of SPS.",
                            "bottom"
                        )
                )
            ),
            fluidRow(
                class = "form-group shiny-input-container sps-file center-block",
                id = ns("exist_browse"),
                tags$label(class="control-label", `for`=ns("exist_wf"), "Select existing workflow folder"),
                div(class="input-group",
                    tags$label(class="input-group-btn input-group-prepend",
                               shinyFiles::shinyFilesButton(
                                   ns("exist_wf"), "Browse", multiple = FALSE,
                                   title = "",
                                   buttonType = "btn btn-primary",
                                   icon = NULL)
                    ),
                    textInput(inputId = ns("exist_show"), label = NULL,
                              placeholder="No path yet", width = "100%")
                )
            ),
            tags$ul(
                id = ns("example_tip"),
                HTML("<li>If you have submitted .</li>")
            ),
            tags$ul(
                class = "text-danger", id = ns("gen_warning"),
                tags$li("This will run a non-example workflow.
                If the required command
                line tools are not installed, the workflow will fail."),
                HTML("<li>Most other default template workflows like RNAseq,
                Varseq provide more than one alignment, calling, and other methods. You
                only need to choose one method on certain steps. Make sure to select
                the desired method steps in the <strong>Workflow File tab</strong>.
                Using the defualt workflow file without
                any custom selection is not recommended.</li>")
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
        # resolve dir path input
        roots <- c(current=getwd(), shinyFiles::getVolumes()())
        shinyFiles::shinyFileChoose(input, 'exist_wf', roots = roots, session = session)
        wf_exist_path <- reactive(NULL)
        observeEvent(input[['exist_wf']], {
            req(is.list(input[['exist_wf']]))
            file_selected <- shinyFiles::parseFilePaths(roots, input[['exist_wf']])
            updateTextInput(inputId = 'exist_show',
                            session = session,
                            placeholder = unname(file_selected$datapath))
            wf_exist_path({as.data.frame(file_selected)})
        })
        # right side display in task files
        observeEvent(shared$targets$file, {
            req(shared$wf_flags$targets_ready)
            req(shared$targets$file)
            shinyjs::html("intask_targets", shared$targets$file)
            shinyjs::html("intask_targets_title", "Targets file (Ready):")
            shinyjs::addCssClass("intask_targets_title", "text-success")
        })
        observeEvent(shared$wf$file, {
            req(shared$wf_flags$wf_ready)
            req(shared$wf$file)
            shinyjs::html("intask_wf", shared$wf$file)
            shinyjs::html("intask_wf_title", "Workflow file (Ready):")
            shinyjs::addCssClass("intask_wf_title", "text-success")
        })
        ## open close wf push bar
        pushbar::setup_pushbar(blur = TRUE, overlay = TRUE)
        observeEvent(input$gen_env, ignoreInit = TRUE, {
            pushbar::pushbar_open(id = "core_top-wf_push")
            shared$wf$wd_old <- spsOption("app_path")
            setwd("new")
            print(getwd())
        })
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
