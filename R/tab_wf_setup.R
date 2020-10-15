# UI
wf_setupUI <- function(id){
    ns <- NS(id)
    tagList(
        renderDesc(id = ns("desc"),
        '
        #### Set up a workflow environment
        To run a SPR workflow, a workflow environment is required. The
        environment is a directory containing all required files, like the targets
        file, the workflow file, and all other
        [parameter files](https://systempipe.org/docs/systemPipeR/#directory-structure).

        The directory structure looks like this image below:

        ![spr-structure](https://systempipe.org/assets/images/doc/SYSdir.png)

        Read more about this [workflow structure](https://systempipe.org/docs/systemPipeR/#directory-structure).

        #### Template workflows
        SPR has some preconfigured workflows that you can generate in SPS with
        one click. Supported template workflows are: **1**. *RNASeq*, **2**. *VarSeq*, **3**. *RiboSeq*,
        **4**. *ChipSeq*. You can also choose an **5**. *existing* SPR workflow directory or create
        an **6**. *empty* SPR workflow directory.

        - All choices except "existing" will directly use the targets file and workflow
        file inside the SPR project folder as default or you can upload a new one.
        When you "Add to task" targets or workflow file, these files in the project
        directory will be overwritten back to the same file.

        - If you choose the "*existing*" option, we cannot guess the targets file name
        and workflow file name. You can only upload the file. App will not detect and
        open the files for you. When you click "Add to task" in targets preparation step,
        a file with "*targets.txt*" will be written to the workflow directory. So
        make sure to **rename your targets file if you do not want it be overwritten**.
        The same applies to workflow file preparation step, a file named "*workflow.Rmd*"
        will be written to the directory you have chosen.

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
wf_setupServer <- function(id, shared){
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
