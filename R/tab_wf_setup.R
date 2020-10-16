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
        one click. Supported template workflows are: **2**. *RNASeq*, **3**. *VarSeq*, **4**. *RiboSeq*,
        **5**. *ChipSeq*. You can also choose an **6**. *existing* SPR workflow directory or create
        an **7**. *empty* SPR workflow directory.

        **1**. Example is a very tiny workflow with only 2 steps, one commandline
        step, one R step. You need to have a default terminal which has `echo`
        to work, bash for Linux and cmd or powershell for Windows for example.

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
        column(1),
        boxPlus(
            width = 10,
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
                                    Riboseq="riboseq", Chipseq="chipseq", Existing="exist",
                                    Empty="empty"),
                        options = list(style = "btn-primary")
                    )
                ),
                column(
                    6,
                    actionButton(ns("gen_env"), "Gen workflow", style = "margin-top: 25px;") %>%
                        bsHoverPopover(
                            "Start a workflow environment",
                            "Clicking here will create a workflow environment folder for you.",
                            "bottom"
                        )
                )
            ),
            fluidRow(
                class = "form-group shiny-input-container sps-file center-block",
                tags$label(class="control-label", `for`=ns("exist_wf"),
                           "Select where you want to create a new workflow or an existing workflow directory"),
                p("Default is current directory."),
                div(class="input-group",
                    tags$label(class="input-group-btn input-group-prepend",
                               shinyFiles::shinyDirButton(
                                   ns("wf_path"), "Browse",
                                   title = "",
                                   buttonType = "btn btn-primary",
                                   icon = NULL)
                    ),
                    textInput(inputId = ns("exist_show"), label = NULL,
                              placeholder=getwd(), width = "100%")
                )
            ),
            if(Sys.info()['sysname'] == "Windows"){
                tags$ul(
                    class = "text-danger",
                    HTML("<li>Your host system is Windows, Most bioinformatics command
                     line tools <strong>DO NOT</strong> work on Windows.
                     Only the example workflow will work.</li>")
                )
            } else div(),
            tags$ul(
                class = "text-danger", id = ns("warn_noneg"),
                tags$li("This will run a non-example workflow.
                If the required command
                line tools are not installed, the workflow will fail."),
                HTML("<li>Most other pre-configed template workflows like RNAseq,
                Varseq provide more than one alignment, calling, and other methods. You
                only need to choose one method on certain steps. Make sure to select
                the desired method steps in the step <strong>3. Workflow File</strong>.
                Using the defualt workflow file without
                any custom selection is not recommended.</li>")
            ),
            tags$ul(
                class = "text-danger", id = ns("warn_empty"),
                tags$li("This option will generate an empty workflow folder.
                        The workflow file only has a header. You need to write
                        your own code.")
            )
        ),
        column(1)
    )

}

# server
wf_setupServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "wf_setup"
        # toggle elements in for warning
        observeEvent(input$choose_wf, {
            shinyjs::toggleElement(
                id = "warn_noneg", anim = TRUE,
                condition = input$choose_wf != "eg")
            shinyjs::toggleElement(
                id = "warn_empty", anim = TRUE,
                condition = input$choose_wf == "empty")
        })
        # resolve dir path input
        roots <- c(current=getwd(), Home = normalizePath("~", mustWork = FALSE), shinyFiles::getVolumes()())
        shinyFiles::shinyDirChoose(input, 'wf_path', roots = roots, session = session)
        wf_path <- reactive(getwd())
        observeEvent(input[['wf_path']], {
            req(is.list(input[['wf_path']]))
            dir_selected <- shinyFiles::parseDirPath(roots, input[['wf_path']])
            updateTextInput(inputId = 'exist_show',
                            session = session,
                            placeholder = unname(dir_selected))
            wf_path(file_selected)
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
