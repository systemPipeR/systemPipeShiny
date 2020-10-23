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
        to work, bash for Linux or Mac and powershell for Windows for example.

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
        tags$script(src="sps/js/sps_setup_box_img.js"),
        # column(1),
        boxPlus(
            width = 12,
            collapsible = FALSE,
            closable = FALSE,
            title = "Initiate a workflow environment",
            id = "wf-setup_box",
            tags$img(id = "setup-box-img", src = 'sps/img/sps.png'),
            tags$style("
            #wf-setup_box {
              height: 500px;
              position: relative;
            }
           #setup-box-img {
              position: absolute;
              display: inline-block;
              height: 250px;
              width: 300px;
              align-items: center;
              text-align: center;
              top: 50%;
              left: 50%;
              transform: translate(-50%, 0);
              opacity: 0.25;
            }
            #wf-setup_box.box-body::before  {
              display: block !important;
            }
            "),
            fluidRow(
                column(
                    6,
                    shinyWidgets::pickerInput(
                        inputId = ns("choose_wf"),
                        label = "Choose a workflow template",
                        choices = c(Example="eg", RNAseq="rnaseq", Varseq="varseq",
                                    Riboseq="riboseq", Chipseq="chipseq", Existing="exist",
                                    Empty="new"),
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
                tags$label(class="control-label",
                           "Select where you want to create a new workflow or use an existing workflow directory"),
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
            ),
            tags$ul(
                class = "text-danger", id = ns("warn_exist"),
                tags$li("You are selecting an existing SPR workflow project
                        directory. Make sure you have the project folder's
                        writing permission and it has subfolders:
                        'data', 'param', and 'results'")
            )
        ),
        # column(1),
        absolutePanel(
            id = ns("gen_wf_pg_panel"),
            style = "background-color: #ecf0f5; border: 2px solid #d2d6de; border-radius: 5%; display: none;",
            top = "40%",
            left = "45%",
            width = "400px",
            height = "100px",
            fixed = FALSE,
            cursor = "default",
            h4("Workflow Generation Progress", style="text-align: center"), br(),
            progressBar(
                id = ns("gen_wf_pg"), value = 0,
                title = "", total = 6
            )
        )
    )
}

# server
#' @importFrom systemPipeRdata genWorkenvir
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
                condition = input$choose_wf == "new")
            shinyjs::toggleElement(
                id = "warn_exist", anim = TRUE,
                condition = input$choose_wf == "exist")
        })
        # resolve dir path input
        roots <- c(current=getwd(), Home = normalizePath("~", mustWork = FALSE), shinyFiles::getVolumes()())
        shinyFiles::shinyDirChoose(input, 'wf_path', roots = roots, session = session)
        wf_path <- reactiveVal(getwd())
        observeEvent(input[['wf_path']], {
            req(is.list(input[['wf_path']]))
            dir_selected <- shinyFiles::parseDirPath(roots, input[['wf_path']])
            updateTextInput(inputId = 'exist_show',
                            session = session,
                            placeholder = unname(dir_selected))
            wf_path(dir_selected)
        })

        ### action when gen WF clicked
        observeEvent(input$gen_env, {
            on.exit({shinyjs::hideElement('gen_wf_pg_panel', anim = TRUE)})
            # clear everything on start
            shared$wf$env_option <- NULL
            shared$wf$env_path <- NULL
            shared$wf$targets_path <- NULL
            shared$wf$wf_path <- NULL
            shared$wf$flags$env_ready <- FALSE
            shared$wf$flags$targets_ready <- FALSE
            shared$wf$flags$wf_ready <- FALSE
            shared$wf$all_ready <- FALSE

            # assertions
            updateProgressBar(session, "gen_wf_pg", 0, 6, title = "Checking path permission", status = "danger")
            shinyjs::showElement('gen_wf_pg_panel', anim = TRUE, time = 0.2)
            Sys.sleep(0.5)
            shinyCatch({
                if(!is.writeable(wf_path()))
                   stop("Path ", wf_path(), " is not writeable, check your permissions")
            }, blocking_level = "error")
            updateProgressBar(session, "gen_wf_pg", 1, 6, title = "check if the directory exists")
            Sys.sleep(0.5)
            final_env_path <- switch(
                    input$choose_wf,
                    "eg" = "spr_example_wf",
                    "exist" = "",
                    input$choose_wf
            ) %>% {file.path(isolate(wf_path()), .)}
            shinyCatch({
                if(dir.exists(final_env_path) & input$choose_wf != "exist")
                    stop("Folder ", final_env_path, " is already there, cannot ",
                         "create the workflow. Use 'Existing' option or rename it.")
            }, blocking_level = "error")
            updateProgressBar(session, "gen_wf_pg", 2, 6, title = "start to create files or check files for existing WF")
            Sys.sleep(0.5)
            # gen env
            shinyCatch({
                switch(input$choose_wf,
                    "exist" = {
                        lapply(c("data", "param", "results") %>% {paste0(final_env_path, .)}, function(x){
                            if(!dir.exists(x)){
                                stop("Required folder '", x, "' for an existing workflow is not there")
                            }
                        })
                    },
                    "eg" = {
                        dir.create(file.path(final_env_path, "data"), recursive = TRUE)
                        dir.create(file.path(final_env_path, "param", "cwl"), recursive = TRUE)
                        dir.create(file.path(final_env_path, "results"), recursive = TRUE)
                        file.copy(system.file("app", "data", "targetsPE.txt", package = "systemPipeShiny"),
                                  file.path(final_env_path, "targetsPE.txt"))
                        file.copy(system.file("app", "data", "example_wf.md", package = "systemPipeShiny"),
                                  file.path(final_env_path, "systemPipeExample.Rmd"))
                    },
                    systemPipeRdata::genWorkenvir(input$choose_wf, mydirname = final_env_path)
                )
            },
            blocking_level = "error")
            updateProgressBar(session, "gen_wf_pg", 3, 6, title = "update project info - targets", status = "warning")
            Sys.sleep(0.1)
            # post updates
            targes_path <- shinyCatch(switch(input$choose_wf,
                "chipseq" = normalizePath(file.path(final_env_path, "targetsPE_chip.txt")),
                "new" = normalizePath(file.path(final_env_path, "targets.txt")),
                "exist" = "upload_required",
                normalizePath(file.path(final_env_path, "targetsPE.txt"))
            ), blocking_level = "error")
            updateProgressBar(session, "gen_wf_pg", 4, 6, title = "update project info - workflow file")
            Sys.sleep(0.1)
            wf_file_path <- shinyCatch(switch(input$choose_wf,
                "rnaseq" = normalizePath(file.path(final_env_path, "systemPipeRNAseq.Rmd")),
                "varseq" = normalizePath(file.path(final_env_path, "systemPipeVARseq.Rmd")),
                "riboseq" = normalizePath(file.path(final_env_path, "systemPipeRIBOseq.Rmd")),
                "chipseq" = normalizePath(file.path(final_env_path, "systemPipeChIPseq.Rmd")),
                "exist" = "upload_required",
                "eg" = normalizePath(file.path(final_env_path, "systemPipeExample.Rmd")),
                "new" = normalizePath(file.path(final_env_path, "new.Rmd"))
            ), blocking_level = "error")
            updateProgressBar(session, "gen_wf_pg", 5, 6, title = "update project info - shiny server")
            Sys.sleep(0.1)
            shared$wf$env_option <- input$choose_wf
            shared$wf$env_path <- final_env_path
            shared$wf$targets_path <- targes_path
            shared$wf$wf_path <- wf_file_path
            shared$wf$flags$env_ready <- TRUE
            updateProgressBar(session, "gen_wf_pg", 6, 6, title = "All done", status = "success")
            Sys.sleep(0.5)
            # jump to next step
            shinyWidgets::confirmSweetAlert(
                session = session,
                inputId = ns("confirm_next"),
                title = "Workflow environment setup done!",
                closeOnClickOutside = FALSE,
                html = TRUE,
                type = "success",
                text = HTML(glue(
                    "
                    <ul class='text-left'>
                      <li><b>The workflow environment is located at</b>: {shared$wf$env_path}</li>
                      <li><b>The targets file is located at</b>: {shared$wf$targets_path}</li>
                      <li><b>The workflow file is located at</b>: {shared$wf$wf_path}</li>
                    </ul>
                    <h3>Do you want to proceed to the next step?</h3>
                    "
                ))
            )
        })
        observeEvent(input$confirm_next, {
            req(input$confirm_next)
            shinyjs::runjs("$('#wf-wf_panel-1-heading > h4').trigger('click');")
        })
    }
    moduleServer(id, module)
}
