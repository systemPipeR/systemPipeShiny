# UI
wf_setupUI <- function(id){
    ns <- NS(id)
    tagList(
        renderDesc(id = ns("desc"),
        '
        #### Set up a workflow environment
        To run a SPR workflow, a workflow environment is required. The
        environment is a directory containing all required files, like the
        [targets file{blk}](https://systempipe.org/sp/spr/gettingstarted/#structure-of-targets-file),
        the [workflow file{blk}](https://systempipe.org/sp/spr/templates/),
        and all other
        [parameter files{blk}](https://systempipe.org/sp/spr/gettingstarted/#structure-of-the-new-param-files-and-construct-sysargs2-container).

        The directory structure looks like this image below:

        ![spr-structure](https://systempipe.org/sp/spr/gettingstarted/SYSdir.png)

        Read more about this [workflow structure{blk}](https://systempipe.org/sp/spr/gettingstarted/#directory-structure).

        #### Template workflows
        SPR has some preconfigured workflows that you can generate in SPS with
        one click. Supported template workflows are: **2**. *RNASeq*, **3**. *VarSeq*, **4**. *RiboSeq*,
        **5**. *ChipSeq*. You can also choose an **6**. *existing* SPR workflow directory or create
        an **7**. *empty* SPR workflow directory.

        **1**. Example is a very tiny workflow. It basically just list the `session info`.

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
        # column(1),
        box(
            width = 12,
            collapsible = FALSE,
            closable = FALSE,
            title = "Initiate a workflow environment",
            id = "wf-setup_box",
            tags$img(id = "setup-box-img", src = 'sps/img/spr.png'),
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
                    selectizeInput(
                        inputId = ns("choose_wf"),
                        label = "Choose a workflow template",
                        choices = c(Example="eg", RNAseq="rnaseq", Varseq="varseq",
                                    Riboseq="riboseq", Chipseq="chipseq", Empty="new",
                                    Existing="exist"
                                    ),
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
                        ),
                    div(id = ns("loading_env"), style = "display:none", spsLoader())
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
                    textInput(inputId = ns("dir_show"), label = NULL,
                              placeholder=getwd(), width = "100%")
                ),
                div(
                    id = ns("exist_upload"),
                    tags$b("Choose targets file from workflow root folder:"),
                    div(class="input-group",
                        tags$label(class="input-group-btn input-group-prepend",
                                   shinyFiles::shinyFilesButton(
                                       ns("wf_targets_path"), "Browse",
                                       title = "Choose targets path",
                                       multiple = FALSE,
                                       buttonType = "btn btn-primary",
                                       icon = NULL)
                        ),
                        textInput(inputId = ns("targets_show"), label = NULL,
                                  placeholder="No file selected", width = "100%")
                    ),
                    tags$b("Choose workflow file from workflow root folder:"),
                    div(class="input-group",
                        tags$label(class="input-group-btn input-group-prepend",
                                   shinyFiles::shinyFilesButton(
                                       ns("wf_wf_path"), "Browse",
                                       title = "Choose workflow file path",
                                       multiple = FALSE,
                                       buttonType = "btn btn-primary",
                                       icon = NULL)
                        ),
                        textInput(inputId = ns("wf_show"), label = NULL,
                                  placeholder="No file selected", width = "100%")
                    )
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
                HTML("<li>Current version of SystemPipeR templates has some formatting
                     issues that will cause the workflow fail to run. Please read the
                     updates on <b>About</b> tab when we fix them.</li>"),
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
                HTML(
                "<li>
                You are selecting an existing SPR workflow project directory.
                Make sure you have the project folder's writing permission and
                it has subfolders: <b>'data'</b>, <b>'param'</b>, <b>'results'</b>.
                You are <b>required</b> to choose the <b>targets file</b> and <b>workflow file</b>
                location above. These two files should be located in your workflow root
                directory.
                </li>")
            )
        ),
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
            shinyWidgets::progressBar(
                id = ns("gen_wf_pg"), value = 0,
                title = "", total = 6
            )
        )
    )
}

# server
wf_setupServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "wf_setup"
        wf_path <- reactiveVal(NULL)
        wf_targets_path <- reactiveVal(NULL)
        wf_wf_path <- reactiveVal(NULL)
        # toggle elements in for warning
        observeEvent(input$choose_wf, {
            shinyjs::toggleElement(
                id = "warn_noneg", anim = TRUE,
                condition = !input$choose_wf %in% c("eg", "exist", "new"))
            shinyjs::toggleElement(
                id = "warn_empty", anim = TRUE,
                condition = input$choose_wf == "new")
            shinyjs::toggleElement(
                id = "warn_exist", anim = TRUE,
                condition = input$choose_wf == "exist")
            shinyjs::toggleElement(
                id = "exist_upload", anim = TRUE,
                condition = input$choose_wf == "exist")
        })
        # resolve dir path input

        if(spsOption("is_demo")) {
            dir_name <- paste0(sample(letters, 6), collapse = "")
            temp_dir <- file.path(tempdir(), dir_name)
            dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
            roots <- temp_dir
            names(roots) <- dir_name
            updateTextInput(session, "dir_show", placeholder = temp_dir)
            wf_path(temp_dir)
        } else {
            wf_path(getwd())
            roots <- c(current=getwd(), Home = normalizePath("~", mustWork = FALSE), shinyFiles::getVolumes()())
        }

        shinyFiles::shinyDirChoose(input, 'wf_path', roots = roots, session = session)
        observeEvent(input[['wf_path']], {
            req(is.list(input[['wf_path']]))
            dir_selected <- shinyFiles::parseDirPath(roots, input[['wf_path']])
            updateTextInput(inputId = 'dir_show',
                            session = session,
                            placeholder = unname(dir_selected))
            wf_path(dir_selected)
        })
        # resolve path for choose targets and wf an existing option
        observeEvent(c(input$choose_wf, wf_path()), {
            req(input$choose_wf == "exist")
            req(!is.null(wf_path()))
            roots <- c(`workflow folder`= wf_path())
            # targets
            shinyFiles::shinyFileChoose(
                input = input, id = 'wf_targets_path', roots = roots,
                restrictions = list.dirs(wf_path(), recursive = FALSE),
                filetypes = c("txt", "tsv", "csv")
            )
            observeEvent(input[['wf_targets_path']], {
                file_selected <- shinyFiles::parseFilePaths(roots, input[['wf_targets_path']])
                updateTextInput(inputId = 'targets_show',
                                session = session,
                                placeholder = unname(file_selected$datapath))
                wf_targets_path(file_selected$datapath)
            })
            # wf file
            shinyFiles::shinyFileChoose(
                input = input, id = 'wf_wf_path', roots = roots,
                restrictions = list.dirs(wf_path(), recursive = FALSE),
                filetypes = c("Rmd")
            )
            observeEvent(input[['wf_wf_path']], {
                file_selected <- shinyFiles::parseFilePaths(roots, input[['wf_wf_path']])
                updateTextInput(inputId = 'wf_show',
                                session = session,
                                placeholder = unname(file_selected$datapath))
                wf_wf_path(file_selected$datapath)
            })
        })
        ### action when gen WF clicked
        observeEvent(input$gen_env, {
            on.exit({
                shinyjs::hideElement('gen_wf_pg_panel', anim = TRUE)
                shinyjs::showElement("gen_env")
                shinyjs::hideElement("loading_env")
            })
            shinyjs::hideElement("gen_env")
            shinyjs::showElement("loading_env")

            # clear everything on start
            shared$wf$env_option <- shared$wf$env_path <- shared$wf$targets_path <- shared$wf$wf_path <- NULL
            shared$wf$flags$env_ready <- shared$wf$flags$targets_ready <-  shared$wf$flags$wf_ready <- 0
            shared$wf$all_ready <- 0

            # assertions
            updateProgressBar(session, "gen_wf_pg", 0, 6, title = "Checking path permission", status = "danger")
            shinyjs::showElement('gen_wf_pg_panel', anim = TRUE, time = 0.2)
            Sys.sleep(0.3)
            shinyCatch({
                if(!is.writeable(wf_path()))
                   stop("Path ", wf_path(), " is not writeable, check your permissions")
            }, blocking_level = "error")
            updateProgressBar(session, "gen_wf_pg", 1, 6, title = "check if the directory exists")
            Sys.sleep(0.3)
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
            Sys.sleep(0.3)
            # gen env
            shinyCatch({
                switch(input$choose_wf,
                    "exist" = {
                        lapply(c("data", "param", "results") %>% {paste0(final_env_path, .)}, function(x){
                            if(!dir.exists(x)){
                                stop("Required folder '", x, "' for an existing workflow is not there")
                            }
                        })
                        if(!emptyIsFalse(wf_targets_path())) stop("Targets file is empty")
                        if(!emptyIsFalse(wf_wf_path())) stop("Workflow file is empty")
                    },
                    "eg" = {
                        res <- list()
                        dir.create(file.path(final_env_path, "data"), recursive = TRUE)
                        dir.create(file.path(final_env_path, "param"), recursive = TRUE)
                        dir.create(file.path(final_env_path, "results"), recursive = TRUE)
                        # file.copy(system.file("app", "data", "targetsPE.txt", package = "systemPipeShiny"),
                        res[['1']] <- file.copy(system.file("extdata", "cwl", "gunzip", "targets_gunzip.txt", package="systemPipeR"),
                                  file.path(final_env_path, "targets_gunzip.txt"))
                        res[['2']] <- file.copy(system.file("app", "templates", "spr_simple_wf.Rmd", package = "systemPipeShiny"),
                                  file.path(final_env_path, "systemPipeExample.Rmd"))
                        res[['3']] <- file.copy(system.file("extdata", "cwl", package="systemPipeR"),
                                                file.path(final_env_path, "param"), recursive = TRUE)
                        if(!unlist(res) %>% all()) stop("Files not copied, see warnings")
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
                "exist" = wf_targets_path(),
                "eg" = normalizePath(file.path(final_env_path, "targets_gunzip.txt")),
                normalizePath(file.path(final_env_path, "targetsPE.txt"))
            ), blocking_level = "error")
            updateProgressBar(session, "gen_wf_pg", 4, 6, title = "update project info - workflow file")
            Sys.sleep(0.1)
            wf_file_path <- shinyCatch(switch(input$choose_wf,
                "rnaseq" = normalizePath(file.path(final_env_path, "systemPipeRNAseq.Rmd")),
                "varseq" = normalizePath(file.path(final_env_path, "systemPipeVARseq.Rmd")),
                "riboseq" = normalizePath(file.path(final_env_path, "systemPipeRIBOseq.Rmd")),
                "chipseq" = normalizePath(file.path(final_env_path, "systemPipeChIPseq.Rmd")),
                "exist" = wf_wf_path(),
                "eg" = normalizePath(file.path(final_env_path, "systemPipeExample.Rmd")),
                "new" = normalizePath(file.path(final_env_path, "new.Rmd"))
            ), blocking_level = "error")
            updateProgressBar(session, "gen_wf_pg", 5, 6, title = "update project info - shiny server")
            Sys.sleep(0.1)
            shared$wf$env_option <- input$choose_wf
            shared$wf$env_path <- final_env_path
            shared$wf$targets_path <- targes_path %>% unname()
            shared$wf$wf_path <- wf_file_path %>% unname()
            shared$wf$flags$env_ready <- isolate(shared$wf$flags$env_ready) + 1
            updateProgressBar(session, "gen_wf_pg", 6, 6, title = "All done", status = "success")
            Sys.sleep(0.3)
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
            ## panel icon change with js
            observeEvent(shared$wf$env_option, {
                env_names <- c(Example="eg", RNAseq="rnaseq", Varseq="varseq",
                           Riboseq="riboseq", Chipseq="chipseq", Existing="exist",
                           Empty="new")
                env_name <- names(env_names[env_names == shared$wf$env_option])
                session$sendCustomMessage(
                    type = 'change-panel-icon',
                    message = list(choice = env_name)
                )
            })
        })
        observeEvent(input$confirm_next, {
            req(input$confirm_next)
            shinyjs::runjs("$('#wf-wf_panel-1-heading > h4').trigger('click');")
        })
    }
    moduleServer(id, module)
}
