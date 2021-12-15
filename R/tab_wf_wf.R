## UI
#' @noRd
wf_wfUI <- function(id){
    ns <- NS(id)
    tagList(
        # actionButton(ns("set"), "set"), # dev shortcut
        div(
            id = "wf_wf_displayed",
            style = "display:none",
            tabTitle("Workflow"),
            renderDesc(id = ns("desc"),
            '
            #### Workflow designer
            This is the workflow designer. Here you can add/remove/modify workflow
            steps and visualize the entire workflow.
            ***

            ##### How to use it
            It is best to start with some of the template workflows that can be
            generated from the from SPS workflow module Step 1. If you have selected
            one of the non-empty templates, like example, RNAseq etc., you should
            now see some steps are display on the left designer.

            1.You can drag to change the order, or drag to the <i class="fa fa-trash-alt"></i>
            to delete a step.
            2. Use <i class="fa fa-undo-alt"></i> undo or <i class="fa fa-redo-alt"></i> redo
            to restore your actions.
            3. You can use <i class="fa fa-cog"></i> to configure each step.
            4. Use <i class="fa fa-plus"></i> to create a new step. You can create either
            an R step or a sysArgs step. the latter has more settings, you may want
            to read the manual of SPS or SPR before that.
            5. Use <i class="fa fa-expand-arrows-alt"></i> to enlarge the left or
            right panel to have a better view.
            ***

            #### Visualize the workflow
            You can see the workflow dependency graph on the right panel. It may
            change if you make some actions, like changing the step
            dependencies.
            ***

            #### Fix dependencies
            Most steps will have dependencies, they are very important for a workflow
            to run. The dependency graph is pre-configured for template workflows.
            If you ever add/remove/change order of a step, the dependency graph may
            fail. You then will see steps are marked in red. You **must fix** the dependencies
            of these steps by clicking <i class="fa fa-cog"></i> before continuing to run
            the workflow.
            ***

            If you are not sure how to fix, look at the workflow plot on the right.
            It is a good guide to build the dependency graph.

            ##### Add to task
            Clicking this <i class="fa fa-save"></i> will add the workflow file to the SPS task, it will
            be used in **step 5**. You need to have at least **one** step and **fix dependency problems**
            to enable this button. This is the final button to click after everything is done.
            ***

            #### Other information
            ##### Workflow templates
            In SPR, workflows are defined as Rmarkdown files,
            you can read details and obtain them
            [here{blk}](https://systempipe.org/sp/spr/templates/). This step can
            help you choose/ skip some steps. Make a workflow diagram to see how
            the order SPR execute the workflow and take a preview of the
            final report. If you just want to use all the defaults, simply clicking
            the <i class="fa fa-save"></i>.

            ##### Detailed manual
            A manual with screenshots and details is on [our website{blk}](https://systempipe.org/sps/modules/workflow/)
            '),
            spsHr(),
            tags$script(src="https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.js"),
            tags$script(src="sps/js/sps_wf_wf.js"),
            tags$link(rel="stylesheet", href="sps/css/sps_wf_wf.css"),
            fluidRow(
                shinydashboardPlus::box(
                    width = 6, id = ns("step_container"), title = "Workflow step designer",
                    solidHeader = TRUE, status = "primary",
                    div(
                        id = ns("step_box"),
                        animateIcon(
                            id=ns("step_enlarge"),"expand-arrows-alt", animation = "pulse",
                            size = "lg", class= "enlarge-icon",
                            hover = TRUE, enlarge_target= paste0('#', ns('step_box')), enlarged='false'
                        ) %>%
                            bsTip("Enlarge the step designer", "bottom", "info"),
                        uiOutput(ns("sort_box")),
                        br(), br(), br(), spsHr(other_color = "rgb(2, 117, 216, 0.5)"),
                        div(
                            class = 'step-box-control',
                            tags$button(class="fa fa-undo-alt shiny-bound-input action-button", id=ns("step_undo"), style="color: #3c8dbc") %>%
                                bsTip("Undo", placement = "bottom"),
                            tags$button(class="fa fa-redo-alt shiny-bound-input action-button", id=ns("step_redo"), style="color: #3c8dbc") %>%
                                bsTip("Redo", placement = "bottom"),
                            div(
                                class = "wf-history-panel",
                                tags$table(
                                    tags$tr(tags$td("Last history:"), tags$td("1")),
                                    tags$tr(tags$td("Current history:"), tags$td("2")),
                                    tags$tr(tags$td("Next history:"), tags$td("3"))
                                )
                            ),
                            tags$i(class="fa fa-plus shiny-bound-input action-button", id=ns("step_new"), style="color: #5cb85c;") %>%
                                bsTip("Add a new step", placement = "bottom", status = "success"),
                            div(id=ns("step_trash"), class="step-trash", tags$span(), tags$i()) %>%
                                bsTip("Drag here to delete a step", placement = "bottom", status = "danger"),
                            tags$button(class="fa fa-save shiny-bound-input action-button", id=ns("totask"))%>%
                                bsPop("Add to SPS task", placement = "bottom",
                                      "Send the workflow to SPS workflow module manager so you can run it.")
                        )
                    )
                ),
                shinydashboardPlus::box(
                    width = 6, id = ns("wf_plot_container"), title = "Workflow Dependency Plot",
                    solidHeader = TRUE, status = "primary",
                    div(
                        id =ns("wf_plot_box"),
                        animateIcon(
                            id=ns("wf_plot_enlarge"),"expand-arrows-alt", animation = "pulse",
                            size = "lg", class= "enlarge-icon",
                            hover = TRUE, enlarge_target=paste0('#', ns('wf_plot_box')), enlarged='false'
                        ) %>%
                            bsTip("Enlarge workflow plot", "bottom", "info"),
                        systemPipeR::plotwfOutput(outputId = ns("wf_plot")), br(), br()
                    ),
                    heightMatcher(ns("wf_plot_container"), ns("step_container"))
                )
            )
        ),
        div(
            id = "wf_wf_disable",
            h3("Comfirm your targets from step 2 frist",
               style = "text-center text-warning")
        )
    )
}

## server
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom shinyjs runjs enable disable
#' @noRd
wf_wfServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        renderSort <- renderUI({
            sal <- his$get()$item$sal
            sal_stat <- statSal(sal)
            destoryOb(wf_share$config_ob)
            # wf_share$config_ob <- makeConfig(sal, sal_stat$names, sal_stat$deps, session, input, output, ns)
            makeSort(sal_stat$names, sal_stat$type, ns)
        })
        # init ----
        ### dev shortcut ####
        # observeEvent(input$set, {
        #     shared$wf$flags$targets_ready <- 1
        #     shared$wf$env_path <- file.path(getwd(), "spr_example_wf")
        #     shared$wf$sal <- my_sal
        #     wf_share$config_ob <- NULL
        #     his$clear()
        #     his$add(list(
        #         sal = my_sal,
        #         msg = "Initial sal"
        #     ))
        #     savehis(savehis() + 1)
        # }, once = TRUE)
        ########
        wf_share <- reactiveValues()
        # start history stack
        his <- historyStack$new(verbose = spsOption("verbose"),limit = 100)
        # save envet trigger
        savehis <- reactiveVal(0)
        observeEvent(shared$wf$flags$targets_ready, priority = 2L, {
            # config obs
            wf_share$config_ob <- NULL
            req(shared$wf$sal)
            # my_sal <<-shared$wf$sa
            his$clear()
            his$add(list(
              sal = shared$wf$sal,
              msg = "New sal created"
            ))
            savehis(savehis() + 1)
        })


        # new step ----
        observeEvent(input$step_new, {
            req(input$step_new)
            newStepMain(his$get()$item$sal, ns)
        }, ignoreInit = TRUE)
        # render modal based on new step type ----
        observeEvent(input$new_step_choose, ignoreInit = TRUE, {
            req(input$new_step_choose)
            req(input$new_step_type)
            req(input$new_step_index)
            sal <- his$get()$item$sal
            sal_stat <- statSal(sal)
            dep_choice <- sal_stat$names[seq(as.numeric(input$new_step_index))]

            if(input$new_step_type == "r") {
                showModal(modalDialog(
                    size = "xl",
                    footer = tagList(
                        modalButton("Cancel"),
                        actionButton(ns("new_step_back"), "Back"),
                        actionButton(ns("new_step_r_save"), "Save"),
                    ), {
                        div(
                            spsTitle("Type the R code of this step below", "4"),
                            shinyAce::aceEditor(
                                ns("new_code_r"), fontSize = 14, value = "",
                                mode = "r", wordWrap = TRUE, debounce = 100, height = "250px",
                                placeholder = "Write some R code"
                            ),
                            spsTitle("Type step name", "4"),
                            p("Must be different than these names:"),
                            tags$pre(paste(sal_stat$names, collapse = ", ")),
                            textInput(
                                ns("new_step_name"), "",
                                paste0("step", input$new_step_index, "_", sample(letters, 3) %>% paste0(collapse = ""))
                            ),
                            spsTitle("Choose dependency", "4"),
                            p(
                                "This step will be inserted to the index you selected.
                                You may change the order later but remember to fix the dependency. ",
                                tags$span(class="text-danger", "Adding a downstream step as dependency is not allowed.")
                            ),
                            selectizeInput(
                                ns("new_step_dep"), "", multiple = TRUE, choices = dep_choice[seq_len(length(dep_choice) - 1)],
                                selected = dep_choice[length(dep_choice) - 1]
                            ),
                            spsTitle("Required?", "4"),
                            p("Is this step mandatory or optional?"),
                            selectizeInput(ns("new_step_req"), "",choices = c("mandatory", "optional")),
                            spsTitle("Run time location", "4"),
                            p("Where will this step be run, the same management session or a child
                              compute node?"),
                            selectizeInput(ns("new_step_session"), "", choices = c("management", "compute"))
                        )
                    }))
            } else {
                showModal(modalDialog(
                    size = "xl",
                    footer = tagList(
                        modalButton("Cancel"),
                        actionButton(ns("new_step_back"), "Back"),
                        actionButton(ns("new_step_sys_save"), "Save"),
                    ), {
                        tabsetPanel(
                            tabPanel(
                                "Basic Arguments",
                                spsTitle("Type step name", "4"),
                                p("Must be different than these names:"),
                                tags$pre(paste(sal_stat$names, collapse = ", ")),
                                textInput(
                                    ns("new_step_name"), "",
                                    paste0("step", input$new_step_index, "_", sample(letters, 3) %>% paste0(collapse = ""))
                                ),
                                spsTitle("Choose dependency", "4"),
                                p("This step will be inserted to the index you selected.
                                  You may change the order later but remember to fix the dependency. ",
                                tags$span(class="text-danger", "Adding a downstream step as dependency is not allowed.")),
                                selectizeInput(
                                    ns("new_step_dep"), "", multiple = TRUE, choices = dep_choice[seq_len(length(dep_choice) - 1)],
                                    selected = dep_choice[length(dep_choice) - 1]
                                ),
                                spsTitle("sub-directory", "4"),
                                p("Create a sub-dir inside results to store outputs from this step?"),
                                shinyWidgets::awesomeCheckbox(ns("new_sys_dir"), "sub-directory?", value = TRUE, status = "primary"),
                                spsTitle("Required?", "4"),
                                p("Is this step mandatory or optional?"),
                                selectizeInput(ns("new_step_req"), "",choices = c("mandatory", "optional")),
                                spsTitle("Run time location", "4"),
                                p("Where will this step be run, the same management session or a child
                                    compute node?"),
                                selectizeInput(ns("new_step_session"), "", choices = c("management", "compute"))
                            ),
                  tabPanel(
                      "CWL Arguments",
                      spsTitle("Choose targets connection", "4"),
                      p("It can be a previous step(s) or comes from a fresh file"),
                      shinyWidgets::radioGroupButtons(
                          justified = TRUE,
                          ns("new_sys_t_source"), "", choices = c(`Previous Step`="step", `New File`="upload"), status = "primary",
                          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                      ),
                      conditionalPanel(
                          "input.new_sys_t_source == 'step'", ns = ns,
                          selectizeInput(
                              ns("new_step_t_con"), "Choose targets connection from a step(s)", multiple = TRUE, choices = dep_choice[seq_len(length(dep_choice) - 1)]
                          )
                      ),
                      conditionalPanel(
                          "input.new_sys_t_source == 'upload'", ns = ns,
                          dynamicFile(ns("new_sys_t_path"), "Choose new targets file from server", mode = "local")
                      ),
                      DT::DTOutput(ns("step_sys_t_df")),
                      spsTitle("Choose CWL file", "4"),
                      selectInput(
                          ns("new_sys_cwl"), "", multiple = FALSE,
                          choices = fs::dir_ls(file.path(shared$wf$env_path, "param", "cwl"), recurse = TRUE, regexp = "\\.cwl$") %>% str_remove(file.path(shared$wf$env_path, "param", "cwl/"))
                      ),
                      spsTitle("Choose input yaml file for CWL", "4"),
                      selectInput(
                          ns("new_sys_yaml"), "", multiple = FALSE,
                          choices = fs::dir_ls(file.path(shared$wf$env_path, "param", "cwl"), recurse = TRUE, regexp = "\\.y[a]{0,}ml$") %>% str_remove(file.path(shared$wf$env_path, "param", "cwl/"))
                      ),
                      spsTitle("inputVar: targets -> yaml replacement", "4"),
                      p("You need to have targets table displayed in the `Basic Arguments`
                    tab and the proper yaml file (some yaml files have no varaibles)
                    to have the options shown below.", class="text-warning"),
                    tags$ul(
                        tags$li(id = ns("require_t_con"), class="text-danger", "Require valid target connections"),
                        tags$li(id = ns("require_yaml"), class="text-danger", "Require a yaml file with inputVar variables")
                    ),
                    p("Choose a column from a targets to replace a variables in CWL yaml you have chosen"),
                    fluidRow(id = ns("cwl_var")),
                    spsHr(),
                    spsTitle("Raw rendered commands", "4"),
                    actionButton(ns("rerender_cmd"), "Render", icon= animateIcon("sync-alt")),
                    verbatimTextOutput(ns("new_step_sys_cmd"))
                  )
                        )
                    }))
            }
        })
        # back is clicked ----
        observeEvent(input$new_step_back, ignoreInit = TRUE, {
            newStepMain(his$get()$item$sal, ns)
        })
        # sys step ----
        new_step_sys_t_path <- dynamicFileServer(
            input, session, id = "new_sys_t_path", mode = "local",
            roots = c(app = spsOption("app_path")))

        sys_t_con <- reactive({
            req(input$new_sys_t_source)
            if(input$new_sys_t_source == "step") return(input$new_step_t_con)
            req(new_step_sys_t_path())
            new_step_sys_t_path()$datapath %>% basename()
        }) %>% debounce(2000)
        sys_bind_df <- reactiveVal()
        cwl_input_vars <- reactive({
            req(input$new_sys_yaml)
            file_content <- readLines(file.path(shared$wf$env_path, "param", "cwl", input$new_sys_yaml))
            file_content %>%
                str_split("\n") %>%
                unlist() %>%
                {.[str_detect(., ".*:")]} %>%
                {.[str_detect(., ":\\s{0,}_[a-zA-Z0-9_]+_\\s{0,}$")]} %>%
                str_extract("_[a-zA-Z0-9_]+_")
        }) %>% debounce(1000)

        observeEvent(sys_t_con(), ignoreInit = TRUE, ignoreNULL = FALSE, {
            if(is.null(sys_t_con())) return({
                output$step_sys_t_df <- DT::renderDT({DT::datatable(data.frame())})
                sys_bind_df(NULL)
            })
            sal <- his$get()$item$sal
            sal_stat <- statSal(sal)
            shinyCatch(blocking_level = "error", {
                if(input$new_sys_t_source == "step") {
                    ## handle outfiles
                    outfiles <- systemPipeR::outfiles(sal)[sys_t_con()] %>%
                        lapply(as.data.frame) %>%
                        {.[lapply(., function(x) nrow(x) > 0) %>% unlist()]}
                    if(length(outfiles) > 1) {
                        outfiles_length <- lapply(outfiles, nrow) %>% unlist()
                        if(
                            (length(unique(outfiles_length)) > 2) ||
                            (outfiles_length[1] != mean(outfiles_length)) &&
                            (!1 %in% outfiles_length)
                        ) {
                            stop("Steps you selected have different Sample length in outfiles, cannot use these steps as targets connections")
                        }
                    }
                    outfiles <- dplyr::bind_cols(outfiles)
                    ## handle targets
                    targets <- systemPipeR::targetsWF(sal)[sys_t_con()] %>%
                        lapply(as.data.frame) %>%
                        {.[lapply(., function(x) nrow(x) > 0) %>% unlist()]}
                    if(length(targets) > 1) {
                        targets_length <- lapply(targets, nrow) %>% unlist()
                        if(
                            (length(unique(targets_length)) > 2) ||
                            (targets_length[1] != mean(targets_length)) &&
                            (!1 %in% targets_length)
                        ) {
                            stop("Steps you selected have different Sample length in targets, cannot use these steps as targets connections")
                        }
                        targets <- lapply(seq_along(targets), function(x){
                            if(x == 1) return(targets[[x]])
                            names(targets[[x]]) <- paste0(names(targets[[x]]), "_", names(targets)[x])
                            targets[[x]]
                        })
                    }
                    targets <- dplyr::bind_cols(targets)
                    ## merge both
                    df <-  if(length(outfiles) == 0 && length(targets) != 0) {
                        targets
                    } else if(length(outfiles) != 0 && length(targets) == 0) {
                        outfiles
                    } else if(length(outfiles) == 0 && length(targets) == 0) {
                        stop("Selected steps don't have any targets or outfiles, try to choose other steps as targets connections")
                    } else {
                        if((nrow(targets) != nrow(outfiles)) && (nrow(outfiles) != 1)) {
                            stop("Step(s) you selected have different length in",
                                 "outfiles and targets dataframes or the outfiles",
                                 "length is not 1, this is not allowed")
                        }
                        dplyr::bind_cols(targets, outfiles)
                    }
                } else {
                    if(!file.exists(file.path(shared$wf$env_path, sys_t_con())))
                        stop("You can only choose files inside current SPR workflow project folder, not in SPS app root")
                    df <- read.delim(file.path(shared$wf$env_path, sys_t_con()), comment.char = "#", sep = "\t")
                }
                output$step_sys_t_df <- DT::renderDT({DT::datatable(df, options = list(searching= FALSE, scrollX = TRUE), class = "compact")})
                sys_bind_df(df)
                shinyCatch(message("New targets table created."))
            })
        })

        observeEvent(c(cwl_input_vars(), sys_bind_df()),{
            shinyjs::toggleElement("require_t_con", condition = !emptyIsFalse(sys_bind_df()), anim = TRUE)
            shinyjs::toggleElement("require_yaml", condition = !emptyIsFalse(cwl_input_vars()), anim = TRUE)
            removeUI(
                selector = glue('#{ns("cwl_var")} div'),
                multiple = TRUE
            )
            req(emptyIsFalse(cwl_input_vars()))
            req(emptyIsFalse(sys_bind_df()))
            for(i in seq_along(cwl_input_vars())){
                insertUI(
                    glue('#{ns("cwl_var")}'),
                    where = "beforeEnd",
                    ui =  column(3, selectizeInput(
                        inputId = ns(paste0("cwl_var-", i)),
                        label = cwl_input_vars()[i],
                        choices = c("Not required", names(sys_bind_df())),
                        options = list(style = "btn-primary")
                    ))
                )
            }
            shinyCatch(message("New inputVar options created."))
        })
        ## render raw cmd
        observeEvent(input$rerender_cmd, ignoreInit = TRUE, {
            req(input$new_sys_cwl)
            req(input$rerender_cmd)
            sal <- his$get()$item$sal
            sal <- shinyCatch({
                # parse replacement
                replace_cols <- lapply(seq_along(cwl_input_vars()), function(i){
                    value <- input[[paste0("cwl_var-", i)]]
                }) %>% unlist()
                not_required <- replace_cols == "Not required"
                inputvars <- cwl_input_vars()
                names(inputvars) <- replace_cols
                inputvars <- inputvars[!not_required]
                if(length(names(inputvars)) != length(unique(names(inputvars))))
                    stop("Each target column can only be used once for inputvars")
                inputvars <- if(emptyIsFalse(inputvars[1])) inputvars else NULL
                targets <- if(emptyIsFalse(sys_t_con()) && length(inputvars) !=0) sys_t_con() else NULL
                if(emptyIsFalse(targets) && !emptyIsFalse(inputvars[1])) stop(
                    "Targets connection is not empty but you have no inputVar replacement selected."
                )
                # parsing
                on.exit(setwd(spsOption("app_path")), add = TRUE)
                setwd(shared$wf$env_path)
                systemPipeR::appendStep(sal, after = 0) <- systemPipeR::SYSargsList(
                    targets = targets, step_name = "dummy_step",
                    wf_file = input$new_sys_cwl,
                    input_file = input$new_sys_yaml,
                    dir_path = "param/cwl",
                    inputvars = inputvars
                )
                sal
            }, blocking_level = "error")
            output$new_step_sys_cmd <- renderPrint({
                print(suppressMessages(systemPipeR::cmdlist(sal))[['dummy_step']] %>% unlist() %>% unname())
            })
            shinyCatch(message("Raw commandline code generated."))
        })

        # sys step new save ----
        observeEvent(input$new_step_sys_save, {
            req(input$new_step_sys_save)
            sal <- his$get()$item$sal
            shinyCatch({
                # parse replacement
                replace_cols <- lapply(seq_along(cwl_input_vars()), function(i){
                    value <- input[[paste0("cwl_var-", i)]]
                }) %>% unlist()
                not_required <- replace_cols == "Not required"
                inputvars <- cwl_input_vars()
                names(inputvars) <- replace_cols
                inputvars <- inputvars[!not_required]
                if(length(names(inputvars)) != length(unique(names(inputvars))))
                    stop("Each target column can only be used once for inputvars")
                inputvars <- if(emptyIsFalse(inputvars[1])) inputvars else NULL
                targets <- if(emptyIsFalse(sys_t_con()) && length(inputvars) !=0) sys_t_con() else NULL
                if(emptyIsFalse(targets) && !emptyIsFalse(inputvars[1])) stop(
                    "Targets connection is not empty but you have no inputVar replacement selected."
                )
                if(!emptyIsFalse(input$new_step_name)) stop("Step name is empty")
                on.exit(setwd(spsOption("app_path")), add = TRUE)
                setwd(shared$wf$env_path)
                systemPipeR::appendStep(sal, after = as.numeric(input$new_step_index)) <- systemPipeR::SYSargsList(
                    targets = targets,
                    dir = input$new_sys_dir,
                    wf_file = input$new_sys_cwl,
                    input_file = input$new_sys_yaml,
                    dir_path = "param/cwl",
                    step_name = input$new_step_name,
                    inputvars = inputvars,
                    dependency = if(is.null(input$new_step_dep)) "" else input$new_step_dep,
                    run_step = input$new_step_req,
                    run_session = input$new_step_session
                )
                his$add(list(sal = sal, msg = "New sysArgs step"))
            }, blocking_level = "error")
            step_order_change(FALSE)
            savehis(savehis() + 1)
            removeModal()
            shinyCatch(message("New sysArgs step saved."))
        }, ignoreInit = TRUE)

        # R step new save ----
        observeEvent(input$new_step_r_save, {
            req(input$new_step_r_save)
            sal <- his$get()$item$sal
            shinyCatch(blocking_level = "error", {
                if(!emptyIsFalse(input$new_code_r)) stop("R code is empty")
                if(!emptyIsFalse(input$new_step_name)) stop("Step name is empty")
                options(linewise_importing = TRUE)
                on.exit(setwd(spsOption("app_path")), add = TRUE)
                setwd(shared$wf$env_path)
                systemPipeR::appendStep(sal, after = as.numeric(input$new_step_index) - 1) <- systemPipeR::LineWise(
                    code = input$new_code_r, step_name = input$new_step_name,
                    dependency = if(is.null(input$new_step_dep)) "" else input$new_step_dep,
                    run_step = input$new_step_req,
                    run_session = input$new_step_session
                )
                his$add(list(sal = sal, msg = "New R step"))
            })
            step_order_change(FALSE)
            savehis(savehis() + 1)
            removeModal()
            shinyCatch(message("New R step saved."))
        }, ignoreInit = TRUE)

        # step config save ---
        cur_config <- reactiveVal(NULL)
        ## config R
        configStep <- function(rcode = TRUE){
            sal <- his$get()$item$sal
            sal_stat <- statSal(sal)
            sal <- shinyCatch(blocking_level = "error", {
                # save new code
                if(rcode) sal@stepsWF[[cur_config()]]@codeLine <- shinyCatch(parse(text = input$edit_code), blocking_level = "error")
                # rename
                if(!emptyIsFalse(input$change_name)) stop("Step Name is empty")
                if(sal_stat$names[cur_config()] != input$change_name) {
                    if(input$change_name %in% sal_stat$names) stop("Duplicated step name")
                    systemPipeR::renameStep(sal, step = cur_config()) <- input$change_name
                }
                # change dep
                systemPipeR::dependency(sal, step = cur_config()) <- if(emptyIsFalse(input$change_dep[1])) input$change_dep else ""
                sal
            })
            step_order_change(FALSE)
            his$add(list(sal = sal, msg = "Config R step"))
            wf_ui_updates()
            updateHisInfo(his)
        }
        observeEvent(input$save_config_r, {
            req(input$save_config_r)
            req(cur_config())
            on.exit(setwd(spsOption("app_path")), add = TRUE)
            setwd(shared$wf$env_path)
            configStep()
            shinyCatch(message("R step configured"))
            removeModal()
        })
        ## config sys
        observeEvent(input$save_config_sys, {
            req(input$save_config_sys)
            req(cur_config())
            on.exit(setwd(spsOption("app_path")), add = TRUE)
            setwd(shared$wf$env_path)
            configStep(rcode = FALSE)
            shinyCatch(message("sysArgs step configured"))
            removeModal()
        })

        # when new sal history  ----
        ob_index <- reactiveVal(0)
        wf_ui_updates <- function(){
            sal <- his$get()$item$sal
            sal_stat <- statSal(sal)
            # update sort
            if(length(sal$stepsWF) != 0) {
                output$sort_box <-  renderUI({
                    makeSort(sal_stat$names, sal_stat$type, ns, sal_stat, ob_index)
                })
                destoryOb(isolate(wf_share$config_ob))
                wf_share$config_ob <- makeConfig(sal, sal_stat$names, sal_stat$deps, session, input, output, ns, cur_config, ob_index)
            }
            # update plot
            output$wf_plot <- systemPipeR::renderPlotwf({
                req(sal)
                systemPipeR::plotWF(sal, rstudio = TRUE, plot_ctr = FALSE)
            })
            # update redo undo
            shinyjs::toggleState("step_undo", !his$status()$first)
            shinyjs::toggleState("step_redo", !his$status()$last)
        }

        observeEvent(savehis(), {
            req(savehis() > 0)
            wf_ui_updates()
            updateHisInfo(his)
        })

        # on sort order change ----
        step_order_change <- reactiveVal(TRUE)
        step_order_inited <- reactiveVal(FALSE)
        sal_old <- reactiveVal(FALSE)
        observeEvent(input$step_orders, {
            req(!is.null(input$step_orders))
            # cat("change before", step_order_change(), "\n")
            if(!step_order_change()) return(step_order_change(TRUE))
            if(!step_order_inited()) return(step_order_inited(TRUE))
            # cat("change after", step_order_change(), "\n")
            sal <- his$get()$item$sal
            step_order <- as.numeric(input$step_orders)
            if(length(step_order) == 1 && step_order[1] == "0") step_order <- NULL
            sal_new <- sal[step_order]
            req(!identical(sal_new, sal_old()))
            sal_old(sal_new)
            his$add(list(sal = sal_new, msg = "Change order"))
            savehis(savehis() + 1)
        }, ignoreInit = TRUE)
        # delete step
        observeEvent(input$step_order_del_trigger, {
            req(input$step_order_del_trigger)
            req(!is.null(input$step_order_del))
            sal <- his$get()$item$sal
            step_order <- as.numeric(input$step_order_del)
            if(length(step_order) == 1 && step_order[1] == "0") step_order <- NULL
            sal_new <- sal[step_order]
            ob_index(ob_index() + 1)
            step_order_change(FALSE)
            his$add(list(sal = sal_new, msg = "Delete step"))
            savehis(savehis() + 1)
        }, ignoreInit = TRUE)
        # on redo or undo ----
        updateHisInfo <- function(his){
            getMsg <- function(pos){
                quiet(shinyCatch(his$get(pos)$item$msg, shiny = FALSE)) %>%
                    if(emptyIsFalse(.)) . else "some action"
            }
            stats <- his$status()
            msgs <- paste0(stats$pos, ".", getMsg(stats$pos))
            msgs <- if(!stats$first) c(paste0(stats$pos - 1, ". ", getMsg(stats$pos - 1)), msgs) else c("-", msgs)
            msgs <- if(!stats$last) c(msgs, paste0(stats$pos + 1, ". ", getMsg(stats$pos + 1))) else c(msgs,  "-")
            session$sendCustomMessage("wf-undo-redo", list(msg = unname(msgs)))
        }
        observeEvent(input$step_undo, {
            req(!his$status()$first)
            shinyCatch(his$backward(), blocking_level = "error")
            step_order_change(FALSE)
            wf_ui_updates()
            updateHisInfo(his)
        })
        observeEvent(input$step_redo, {
            req(!his$status()$last)
            shinyCatch(his$forward(), blocking_level = "error")
            step_order_change(FALSE)
            wf_ui_updates()
            updateHisInfo(his)
        })

        # on final save button ----
        observeEvent(input$totask, {
            req(input$totask)
            shared$wf$sal <- sal <- his$get()$item$sal
            shinyCatch(blocking_level = "error", {
                systemPipeR::write_SYSargsList(sys.file = file.path(shared$wf$env_path, ".SPRproject", "SYSargsList.yml"), sal)
            })
            shared$wf$flags$wf_ready = isolate(shared$wf$flags$wf_ready) + 1
            shinyWidgets::confirmSweetAlert(
                session = session,
                inputId = ns("confirm_next"),
                title = "Workflow file setup done!",
                closeOnClickOutside = TRUE,
                cancelOnDismiss = FALSE,
                btn_labels = c("Step 4", "Step 5"),
                html = TRUE,
                type = "success",
                text = HTML(glue(
                    "
                    <h4>Do you want to proceed to download/run the workflow? or play with some CWL operations (optional)</h4>
                    <ul class='text-left'>
                      <li>Your workflow is ready.</li>
                      <li>Click outside of this box or cancel if you want to stay here.</li>
                    </ul>
                    "
                ))
            )
        })

        observeEvent(input$confirm_next, {
            if(is.null(input$confirm_next)) return(NULL)
            tab_index <- if(input$confirm_next) 4 else 3
            shinyjs::runjs(glue("$('#wf-wf_panel-{tab_index}-heading > h4').trigger('click');"))
        })
    }
    moduleServer(id, module)
}

