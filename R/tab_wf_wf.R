## UI
#' @importFrom networkD3 diagonalNetworkOutput
#' @importFrom shiny downloadButton
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyTree shinyTree
#' @importFrom shinyWidgets radioGroupButtons
#' @noRd
wf_wfUI <- function(id){
    ns <- NS(id)
    tagList(
        actionButton(ns("set"), "set"),
        div(
            id = "wf_wf_displayed",
            style = "display:none",
            tabTitle("Workflow"),
            renderDesc(id = ns("desc"),
            '
            #### Workflow files
            In SPR, workflows are defined as Rmarkdown files,
            you can read details and obtain them
            [here](https://systempipe.org/pages/pipelines_area/). This step can
            help you choose/ skip some steps. Make a workflow diagram to see how
            the order SPR execute the workflow and take a preview of the
            final report. If you just want to use the defaults, simply clicking
            the "Add to task". SPS has already selected all steps for you.
            ***
            #### Workflow steps
            Loading the default with pre-configed workflows or upload a
            Rmd file on "Existing" option. The workflow file will be display on
            "*Display workflow file*" box below. Workflow
            steps are defined by "#" hashtag levels, similar to the title level in
            markdown files. For example, text and code under the single "#" belongs
            to the heighest level step; code under double "##" is a sub-step under the
            nearest single "#" step from the top, *etc*.
            ***
            #### Add to task
            Clicking this button will add the workflow file to the running task, will
            be used in **step 5**. You need to select at least one step to enable
            this button.

            This is the last workflow preparation step (step 4 is optional). If you
            have followed the order so far, after this step, you should see all status
            indicators become green and you are go to lunch a workflow running session
            at step 5.
            ***
            #### Select workflow steps
            This box allows you to select workflow steps. You need to choose at lest
            one step to enable other buttons in this box.

            Clicking "Report preview" generates a preview of what the final report
            will look like based on your step selection, but in the preview,
            no code is evaluated. The report is displayed in the bottom of this tab.

            Clicking on the "Plot steps" will show a flow chart on the right side
            of what the step execution orders are when you run the actual workflow
            in SPR.
            ***
            #### Download
            After step selection, you can download the new Rmarkdown file by
            "Save New Rmd".
            '),
            spsHr(),
            boxPlus(
                title = "Confirm to use this workflow file",
                closable = FALSE, collapsible = TRUE,
                width = 12,
                class = "center-block",
                HTML(
                    "
                <ul>
                  <li>
                    When you have finished choosing workflow steps,
                    clicking on the <b>Add to task</b>.
                  </li>
                  <li>
                    You can also <b>Save</b> it as
                    an individual file from the browser.</p>
                  </li>
                </ul>
                "),
                div(class = "text-danger",
                    tags$ul(
                        id = ns("warn_exist"),
                        HTML(
                            "<li>You have chosen an <b>'Existing'</b> workflow. Upon passing
                            'Add to task' checks, a file named <b>workflow.Rmd</b> will
                            be written to the workflow folder. Rename/back up it if you already
                            have one with the same name.</li>"
                        )),
                    tags$ul(
                        id = ns("warn_other"),
                        HTML(
                            "<li>Upon passing 'Add to task' checks, the original workflow file
                            in the workflow folder will be overwritten. Rename/back up it if you do
                            not wish it to be replaced.</li>"
                        ))
                ),
                fluidRow(
                    style = "padding-left: 40%",
                    actionButton(ns("to_task_wf"),
                                 label = "Add to task",
                                 icon("paper-plane")) %>%
                        bsHoverPopover(
                            "Add workflow and add to workflow task",
                            "You should choose some
                            steps before adding it to task.",
                            "bottom"
                        ),
                    downloadButton(ns("down_rmd"), "Save") %>%
                        bsHoverPopover(
                            "Download current workflow file",
                            "You can download current workflow file from the
                            browser.",
                            "bottom"
                        )
                )
            ),
            fluidRow(
                boxPlus(title = "Display workflow file", width = 12,
                        closable = FALSE,
                        shinyWidgets::radioGroupButtons(
                            inputId = ns("wf_source"),
                            label = "Choose workflow file source:",
                            selected = "default",
                            choiceNames = c("Default", "Upload"),
                            choiceValues = c("default", "upload"),
                            justified = TRUE, status = "primary",
                            checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                             no = icon(""))
                        ),
                        fileInput(ns("rmd_file"), "Choose R markdown File",
                                  multiple = FALSE,
                                  accept = "Rmd"),
                        tags$div(
                            style = 'overflow:auto; height: 500px',
                            diagonalNetworkOutput(ns("wf_D3"))
                        )
                )
            ),
            fluidRow(
                column(5,
                       boxPlus(
                           title = "Select workflow steps",
                           width = 12,
                           closable = FALSE,
                           collapsible = TRUE,
                           column(width = 12, style = "padding-left: 0;",
                                  actionButton(ns("wf_plot_step"),
                                               label = "Plot steps",
                                               icon("redo-alt")),
                                  actionButton(ns("wf_render_md"),
                                               label = "Report preview",
                                               icon("redo-alt"))
                           ),
                           hr(),
                           h4("Search steps in the box below"),
                           p("When steps are chosen, you can plot steps and preview
                             report document."),
                           shinyTree(ns("rmd_tree"), checkbox = TRUE)

                       )
                ),
                column(7,
                       boxPlus(title = "Workflow steps selected",
                               width = 12,
                               closable  = FALSE,
                               enable_sidebar = TRUE,
                               sidebar_title = "Workflow diagram legend",
                               sidebar_background = "#337ab7",
                               sidebar_start_open = TRUE,
                               collapsible = TRUE,
                               sidebar_width = 25,
                               sidebar_content = div(
                                   h3("Diagram Legend"),
                                   p("This part uses systemPipeR::plotWF function"),
                                   tags$ul(
                                       tags$li("Gray colored steps are steps without any code chunks, only text"),
                                       tags$li("steps with 'NA' means unknown number of samples in this step.
                                               If it is real case in SPR, you should see something like
                                               '10/10' all samples passed this step or '10/20' only half passed.")
                                    )
                               ),
                               uiOutput(ns("wf_plot_ui"))
                       )
                )
            ),
            fluidRow(
                boxPlus(
                    title = "Preview of the workflow report", width = 12,
                    closable = FALSE,
                    uiOutput(ns("wf_md_ui"))
                )
            )
        ),
        div(
            id = "wf_wf_disable",
            h3("Generate a workflow environment at Step 1 first",
               style = "text-center text-warning")
        )
    )
}

## server
#' @importFrom networkD3 renderDiagonalNetwork diagonalNetwork
#' @importFrom shinyTree get_selected updateTree renderTree
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom shinyjs runjs enable disable
#' @noRd
wf_wfServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        rmd_file_temp <-  reactiveVal(NULL)
        #######
        observeEvent(input$set, {
            shared$wf$flags$env_ready = T
            shared$wf$targets_path = "upload_required"
            shared$wf$env_path = normalizePath("riboseq")
            shared$wf$env_option = "exist"
        })
        #####
        # toggle elements based on options
        observeEvent(shared$wf$env_option, {
            shinyjs::toggleElement("warn_exist", anim = TRUE, condition = shared$wf$env_option == "exist")
            shinyjs::toggleElement("warn_other", anim = TRUE, condition = shared$wf$env_option != "exist")
            if(shared$wf$env_option == "exist"){
                shinyWidgets::updateRadioGroupButtons(
                    session,
                    "wf_source",
                    label = "Existing option selected, no default file displayed, please upload workflow file",
                    choiceNames = "Upload",
                    choiceValues = "upload"
                )
            }
        })
        observeEvent(input$wf_source, {
            shinyjs::toggleElement("rmd_file", anim = TRUE, condition = input$wf_source == "upload")
        })
        # resolve path
        rmd_file_path <- reactive({
            if (input$wf_source == "default") shared$wf$wf_path
            else input$rmd_file$datapath
        })
        # read Rmd
        rmd <- reactive({
            if (!is.null(rmd_file_path())) {
                quiet(.subsetRmd(p = rmd_file_path()))
            } else {
                NULL
            }
        })
        # get selected steps
        rmd_tree_selected <- reactive({
            if (input$wf_source == "upload" & is.null(rmd_file_path())) {
                NULL
            } else {
                shinyTree::get_selected(input$rmd_tree, format = "names") %>%
                    unlist() %>%
                    str_remove_all(" .*$") %>%
                    findTreeParent()
            }

        })
        # disable all on start
        observeEvent(input$wf_source, {
            disable("down_rmd"); disable("wf_render_md"); disable("to_task_wf")
            disable("wf_plot_step")
        })
        # disable all if no step been chosen
        observeEvent(input$rmd_tree, {
            if (length(rmd_tree_selected()) < 1 ) {
                disable("down_rmd"); disable("wf_render_md"); disable("to_task_wf")
                disable("wf_plot_step")
            } else {
                enable("down_rmd"); enable("wf_render_md"); enable("to_task_wf")
                enable("wf_plot_step")
            }
        })
        observeEvent(c(input$wf_source, rmd_tree_selected(), rmd_file_path()), {
            if (input$wf_source == "upload" & is.null(rmd_file_path())) {
                # hide tree if nothing is there
                runjs('document.querySelectorAll("[id*=rmd_tree]")[0].style.visibility = "hidden"')
                updateTree(session = session,
                                      treeId = "rmd_tree",
                                      data = list(""))
            } else {
                runjs('document.querySelectorAll("[id*=rmd_tree]")[0].style.visibility = ""')
            }

        })
        # default to select all steps
        # rmd_old <- reactiveVal("")
        # observeEvent(!identical(rmd(), rmd_old), {
        #     runjs('setTimeout(function(){$("[id*=rmd_tree]").jstree("select_all");}, 1000)')
        #     rmd_old(rmd())
        # })

        output$wf_D3 <- renderDiagonalNetwork({
            diagonalNetwork(
                step2listD3(rmd()$t_lvl, paste(rmd()$t_number, rmd()$t_text)),
                fontSize = 15)
        })
        output$rmd_tree <- renderTree({
            runjs('setTimeout(function(){$("[id*=rmd_tree]").jstree("select_all");}, 2000)')
            step2listTree(rmd()$t_lvl, paste(rmd()$t_number, rmd()$t_text))

        })

        # bottom right
        observeEvent(input$wf_plot_step, {
            rmd_tree_df <- rmd()[rmd()$t_number %in% rmd_tree_selected(), ]
            if (length(rmd_tree_selected()) > 0) rmd_tree_df$selected <- TRUE
            output$wf_plot_ui <- renderUI({
                tags$div(style = 'overflow:auto; height: 500px',
                         HTML(.plotWF(df_wf = rmd_tree_df,
                                     plot_style = "linear",
                                     out_type = "shiny"))
                )
            })
        })

        observeEvent(input$wf_render_md, ignoreInit = TRUE, {
            output$wf_md_ui <- renderUI({
                includeMarkdown(rmd_file_temp())
            })
        })
        output$down_rmd <- downloadHandler(
            filename <- function(){
                "workflow.Rmd"
            },
            content <- function(file){
                file.copy(rmd_file_temp(), filename)
            }
        )

        observeEvent(c(input$wf_render_md, input$to_task_wf, input$down_rmd),
                     ignoreInit = TRUE, {
            rmd_file_temp(tempfile(pattern = "wf", fileext = ".Rmd"))
            shinyCatch({
                quiet(.subsetRmd(p = rmd_file_path(),
                                 p_out = rmd_file_temp(),
                                 input_steps = paste(rmd_tree_selected(),
                                                     collapse = ","),
                                 save_rmd = TRUE
                ))
            }, blocking_level = "error")
        })

        observeEvent(input$to_task_wf, {
            req(file.exists(rmd_file_temp()))
            shinyCatch({
                # fix path for existing option
                if(shared$wf$wf_path == "upload_required")
                    shared$wf$wf_path <- file.path(shared$wf$env_path, "workflow.Rmd")
                # create back up folder
                dir.create(file.path(shared$wf$env_path, "backup"), recursive = TRUE, showWarnings = FALSE)
                # if wf file exists, back it up
                if(file.exists(shared$wf$wf_path))
                    file.copy(
                        shared$wf$wf_path,
                        file.path(
                            shared$wf$env_path,
                            "backup",
                            paste0(
                                glue("bk{Sys.time() %>% format('%Y%m%d%H%M%S')}"),
                                basename(shared$wf$wf_path))
                            ),
                        overwrite = TRUE
                    )
                # overwrite wf file
                if(!file.copy(rmd_file_temp(), shared$wf$wf_path, overwrite = TRUE))
                    stop("File ", shared$wf$wfpath, " can not be created or not modified")
            }, blocking_level = "error")
            shared$wf$flags$wf_ready = TRUE
            # jump to next step
            shinyWidgets::confirmSweetAlert(
                session = session,
                inputId = ns("confirm_next"),
                title = "Workflow file setup done!",
                closeOnClickOutside = TRUE,
                btn_labels = c("Step 4", "Step 5"),
                html = TRUE,
                type = "success",
                text = HTML(glue(
                    "
                    <button class='btn btn-box-tool'
                        data-widget='chat-pane-toggle'
                        data-toggle='tooltip'
                        data-original-title='More'
                        type='button' title='Workflow diagram legend'
                        style='padding-left: 90%; padding-bottom: 0;'>
                        <i class='fa fa-minus'></i>
                    </button>
                    <h4>Do you want to proceed to the step 4 or step 5?</h4>
                    <ul class='text-left'>
                      <li>Step 4 is <b>optional</b>, can be skipped</li>
                      <li>If all steps' status are green, you can to go step 5 to run the workflow.</li>
                      <li>Click outside this box to dismiss if you want to stay here.</li>
                    </ul>
                    "
                ))
            )
        })
        observeEvent(input$confirm_next, {
            if(input$confirm_next){
                shinyjs::runjs("$('#wf-wf_panel-4-heading > h4').trigger('click');")
            } else {
                shinyjs::runjs("$('#wf-wf_panel-3-heading > h4').trigger('click');")
            }
        })
    }
    moduleServer(id, module)
}

