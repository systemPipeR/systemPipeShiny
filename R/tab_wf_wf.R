## UI
#' @importFrom networkD3 diagonalNetworkOutput
#' @importFrom shiny downloadButton
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyTree shinyTree
#' @importFrom shinyWidgets radioGroupButtons
#' @noRd
wf_wfUI <- function(id){shinyCheckPkg
    ns <- NS(id)
    tagList(
        tabTitle("Workflow"),
        renderDesc(id = ns("desc"),
        '
        #### Workflow files
        In SPR, workflows are defined as Rmarkdown files,
        you can read details and obtain them
        [here](https://systempipe.org/pages/pipelines_area/).

        #### Workflow steps
        The example is an RNAseq workflow. Loading the example or upload a new
        Rmd file will be display on "Display workflow file" box below. Workflow
        steps are defined by "#" hashtag levels, similar to the title level in
        markdown files. For example, text and code under the single "#" belongs
        to the heighest level step; code under double "##" is a sub-step under the
        nearest single "#" step from the top, etc.

        #### Select workflow steps
        This box allows you to select workflow steps. You need to choose at lest
        one step to enable other buttons in this box.

        Clicking “Report preview” generates a preview of what the final report
        will look like based on your step selection, but in the preview,
        no code is evaluated. The report is displayed in the bottom of this tab.

        Clicking on the “Plot steps” will show a flow chart on the right side
        of what the step execution orders are when you run the actual workflow
        in SPR.

        #### Download
        After step selection, you can download the new Rmarkdown file by
        "Save New Rmd".

        #### Check progress
        Clicking on "Add to task" can update the workflow preparation progress.
        Remember to open it up from the top right corner.
        '),
        spsHr(),
        fluidRow(
            shinydashboardPlus::boxPlus(title = "Display workflow file", width = 12,
                    closable = FALSE,
                    shinyWidgets::radioGroupButtons(
                        inputId = ns("wf_source"),
                        label = "Choose Workflow source:",
                        selected = "upload",
                        choiceNames = c("Upload", "Example Rmd"),
                        choiceValues = c("upload", "eg"),
                        justified = TRUE, status = "primary",
                        checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                         no = icon(""))
                    ),
                    fileInput(ns("rmd_file"), "Choose R markdown File",
                              multiple = FALSE,
                              accept = "Rmd",),
                    tags$div(
                        style = 'overflow:auto; height: 500px',
                        networkD3::diagonalNetworkOutput(ns("wf_D3"))
                    )
            )
        ),
        fluidRow(
            column(5,
                   shinydashboardPlus::boxPlus(
                       title = "Select workflow steps",
                       width = 12,
                       closable = FALSE,
                       column(width = 12, style = "padding-left: 0;",
                              shiny::downloadButton(ns("down_rmd"),
                                                    "Save New Rmd"),
                              actionButton(ns("wf_plot_step"),
                                           label = "Plot steps",
                                           icon("redo-alt")),
                              actionButton(ns("wf_render_md"),
                                           label = "Report preview",
                                           icon("redo-alt")),
                              actionButton(ns("to_task_rmd"),
                                           label = "Add to task",
                                           icon("paper-plane"))
                        ),
                       hr(),
                       h4("Search steps in the box below"),
                       p("Displayed only when file uploaded or use example.
                          When steps are chosen, you can plot steps and preview
                          report document."),
                       shinyTree::shinyTree(ns("rmd_tree"), checkbox = TRUE)

                   )
            ),
            column(7,
                   shinydashboardPlus::boxPlus(title = "Workflow steps selected",
                           width = 12,
                           closable  = FALSE,
                    uiOutput(ns("wf_plot_ui"))
                    )
            )
        ),
        fluidRow(
            shinydashboardPlus::boxPlus(
                title = "Preview of the workflow report", width = 12,
                closable = FALSE,
                uiOutput(ns("wf_md_ui"))
                    )
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
        rmd_file_path <- reactive({
            if (input$wf_source == "eg") "data/systemPipeRNAseq.md"
            else input$rmd_file$datapath
        })

        rmd <- reactive({
            if (!is.null(rmd_file_path())) {
                quiet(.subsetRmd(p = rmd_file_path()))
            } else {
                NULL
            }
        })
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
        rmd_file_new <-  reactiveVal(NULL)
        disable_wf_bt <- reactiveVal(TRUE)

        observeEvent(input$wf_source, {
            shinyjs::disable("down_rmd")
            shinyjs::disable("wf_render_md")
            shinyjs::disable("to_task_rmd")
            shinyjs::disable("wf_plot_step") # shinyjs::disable all on start
        })

        observeEvent(input$rmd_tree, {
            if (length(rmd_tree_selected()) < 1 ) {
                shinyjs::disable("down_rmd");
                shinyjs::disable("wf_render_md")
                shinyjs::disable("to_task_rmd")
                shinyjs::disable("wf_plot_step")
            } else {
                shinyjs::enable("down_rmd")
                shinyjs::enable("wf_render_md")
                shinyjs::enable("to_task_rmd")
                shinyjs::enable("wf_plot_step")
            }
        })
        observeEvent(c(input$wf_source, rmd_tree_selected(), rmd_file_path()), {
            if (input$wf_source == "upload" & is.null(rmd_file_path())) {
                shinyjs::runjs('document.querySelectorAll("[id*=rmd_tree]")[0].style.visibility = "hidden"')
                # rmd_tree_selected <- reactive(NULL)
                shinyTree::updateTree(session = session,
                                      treeId = "rmd_tree",
                                      data = list(""))
            } else {
                shinyjs::runjs('document.querySelectorAll("[id*=rmd_tree]")[0].style.visibility = ""')
            }

        })


        output$wf_D3 <- networkD3::renderDiagonalNetwork({
            networkD3::diagonalNetwork(
                step2listD3(rmd()$t_lvl, paste(rmd()$t_number, rmd()$t_text)),
                fontSize = 15)
        })
        output$rmd_tree <- shinyTree::renderTree({
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
                includeMarkdown(rmd_file_new())
            })
        })
        output$down_rmd <- downloadHandler(
            filename <- function(){
                "NewWF.Rmd"
            },
            content <- function(file){
                .subsetRmd(p = rmd_file_path(),
                          p_out = file,
                          input_steps = paste(rmd_tree_selected(),
                                              collapse = ","),
                          save_rmd = TRUE
                )
            }
        )

        observeEvent(c(input$wf_render_md, input$to_task_rmd),
                     ignoreInit = TRUE, {
            rmd_file_new(tempfile(pattern = "wf", fileext = ".Rmd"))
            quiet(.subsetRmd(p = rmd_file_path(),
                            p_out = rmd_file_new(),
                            input_steps = paste(rmd_tree_selected(),
                                                collapse = ","),
                            save_rmd = TRUE
            ))
            shared$wf$file <- isolate(rmd_file_new())
        })

        observeEvent(input$to_task_rmd, {
            if (!is.null(shared$wf$file)) {
                shared$wf_flags$wf_ready = TRUE
                shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "Workflow added to Task",
                    text = "You can see workflow status by clicking top right",
                    type = "success"
                )
            }
        })
    }
    moduleServer(id, module)
}

