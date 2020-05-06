## UI
wfUI <- function(id){
    ns <- NS(id)
    tabPanel(title = "Workflow",
        h2("Workflow"),
        fluidRow(
            boxPlus(title = "Display Rmd", width = 12,
                    closable = FALSE, 
                    radioGroupButtons(
                        inputId = ns("wf_source"), label = "Choose Workflow source:", 
                        selected = "upload",
                        choiceNames = c("Upload", "Example Rmd"), 
                        choiceValues = c("upload", "eg"),
                        justified = TRUE, status = "primary",
                        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
                    ),
                    fileInput(ns("rmd_file"), "Choose R markdown File",
                              multiple = FALSE,
                              accept = "Rmd",),
                    tags$div(
                        style = 'overflow:auto; height: 500px',
                        diagonalNetworkOutput(ns("wf_D3"))
                    )
            )
        ),
        fluidRow(
            column(5,
                   boxPlus(title = "Choose the steps you want",
                           width = 12,
                           closable = FALSE,
                           column(width = 12, style = "padding-left: 0;",
                                  downloadButton(ns("down_rmd"), "Save New Rmd"),
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
                           shinyTree(ns("rmd_tree"), checkbox = TRUE)

                   )
            ),
            column(7,
                   boxPlus(title = "Workflow you selected",
                           width = 12, 
                           closable  = FALSE,
                    uiOutput(ns("wf_plot_ui"))
                    )
            )
        ),
        fluidRow(
            boxPlus(
                title = "Prevew your workflow report", width = 12,
                closable = FALSE,
                uiOutput(ns("wf_md_ui"))
                    )
        )
    )
}

## server
wfServer <- function(input, output, session, shared){
    ns <- session$ns
    
    rmd_file_path <- reactive({
        if (input$wf_source == "eg") "inst/extdata/systemPipeRNAseq.md" else input$rmd_file$datapath
    })
    
    rmd <- reactive({
        if (!is.null(rmd_file_path())) {
            quiet(subsetRmd(p = rmd_file_path()))
        } else {
            NULL
        }
    })
    rmd_tree_selected <- reactive({
        if (input$wf_source == "upload" & is.null(rmd_file_path())) {
            NULL
        } else {
            get_selected(input$rmd_tree, format = "names") %>% unlist() %>%
                str_remove_all(" .*$") %>% findTreeParent()
        }
        
    })
    rmd_file_new <-  reactiveVal(NULL)
    disable_wf_bt <- reactiveVal(TRUE)
    
    observeEvent(input$wf_source, {
        disable("down_rmd"); disable("wf_render_md"); disable("to_task_rmd"); disable("wf_plot_step") # disable all on start
    })
    
    observeEvent(input$rmd_tree, {
        if (length(rmd_tree_selected()) < 1 ) {
            disable("down_rmd"); disable("wf_render_md"); disable("to_task_rmd"); disable("wf_plot_step")
        } else {
            enable("down_rmd"); enable("wf_render_md"); enable("to_task_rmd"); enable("wf_plot_step")
        }
    })
    observeEvent(c(input$wf_source, rmd_tree_selected(), rmd_file_path()), {
        if (input$wf_source == "upload" & is.null(rmd_file_path())) {
            runjs('document.querySelectorAll("[id*=rmd_tree]")[0].style.visibility = "hidden"')
            # rmd_tree_selected <- reactive(NULL)
            updateTree(session = session, treeId = "rmd_tree", data = list(""))
        } else {
            runjs('document.querySelectorAll("[id*=rmd_tree]")[0].style.visibility = ""')
        }

    })


    output$wf_D3 <- renderDiagonalNetwork({
        diagonalNetwork(step2listD3(rmd()$t_lvl, paste(rmd()$t_number, rmd()$t_text)), fontSize = 15)
    })
    output$rmd_tree <- renderTree({
        step2listTree(rmd()$t_lvl, paste(rmd()$t_number, rmd()$t_text))
    })

    # bottom right
    observeEvent(input$wf_plot_step, {
        rmd_tree_df <- rmd()[rmd()$t_number %in% rmd_tree_selected(), ]
        if (length(rmd_tree_selected()) > 0) rmd_tree_df$selected <- TRUE
        output$wf_plot_ui <- renderUI({
            tags$div(style = 'overflow:auto; height: 500px',
                     HTML(plotWF(df_wf = rmd_tree_df, plot_style = "linear", out_type = "shiny"))
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
            subsetRmd(p = rmd_file_path(),
                      p_out = file,
                      input_steps = paste(rmd_tree_selected(), collapse = ","),
                      save_rmd = TRUE
            )
        }
    )
    
    observeEvent(c(input$wf_render_md, input$to_task_rmd), ignoreInit = TRUE, {
        rmd_file_new(tempfile(pattern = "wf", fileext = ".Rmd"))
        quiet(subsetRmd(p = rmd_file_path(),
                        p_out = rmd_file_new(),
                        input_steps = paste(rmd_tree_selected(), collapse = ","),
                        save_rmd = TRUE
        ))
        shared$wf$file <- isolate(rmd_file_new())
    })
    
    observeEvent(input$to_task_rmd, {
        if (!is.null(shared$wf$file)) {
            shared$wf_flags$wf_ready = TRUE
            sendSweetAlert(
                session = session, 
                title = "Workflow added to Task",
                text = "You can see workflow status by clicking top right",
                type = "success"
            )
        }
    })
}

