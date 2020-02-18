## UI
wfUI <- function(id){
    ns <- NS(id)
    tagList(
        h2("Workflow"),
        fluidRow(
            boxPlus(title = "Display Rmd", width = 12, closable = FALSE,
                    fileInput(ns("rmd_file"), "Choose R markdown File",
                              multiple = FALSE,
                              accept = c(".Rmd")),
                    diagonalNetworkOutput(ns("wf_D3"))
            )
        ),
        fluidRow(
            column(5,
                   uiOutput(ns("wf_tree_ui"))
            ),
            column(7,
                   uiOutput(ns("wf_plot_ui"))
            )
        )
    )
}

## server
wfServer <- function(input, output, session){
    ns <- session$ns
    rmd <- reactive({ capture.output(df <- subsetRmd(p = input$rmd_file$datapath)) %>% invisible(); df })
    # top display
    observe({
        if (!is.null(input$rmd_file)){
            rmd_steps <- rmd()
            output$wf_D3 <- renderDiagonalNetwork({
                diagonalNetwork(step2listD3(rmd_steps$t_lvl, paste(rmd_steps$t_number, rmd_steps$t_text)), fontSize = 15)
            })
        }
    })
    # bottom left display
    observe({
        if (!is.null(input$rmd_file)){
            # top
            rmd_steps <- rmd()
            output$rmd_tree <- renderTree({
                step2listTree(rmd_steps$t_lvl, paste(rmd_steps$t_number, rmd_steps$t_text))
            })
            # ui B left
            output$wf_tree_ui <- renderUI({
                boxPlus(title = "Choose the steps you want",
                        width = 12,
                        collapsible = TRUE,
                        hr(),
                        h4("Search steps in the box below"),
                        shinyTree(ns("rmd_tree"), checkbox = TRUE),
                        h4("Save the new Rmd"),
                        p("It is only enabled when at least one step is selected."),
                        downloadButton(ns("down_rmd"), "Save New Rmd")
                )
            })
            delay(ms = 10, disable("down_rmd"))
        }
    })
    # bottom right
    observe({
        if (!is.null(input$rmd_tree)){
            rmd_steps <- rmd()
            rmd_tree_selected <-  get_selected(input$rmd_tree, format = "names") %>% unlist() %>% str_remove_all(" .*$") %>% 
                findTreeParent()
            rmd_tree_df <- rmd_steps[rmd_steps$t_number %in% rmd_tree_selected, ]
            if (length(rmd_tree_selected) > 0) rmd_tree_df$selected <- TRUE
            output$wf_plot_ui <- renderUI({
                boxPlus(title = "Workflow you selected",
                        width = 12,
                        collapsible = TRUE,
                        HTML(plotWF(df_wf = rmd_tree_df, plot_style = "linear", out_type = "shiny"))
                )
            })
            if (is.null(rmd_tree_selected)) {
                disable("down_rmd")
            } else {
                enable("down_rmd")
                output$down_rmd <- downloadHandler(
                    filename <- function(){
                        "Newsteps.Rmd"
                    },
                    content <- function(file){
                        subsetRmd(p = input$rmd_file$datapath,
                                  p_out = file,
                                  input_steps = paste(rmd_tree_selected, collapse = ","),
                                  save_rmd = TRUE
                        )
                    }
                )
            }
        }
    })
}