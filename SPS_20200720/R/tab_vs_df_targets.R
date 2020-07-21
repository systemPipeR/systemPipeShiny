
targets_desc <-
    "
Here you can upload your targets file. The specifications file can be found in the Workflow management tab

"
## UI
df_targetsUI <- function(id, description = targets_desc){
    ns <- NS(id)
    tagList(
        h2("Targets Data Frame"),
        HTML(markdown::renderMarkdown(text = glue(description))),
        radioGroupButtons(
            inputId = ns("plot_source"), label = "Choose your plot file source:",
            selected = "upload",
            choiceNames = c("Upload", "Example SE","Example PE"),
            choiceValues = c("upload", "se","pe"),
            justified = TRUE, status = "primary",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
        ),
        textInputGroup(textId = ns("df_path"), btnId = ns("upload"), title = "Specify your data path", label = "Upload"),
        column(width = 12, style = "padding-left: 0;",
               downloadButton(ns("down_config"), "Save"),
               actionButton(ns("to_task"),
                            label = "Add to task",
                            icon("paper-plane"))
        ),
        fluidRow(id = "plot_options",
                 a("PCA", href = "#shiny-tab-plot_pca"),
                 a("plot2"),
                 a("plot3"),
                 p("...")
        ),
        rHandsontableOutput(ns("df"))
    )
}


## server
df_targetsServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        shinyjs::hide(id = "plot_options")

        t.df <- reactive(shinyCatch({
            df_target(
                count_p = input$df_path,
                choice = input$plot_source
            )
        }))
        # start the tab
        # shinyjs::hide(id = "tab_main")
        # observeEvent(input$validate, {
        #     if (shinyCheckPkg(
        #         session = session#,
        #         # cran_pkg = "pkg-1",
        #         # bioc_pkg = c("pkg-1", "pkg-2"),
        #         # github = "haha/pkg-3"
        #     )) {
        #         shinyjs::show(id = "tab_main")
        #     }
        # })
        # update table
        observeEvent(c(input$plot_source, input$upload) , {
            if (input$plot_source == "upload" &  is.empty(input$df_path)) shinyjs::hide("df")
            else {
                shinyjs::show("df")
                disable("upload"); disable("df_path")
            }

        })
        # only display first 100 rows
        output$df <- renderRHandsontable({
            rhandsontable(head(t.df()), readOnly = TRUE)
        })

        onclick("to_task", shinyjs::show(id = "plot_options"))
        check_results <- T
        observeEvent(input$to_task, {
            shared$df$file <- tempfile(pattern = "target", fileext = ".txt")
            if (all(check_results)) {
                sendSweetAlert(
                    session = session, type = "success",
                    title = "Data added", text = "Choose a plot type"
                )
                shared$df$target <- t.df()
                writeLines(apply(shared$df$target, 1, paste, collapse = "\t"), shared$df$file)
            }
        })

    }
    moduleServer(id, module)
}

# load raw count file
df_target <- function(choice, count_p=NULL){
    count_p <- switch(choice,
                      "upload" = count_p,
                      "se" = "data/targets.txt",
                      "pe" = "data/targetsPE.txt"
    )
    if (is.empty(count_p)) return(data.frame(place_holder = NA, stringsAsFactors = FALSE))
    df.t <- read.csv(count_p, sep = '\t', comment.char = "#", stringsAsFactors = FALSE, header = FALSE)
    names(df.t) <- paste0("X", 1:ncol(df.t))
    return(df.t)
}
