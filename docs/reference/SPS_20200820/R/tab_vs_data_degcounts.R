degcounts_desc <-
  "
#### Here you can upload DEG counts

"
## UI
data_degcountUI <- function(id, description = degcounts_desc){
    ns <- NS(id)
    tagList(
        h2("Deg Count Data Frame"),
        HTML(markdown::renderMarkdown(text = glue(description))),
        radioGroupButtons(
            inputId = ns("plot_source"), label = "Choose your plot file source:",
            selected = "upload",
            choiceNames = c("Upload", "Example"),
            choiceValues = c("upload", "eg"),
            justified = TRUE, status = "primary",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
        ),
        textInputGroup(textId = ns("data_path"), btnId = ns("upload"), title = "Specify your data path", label = "Upload"),
        column(width = 12, style = "padding-left: 0;",
               downloadButton(ns("down_config"), "Save"),
               actionButton(ns("to_task"),
                            label = "Add to task",
                            icon("paper-plane"))
        ),
        rHandsontableOutput(ns("df")),
        fluidRow(id = "plot_options",
                 a("Box Plot", href = "#shiny-tab-plot_box"),
                 a("plot2"),
                 a("plot3"),
                 p("...")
        )
        )
}

## server
data_degcountServer <- function(id, shared){
    module <- function(input, output, session){
        data_init <- data.frame(matrix("", 8,8), stringsAsFactors = FALSE)
        ns <- session$ns
        shinyjs::hide(id = "plot_options")
        selected_old <- reactiveVal("upload")
        selected_flag <- reactiveVal(TRUE)
        count_p_old <- reactiveVal("")
        t.df <- reactiveVal(data_init)
        # start the tab
        shinyjs::hide(id = "tab_main")
        observeEvent(input$validate, {
            if (shinyCheckPkg(
                session = session#,
                # cran_pkg = "pkg-1",
                # bioc_pkg = c("pkg-1", "pkg-2"),
                # github = "haha/pkg-3"
            )) {
                shinyjs::show(id = "tab_main")
            }
        })
        # update table
        observeEvent(c(input$plot_source, input$data_path) , {
            if (input$plot_source == "upload" & input$data_path == "") shinyjs::hide("df") else shinyjs::show("df")
        })

        output$df <- renderRHandsontable({
            rhandsontable(t.df(), selectCallback = TRUE, useTypes = FALSE) %>%
                hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        })

        onclick("to_task", shinyjs::show(id = "plot_options"))
        check_results <- T
        observeEvent(input$to_task, {
            shared$count$file <- tempfile(pattern = "DEGcounts", fileext = ".xls")
            if (all(check_results)) {
                sendSweetAlert(
                    session = session, type = "success",
                    title = "Data added", text = "Choose a plot type"
                )
                shared$count$df <- t.df()
                writeLines(apply(shared$count$df, 1, paste, collapse = "\t"), shared$count$file)
            }
        })
        observeEvent(c(input$plot_source, input$upload), ignoreInit = TRUE, {# only c work here, dont know why
            if (selected_flag() == TRUE) {
                confirmSweetAlert(
                    session,inputId = "sweet_changecount_confirm",
                    title = "Do you want to change data frame Source?",
                    text = "If you change the data frame source or load a new file, Data frame data will be reset in this tab and 'Task' tab. You will LOSE unsaved data", type = "warning"
                )
            } else {
                selected_flag(TRUE)
            }
        })
        # update df
        observeEvent(input$sweet_changecount_confirm, ignoreNULL = TRUE, {
            if (isTRUE(input$sweet_changecount_confirm)) {
                t.df(
                    hot_deg(count_df = input$df,
                            count_p = input$data_path,
                            count_p_old = count_p_old(),
                            choice = input$plot_source,
                            choice_old = selected_old()
                    )
                )
            }
        })

    }
    moduleServer(id, module)
}

# load deg count file
hot_deg <- function(count_df, count_p=NULL, count_p_old=NULL, choice, choice_old){
  count_p <- switch(choice,
                    "upload" = count_p,
                    "eg" = "./data/DEGcounts.xls"
  )
  if (is.empty(count_p)) return(data.frame(place_holder = NA, stringsAsFactors = FALSE))
  df.t <- read.csv(count_p, sep = '\t', comment.char = "#", stringsAsFactors = FALSE, header = FALSE)
  names(df.t) <- paste0("X", 1:ncol(df.t))
  return(df.t)
}
