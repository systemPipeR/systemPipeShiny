## UI
configUI <- function(id){
    ns <- NS(id)
    tabPanel(title = "Configuration",
        h2("Workflow Configuration"),
        fluidRow(
            radioGroupButtons(
                inputId = ns("config_source"), label = "Choose your config file source:", 
                selected = "upload",
                choiceNames = c("Upload", "Example"), 
                choiceValues = c("upload", "eg"),
                justified = TRUE, status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
            ),
            fileInput(
                ns("config_upload"), "If upload, choose your config file here:",
                multiple = FALSE,
                accept = c("yml", "yaml"),
                placeholder = "Choose your config file path",
            ),
            column(width = 12, style = "padding-left: 0;",
                   downloadButton(ns("down_config"), "Save"),
                   actionButton(ns("to_task_config"),
                                label = "Add to task", 
                                icon("paper-plane"))
            )
        ),
        p("Edit your yaml here"),
        aceEditor(
            outputId = ns("ace_config"),
            theme = "Chrome",
            value = "",
            placeholder = "yaml format",
            mode = "yaml"
        )
    )
}

## server
configServer <- function(input, output, session, shared){
    ns <- session$ns
    down_clicked <- reactiveValues(flag = 0)
    
    rmd_file_path <- reactive({
        if (input$config_source == "eg") "inst/extdata/config.yaml" else input$config_upload$datapath
    })
    observeEvent(rmd_file_path(), {
        updateAceEditor(session, editorId = "ace_config", value = {
            readLines(rmd_file_path(), warn = FALSE) %>% paste(collapse = "\n")
        })
    })

    observeEvent(c(input$to_task_config, down_clicked$flag), {
        shared$config$file <- tempfile(pattern = "target", fileext = ".txt")
        writeLines(isolate(input$ace_config), shared$config$file)
    })
    
    observeEvent(input$to_task_config, {
        if (!is.null(shared$config$file)) {
            shared$wf_flags$wf_conf_ready = TRUE
            sendSweetAlert(
                session = session, 
                title = "Config added to Task",
                text = "You can see workflow status by clicking top right",
                type = "success"
            )
        }
    })
    output$down_targets <- downloadHandler(
        filename <- function() {
            "targets.txt"
        },
        content <- function(filename) {
            down_clicked$flag <- down_clicked$flag + 1
            file.copy(from = shared$config$file, to = filename)
        })
}
