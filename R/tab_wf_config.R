## UI
#' @importFrom shinyAce aceEditor
#' @importFrom shinyWidgets radioGroupButtons
wf_configUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Workflow Configuration"),
        renderDesc(id = ns("desc"),
        '
        #### Configuration file
        In SPR, the config file SPRconfig.yaml is required to define information
        like project path, targets file path and more. Normally, this file is
        generated automatically when one starts a SPR project, but if any
        modification is made, one needs to update this config file.

        This file should be written in [yaml](https://yaml.org/) format.
        Required records are: project_path, data_path, param_path, results_path,
        targets, Rmd.

        If you are using SPS to prepare a new workflow file, the new downloaded
        workflow file is named "NewWF.Rmd".
        In this case, you can change the path
        of "Rmd" to this new name or rename the new workflow file to match the
        config record. Change other records in these files are
        usually not recommended.

        If you plan to use all default configs, you can just **skip** to
        prepare this file in SPS.
        '),
        spsHr(),
        fluidRow(
            shinyWidgets::radioGroupButtons(
                inputId = ns("config_source"),
                label = "Choose your config file source:",
                selected = "upload",
                choiceNames = c("Upload", "Example"),
                choiceValues = c("upload", "eg"),
                justified = TRUE, status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                 no = icon(""))
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
        p("Edit your yaml here (case sensitive)"),
        shinyAce::aceEditor(
            outputId = ns("ace_config"),
            theme = "Chrome",
            value = "",
            placeholder = "yaml format",
            mode = "yaml"
        )
    )
}

## server
#' @importFrom shinyAce updateAceEditor
#' @importFrom shinyWidgets sendSweetAlert
wf_configServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        down_clicked <- reactiveValues(flag = 0)

        config_file_path <- reactive({
            if(input$config_source == "eg") "data/config.yaml"
            else input$config_upload$datapath
        })
        new_config_path <- tempfile(pattern = "config", fileext = ".yaml")

        observeEvent(config_file_path(), {
            shinyAce::updateAceEditor(
                session, editorId = "ace_config",
                value = {
                    shinyCatch(
                        readLines(config_file_path()) %>%
                            paste(collapse = "\n"), blocking_level = "error")
            })
        })

        observeEvent(input$to_task_config, {
            shared$config$file <- new_config_path
            writeLines(input$ace_config, shared$config$file)
        })
        observeEvent(input$to_task_config, {
            for(i in c("roject_path:", "data_path:", "param_path:",
                       "results_path:", "targets:", "Rmd:")){
                if(!str_detect(input$ace_config, i)){
                    shinyWidgets::sendSweetAlert(
                        session = session,
                        title = "Missing records",
                        text = glue("Record '{i} xx' is missing"),
                        type = "error"
                    )
                    shinyCatch(spserror(glue("Record '{i} xx' is missing")),
                               blocking_level = "error",
                               shiny = FALSE)
                }
            }

            if (!is.null(shared$config$file)) {
                shared$wf_flags$wf_conf_ready = TRUE
                shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "Config added to Task",
                    text = "You can see workflow status by clicking top right",
                    type = "success"
                )
            }
        })
        output$down_config <- downloadHandler(
            filename <- function() {
                "SPRconfig.yaml"
            },
            content <- function(filename) {
                writeLines(input$ace_config, filename)
            })
    }
    moduleServer(id, module)
}

