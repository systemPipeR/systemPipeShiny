## UI

#' @importFrom pushbar pushbar_deps pushbar
#' @noRd
core_topUI <- function(id){
    ns <- NS(id)
    init <-
"df <- data.frame(
    num=1:20,
    let=LETTERS[1:20],
    rand=rnorm(20)
)
df"
    tagList(
        pushbar::pushbar_deps(),
        pushbar::pushbar(
            id = ns("wf_push"),
            from = "top",
            style= "background:#ECF0F5;padding:2%;min-height:100%; overflow:auto;",
            tagList(
                fluidRow(
                    div(style = " text-align: center;",
                        shinyWidgets::actionBttn(ns("close_push"),
                                                 style = "simple",
                                                 label = "Close this workflow session",
                                                 icon = icon("times"),
                                                 color = "danger",
                                                 size = "sm")) %>%
                        bsHoverPopover(
                            "Close this session",
                            "Clicking here will set your working directory back
                            to the app directory and may cause the workflow
                            fail to run.",
                            "bottom"
                        )
                ),
                fluidRow(
                    column(
                        6,
                        h2("Source Code"),
                        aceEditor(
                            ns("code"), mode = "r", height = "200px", value = init,
                            autoComplete = "live",
                            autoCompleters = c("static", "text"),
                            autoCompleteList = getNamespaceExports("systemPipeR") %>%
                                {.[!str_detect(., "^\\.")]} %>%
                                {paste0("systemPipeR::", .)} %>%
                                list(systemPipeR = .)),
                        actionButton(ns("eval"), "Evaluate")
                    ),
                    column(
                        6,
                        h2("Output"),
                        verbatimTextOutput(ns("output")) %>%
                            {.$attribs[['style']] <- "scroll: auto; height: 200px;"; .}
                    )
                )
            )
        )
    )
}

## server

#' @importFrom pushbar setup_pushbar pushbar_open pushbar_close
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyjqui orderInput
#' @importFrom shinyWidgets actionBttn confirmSweetAlert
#' @importFrom shinyjs toggleState
#' @noRd
core_topServer <- function(id, shared){
    module <- function(input, output, session){
        pushbar::setup_pushbar(blur = TRUE, overlay = TRUE)
        ns <- session$ns
        # close
        observeEvent(input$close_push, {
            pushbar::pushbar_close()
            setwd(shared$wf$wd_old)
            print(getwd())
        })

        ## run code
        output$output <- renderPrint({
            input$eval
            eval(parse(text = isolate(input$code)))
        })
    }
    moduleServer(id, module)
}

