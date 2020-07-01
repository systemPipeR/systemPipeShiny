# WF main page UI
wf_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("WF main"),
        p("Instructions on how to use the wf utilities."),
        a(href = "#shiny-tab-wf_targets", class = "tablink", "This is an inside link"),
        h2("This is an outside link"),
        a(href = "https://www.google.com", "Visit !")
    )

}


## server
wf_mainServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns

    }
    moduleServer(id, module)
}

