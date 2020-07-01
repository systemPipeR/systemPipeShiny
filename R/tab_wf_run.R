# UI
wf_runUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Run Workflow"),
        p("Coming soon...")
    )

}

# server
wf_runServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns

    }
    moduleServer(id, module)
}

