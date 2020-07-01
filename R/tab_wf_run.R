# UI
wf_runUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Run Workflow"),
        p("Coming soon...")
    )

}

# server
wf_runServer <- function(input, output, session, shared){
    ns <- session$ns
}

