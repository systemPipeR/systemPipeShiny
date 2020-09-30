# UI
wf_runUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Run Workflow"),
        renderDesc(id = ns("desc"),
        '
        #### Running the workflow
        Directly running the prepared workflow from SPS will be supported soon.
        At this point, you should have prepared all the three very important
        files for SPR workflow running. Open up the progress tracking panel
        from top right corner and you should see everything is green. That
        means you are ready to go.

        Copy these files to your SPR workflow project root and follow SPR
        instructions.

        When you are done with workflow running and obtained some results,
        come back to SPS to make some beautiful plots in the "Visualization".

        '),
        spsHr(),
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

