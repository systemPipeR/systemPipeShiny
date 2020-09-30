# WF main page UI
wf_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("WF main"),
        p("Workflow management helps you to prepare a data analysis workflow
          files in systemPipeR (SPR) format or can be used in other similar
          compatible
          workflow environments. Files include the metadata(targets) file,
          workflow step file(Rmarkdown format) and a workflow config file.
          Each of these files are prepared in a SPS tab. You can find them
          at the left side navigation bar. SPS helps users to check the
          formats and requirements of each file. "),
        HTML('
        <p>You can read more details about workflow management in the
        <a href="https://systempipe.org/systemPipeShiny/articles/systemPipeShiny.html#workflow-management">vignette</a></p>
             ')
    )

}


## server
wf_mainServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns

    }
    moduleServer(id, module)
}

