## UI
module_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Visualization"),
        renderDesc(id = ns("desc"),
        '
        ### Use modules in SPS
        Under current version of SPS, there are 3 pre-built modules.

        1. **Workflow**: Choose, design, and run [systemPipeR{blk}](http://www.bioconductor.org/packages/devel/bioc/vignettes/systemPipeR/inst/doc/systemPipeR.html)
           workflows with guided and interactive manner.
        2. **RNA-Seq**: perform downstream RNAseq analysis, like clustering, DEG, plotting, and more.
        3. **Quick {ggplot}**: Make ggplots from any tabular-like datasets users provide.

        ### New modules
        Currently add new custom modules is not supported. If you have any good idea
        for the next module, please contact us.

        You can start by building a custom tab. When your tabs are enriched with
        functionalities, submit an issue or pull request to our [Github{blk}](https://github.com/systemPipeR/systemPipeShiny).
        We can make your tabs the next SPS default module.
        '),
        spsHr(),
        br(),
        genHrefTable(rows = list(`Modules` =  c("wf", "vs_rnaseq", "vs_esq")),
                     title = "All SPS module options")

    )
}

## server
module_mainServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns

    }
    moduleServer(id, module)
}

