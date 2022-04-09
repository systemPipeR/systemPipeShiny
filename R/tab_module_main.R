## UI
module_mainUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Built-in modules"),
        renderDesc(id = ns("desc"),
        '
        ### Use modules in SPS
        Under current version of SPS, there are 3 pre-built modules.

        1. [**Workflow**{blk}](https://systempipe.org/sps/modules/workflow/): Choose, design, and run [systemPipeR{blk}](https://systempipe.org/sp/)
           workflows interactively in a guided manner.
        2. [**RNA-Seq**{blk}](https://systempipe.org/sps/modules/rnaseq/): perform downstream RNAseq analysis, like clustering, DEG, plotting, and more.
        3. [**Quick {ggplot}**{blk}](https://systempipe.org/sps/modules/ggplot/): Make ggplots from any tabular-like datasets users provide.

        ### New modules
        Currently add new custom built-in modules is not supported. If you have any good idea
        for the next module, please contact us.

        You can start by building a custom tab. When your tabs are enriched with
        functionalities, submit an issue or pull request to our [Github{blk}](https://github.com/systemPipeR/systemPipeShiny).
        We can make your tabs the next SPS built-in module.
        '),
        spsHr(),
        br(),

        genHrefTable(rows = list(Modules =  c(
            if(spsOption("module_wf")) "wf",
            if(spsOption("module_rnaseq")) "vs_rnaseq",
            if(spsOption("module_ggplot")) "vs_esq"
        )),
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

