# WF main page UI
#' @importFrom bsplus bs_accordion bs_append
wfUI <- function(id){
    ns <- NS(id)
    desc <-
    '
    Workflow management guides you step by step to prepare a data analysis workflow,
    and enbles you to run some simple workflows directly fomr this app. The
    workflow will be prepared in [systemPipeR (SPR)](https://systempipe.org/)
    format or it can be used in other similar compatible workflow environments.

    #### Quick start
    To run a workflow, you need to create a workflow environment and prepare
    some files. Follow this order:

    1. Generate a workflow environment (**required**).
    2. Prepare a targets (metadata) file (**required**).
    3. Prepare a workflow (Rmd) file (**required**).
    4. Check if CWL files are correct (optional).
    5. Run the workflow.

    To quickly run the example, click **Gen Worklfow** in (1) and **Add to task**
    on (2) and (3). Once workflow set up is done, you can run workflow at (5).

    When the workflow running session starts, you will be locked into a specified
    new page. This will set your working directory into the workflow folder (no
    longer the SPS app folder). When you quit, the working directory will be set
    back to SPS app folder.

    **Note**: Quiting the workflow session while jobs are still running may cause
    the workflow fail. Some SPR workflow configs require you to have the workflow
    folder as the current working directory.

    You can read more details about workflow management in the
    [vignette](https://systempipe.org/systemPipeShiny/articles/systemPipeShiny.html#workflow-management)
    '
    tagList(
        tabTitle("WF main"),
        renderDesc(ns("desc"), desc),
        spsHr(),
        br(),
        h3("Follow steps below:", class = "text-center"),
        bsplus::bs_accordion(id = ns("wf_panel")) %>%
            bsplus::bs_append("1. Create a workflow environment", wf_setupUI(ns("wf_setup"))) %>%
            bsplus::bs_append("2. Prepare the targets file", wf_targetUI(ns("wf_targets"))) %>%
            bsplus::bs_append("3. Prepare the workflow file", wf_wfUI(ns("wf_wf"))) %>%
            bsplus::bs_append("4. Check CWL files (optional)", wf_cwlUI(ns("wf_cwl"))) %>%
            bsplus::bs_append("5. Run workflow", wf_runUI(ns("wf_run"))),
        hexPanel(ns("poweredby"), "THIS TAB IS POWERED BY:",
                 hex_imgs = c(
                     "img/sps.png",
                     "https://github.com/tgirke/systemPipeR/blob/gh-pages/images/systemPipeR_site.png?raw=true"),
                 hex_titles = c("SystemPipeShiny", "SystemPipeR"),
                 ys = c("-10", "-10")
        )
    )
}

# $("#wf_main-wf_panel-4-heading > h4 > a").trigger('click');
## server
wfServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        wf_targetServer("wf_targets", shared)
        wf_setupServer("wf_setup", shared)
        wf_wfServer("wf_wf", shared)
        wf_cwlServer("wf_cwl", shared)
        wf_runServer("wf_run", shared)
    }
    moduleServer(id, module)
}

