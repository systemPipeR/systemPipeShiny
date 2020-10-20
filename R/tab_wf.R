# WF main page UI
#' @importFrom bsplus bs_accordion bs_append
wfUI <- function(id){
    ns <- NS(id)
    desc <-
    '
    Workflow management guides you step by step to prepare a data analysis workflow,
    and enbles you to run some simple workflows directly from this app. The
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
        h3("Track workflow preparation status", class = "text-center text-info"),
        spsTimeline(
            ns("wf_status"),
            up_labels = c("1", "2", "3", "4 Optional", "5"),
            down_labels = c("Gen WF", "Targets", "WF file", "CWL files", "Ready to run"),
            icons = list(
                icon("play"),
                icon("table"),
                icon("list-ol"),
                icon("file-code"),
                icon("check")
            ),
            completes = c(FALSE, FALSE, FALSE, TRUE, FALSE)
        ),
        bsplus::bs_accordion(id = ns("wf_panel")) %>%
            bsplus::bs_append("1. Create a workflow environment", wf_setupUI(ns("wf_setup")), panel_type = "success") %>%
            bsplus::bs_append("2. Prepare the targets file", wf_targetUI(ns("wf_targets"))) %>%
            bsplus::bs_append("3. Prepare the workflow file", wf_wfUI(ns("wf_wf"))) %>%
            bsplus::bs_set_opts(panel_type = "success") %>%
            bsplus::bs_append("4. Check CWL files (optional)", wf_cwlUI(ns("wf_cwl"))) %>%
            bsplus::bs_set_opts(panel_type = "default") %>%
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
## server
wfServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        wf_targetServer("wf_targets", shared)
        wf_setupServer("wf_setup", shared)
        wf_wfServer("wf_wf", shared)
        wf_cwlServer("wf_cwl", shared)
        wf_runServer("wf_run", shared)
        # init wf env
        shared$wf <- list(
            env_option = NULL,
            env_path = NULL,
            targets_path = NULL,
            wf_path = NULL,
            flags = list(
                env_ready = FALSE,
                targets_ready = FALSE,
                wf_ready = FALSE
            ),
            all_ready = FALSE
        )
        # status change
        observeEvent(shared$wf$flags, {
            updateSpsTimeline(session, "wf_status", 1, shared$wf$flags$env_ready)
            shinyjs::toggleCssClass("wf-wf_panel-0", "panel-success", asis = TRUE, shared$wf$flags$env_ready)
            updateSpsTimeline(session, "wf_status", 2, shared$wf$flags$targets_ready)
            shinyjs::toggleCssClass("wf-wf_panel-1", "panel-success", asis = TRUE, shared$wf$flags$targets_ready)
            updateSpsTimeline(session, "wf_status", 3, shared$wf$flags$wf_ready)
            shinyjs::toggleCssClass("wf-wf_panel-2", "panel-success", asis = TRUE, shared$wf$flags$wf_ready)

        }, ignoreInit = TRUE)
        observeEvent(shared$wf$flags$env_ready, {
            shinyjs::toggleElement(
                "wf_targets_displayed", asis = TRUE,
                anim = TRUE, animType = "fade",
                condition = shared$wf$flags$env_ready
            )
            shinyjs::toggleElement(
                "wf_targets_disable", asis = TRUE,
                anim = TRUE, animType = "fade",
                condition = !shared$wf$flags$env_ready
            )
        }, ignoreInit = FALSE)

        observeEvent(shared$wf$all_ready, {
            updateSpsTimeline(session, "wf_status", 5, shared$wf$all_ready)
            shinyjs::toggleCssClass("wf-wf_panel-4", "panel-success", asis = TRUE, shared$wf$all_ready)
        }, ignoreInit = TRUE)
    }
    moduleServer(id, module)
}





















