## UI
#' @noRd
core_welcomeUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Welcome to systemPipeShiny"),
        # tags$style(".sps-dash .desc-body.collapse:not(.in) {height: 400px;}"),
        fluidRow(
            class = "text-main",
            style = "padding-left: 15px;",
            tags$img(
                src = 'img/logo.png',
                class = "home-logo",
                style = 'height: 200px; margin: 0 auto; display: block;'
            ),
            spsHr(),
            renderDesc(id = ns("desc"), desc =
            "
            ## Introduction
            systemPipeShiny (SPS) extends the widely used systemPipeR (SPR)
            workflow environment with a versatile graphical user interface
            provided by a Shiny App. This allows non-R users, such as
            experimentalists, to run many systemPipeR's workflow designs,
            control, and visualization functionalities interactively without
            requiring knowledge of R. Most importantly, SPS has been designed
            as a general purpose framework for interacting with other R packages
            in an intuitive manner. Like most Shiny Apps, SPS can be used on both
            local computers as well as centralized server-based deployments that
            can be accessed remotely as a public web service for using SPR's
            functionalities with community and/or private data.

            ## To start
            Start using SPS by choosing a **module** or a **custom tab** from the
            left sidebar.

            *****

            ### SPS Modules
            A SPS module is a complex app unit for a certain purpose. Usually a
            modules is built by some smaller units, we call them *\"subtabs\"*. Under
            current version of SPS, there are 3 pre-built modules.

            1. **Workflow**: Choose, design, and run [systemPipeR{blk}](https://systempipe.org/sp/)
               workflows with guided and interactive manner.
            2. **RNA-Seq**: perform downstream RNAseq analysis, like clustering, DEG, plotting, and more.
            3. **Quick {ggplot}**: Make ggplots from any tabular-like datasets users provide.

            Please expect more modules in future versions.

            *****

            ### SPS Custom Tabs
            A SPS custom tab is a small app unit, just like the subtab in a module. Unlike a
            subtab in a module, a custom tab is usually stand-alone and does not connect
            with other tabs. The main use case of a custom tab is to help users do some simple data preprocess and
            *make a certain type of plot*.

            *****

            ### SPS Canvas
            SPS Canvas is a unique tab which contains an unique **image editor**. It allows users to
             \"screenshot\" plots from other modules/tabs and send to this workbench to do further
            image editing and make a scientific figure.

            Simply click on the `To Canvas` button in various modules/tabs will take a screenshot.

            *****

            ### Users' manual
            Visit [our website{blk}](https://systempipe.org/sps/) for details!

            *****

            ### Browser compatibility
            App is tested on the recent versions of Chrome and Firefox, should also work on latest
            Edge and may work on Safari. IE is not supported.

            Also, please disable some privacy extensions/plugins that will block
            HTML5 canvas figerprint or drag and drop.

            *****

            ### Developers
            As a shiny framework. SPS provides a lot of functions to help developers to
            add more or customize SPS components and outside the SPS framework, like
            users own shiny Apps. These developer tools are provided in supporting
            packages. Read [this section on our website{blk}](https://systempipe.org/sps/dev) for more details.
            ") %>%
                div(class = "sps-dash"),
            spsHr(),
            gallery(
                texts = c(
                    "App structure",
                    "User login",
                    "Loading themes",
                    "Workflow module",
                    "Workflow metadata",
                    "Workflow step selection & desgin",
                    "Workflow Execution",
                    "RNAseq normalization",
                    "RNAseq DEG",
                    "RNAseq plots",
                    "Canvas Module",
                    "Admin login",
                    "Admin app stats",
                    "Admin user control",
                    "Customizable notifications",
                    "Customizable interactive tutorials",
                    "Logging and error handling"


                ),
                hrefs = c(
                    "https://systempipe.org/sps/intro/",
                    "https://systempipe.org/sps/adv_features/login/#main-app-login",
                    "https://systempipe.org/sps/adv_features/login/#main-app-login",
                    "https://systempipe.org/sps/modules/workflow/",
                    "https://systempipe.org/sps/modules/workflow/#2-prepare-a-target-file",
                    "https://systempipe.org/sps/modules/workflow/#3-prepare-a-workflow-file",
                    "https://systempipe.org/sps/modules/workflow/#5-run-or-finish-workflow-preparation",
                    "https://systempipe.org/sps/modules/rnaseq/",
                    "https://systempipe.org/sps/modules/rnaseq/#deg-report",
                    "https://systempipe.org/sps/modules/rnaseq/#plot-options",
                    "https://systempipe.org/sps/canvas/",
                    "https://systempipe.org/sps/adv_features/login/#admin-page",
                    "https://systempipe.org/sps/adv_features/login/#app-information",
                    "https://systempipe.org/sps/adv_features/login/#account-control",
                    "https://systempipe.org/sps/adv_features/notification/",
                    "https://systempipe.org/sps/adv_features/guide/",
                    "https://systempipe.org/sps/adv_features/debug/"

                ),
                images = c(
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/content/en/sps/img/sps_structure.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/user_login.png?raw=true",
                    "https://systempipe.org/sps/adv_features/login_theme.gif",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/wf_main.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/wf_targets.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/wf_wf.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/wf_run.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/rnaseq_normalize.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/rnaseq_deg.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/rnaseq_heatmap.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/canvas.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/admin_login.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/admin_server_info.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/admin_user_control.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/sps_notification.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/sps_guide.png?raw=true",
                    "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/logging.png?raw=true"
                ),
                title = "Gallery of SPS Features",
                enlarge = TRUE,
                enlarge_method = "modal",
                image_frame_size = 2,
                target_blank = TRUE
            ),
            spsHr(),
            column(
                12, class = "desc-table",
                h3(class="text-center", 'Other packages in systemPipeShiny'),
                markdown(
                '
                | Package | Description | Documents | Function reference | Demo |
                | --- | --- | --- | :---: | --- |
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/sps_small.png?raw=true" align="right" height="30" width="30"/>[systemPipeShiny{blk}](https://github.com/systemPipeR/systemPipeShiny) | SPS main package |[website{blk}](https://systempipe.org/sps/)|[link{blk}](https://systempipe.org/sps/funcs/sps/reference/)  | [demo{blk}](https://tgirke.shinyapps.io/systemPipeShiny/)|
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/spscomps.png?raw=true" align="right" height="30" width="30" />[spsComps{blk}](https://github.com/lz100/spsComps) | SPS UI and server components |[website{blk}](https://systempipe.org/sps/dev/spscomps/)|[link{blk}](https://systempipe.org/sps/funcs/spscomps/reference/)  | [demo{blk}](https://lezhang.shinyapps.io/spsComps)|
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/drawer.png?raw=true" align="right" height="30" width="30" />[drawer{blk}](https://github.com/lz100/drawer) | SPS interactive image editing tool |[website{blk}](https://systempipe.org/sps/dev/drawer/)|[link{blk}](https://systempipe.org/sps/funcs/drawer/reference/)  | [demo{blk}](https://lezhang.shinyapps.io/drawer)|
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/spsutil.png?raw=true" align="right" height="30" width="30" />[spsUtil{blk}](https://github.com/lz100/spsUtil) | SPS utility functions |[website{blk}](https://systempipe.org/sps/dev/spsutil/)|[link{blk}](https://systempipe.org/sps/funcs/spsutil/reference/)  | NA|
                '
                )
            )
        )
    )
}

## server
core_welcomeServer <- function(id, shared){
    module <- function(input, output, session, shared){
        ns <- session$ns
    }
    moduleServer(id, module)
}
