## UI
#' @noRd
core_welcomeUI <- function(id){
    ns <- NS(id)
    tagList(
        tags$script(src = "sps/js/sps_welcome.js"),
        tags$link(rel = "stylesheet", href = "sps/css/sps_welcome.css"),
        tags$style('.skin-blue .main-header .navbar {background-color: #2c8abf;}'),
        div(class = "welcome-header",
            div(id = "welcome-svg"),
            div(
                class = "card wf", `data-tilt` ="", `data-tilt-max`="30",
                `data-tilt-scale`="1.3", `data-tilt-speed`="800",
                tags$img(src = "img/spr_notext.png"),
                tags$span("Workflow")
            ),
            div(
                class = "card rnaseq", `data-tilt` ="", `data-tilt-max`="30",
                `data-tilt-scale`="1.3", `data-tilt-speed`="800",
                tags$img(src = "img/rnaseq_notext.png"),
                tags$span("RNAseq")
            ),
            div(
                class = "card ggplot", `data-tilt` ="", `data-tilt-max`="30",
                `data-tilt-scale`="1.3", `data-tilt-speed`="800",
                tags$img(src = "img/ggplot.png"),
                tags$span("Quick ggplot"),
                onclick="document.querySelector('a[href=\"#shiny-tab-vs_esq\"]').click()"
            ),
            div(
                class = "card canvas", `data-tilt` ="", `data-tilt-max`="30",
                `data-tilt-scale`="1.3", `data-tilt-speed`="800",
                tags$img(src = "img/drawer.png"),
                tags$span("Canvas")
            ),
        ),
        tags$script(src = "sps/js/vanilla-tilt.js"),
        fluidRow(
            class = "text-main",
            style = "padding-left: 15px; padding-top: 25vh; padding-right: 15px",
            spsHr(),
            renderDesc(id = ns("desc"), desc =
            "
            ## To start
            Start by choosing a **module** below or from the left sidebar. Expand
            to read more about SPS.

            *****

            ### SPS Modules
            A SPS module is a complex app unit for a certain purpose. Usually a
            module is built by some smaller units, we call them *\"subtabs\"*. Under
            current version of SPS, there are 3 pre-built modules.

            1. [**Workflow**{blk}](https://systempipe.org/sps/modules/workflow/): Choose, design, and run [systemPipeR{blk}](https://systempipe.org/sp/)
               workflows with guided and interactive manner.
            2. [**RNA-Seq**{blk}](https://systempipe.org/sps/modules/rnaseq/): perform downstream RNAseq analysis, like clustering, DEG, plotting, and more.
            3. [**Quick {ggplot}**{blk}](https://systempipe.org/sps/modules/ggplot/): Make ggplots from any tabular-like datasets users provide.

            Please expect more modules in future versions.

            *****

            ### SPS Custom Tabs
            A SPS custom tab is a small app unit, just like the subtab in a module. Unlike a
            subtab in a module, a custom tab is usually stand-alone and does not connect
            with other tabs. The main use case of a custom tab is to help users do some simple data preprocess and
            *make a certain type of plot*.

            *****

            ### SPS Canvas
            SPS Canvas is a unique tab which contains a **image editor**. It allows users to
             \"screenshot\" plots from other modules/tabs and send to this workbench to do further
            image editing and make a scientific publishable figure.

            Simply click on the `To Canvas` button in various modules/tabs will take a screenshot.

            *****

            ### Users' manual
            Visit [our website{blk}](https://systempipe.org/sps/) for more details!

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
            fluidRow(

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
