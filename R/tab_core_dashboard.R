## UI
#' @noRd
core_dashboardUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Welcome to systemPipeShiny"),
        tags$style(".sps-dash .desc-body.collapse:not(.in) {height: 400px;}"),
        fluidRow(
            class = "text-main",
            style = "padding-left: 15px;",
            tags$img(
                src = 'img/sps.png',
                class = "home-logo",
                style = 'height: 300px; margin: 0 auto; display: block;'
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
            Start using this by choosing a **module** or a **custom tab**.

            *****

            ### SPS Modules
            A SPS module is a complex app unit for a certain purpose. Usually a
            modules is built by some smaller units, we call them *\"subtabs\"*. Under
            current version of SPS, there are 3 pre-built modules.

            1. **Workflow**: Choose, design, and run [systemPipeR](https://systempipe.org/spr/)
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
            SPS Canvas is a unique tab which contains a unique **image editor**. It allows users to
             \"screenshot\" plots from other modules/ tabs and send to this Canvas to do further
            image editing and make a scientific figure.

            Simply click on the `To Canvas` button in various modules\ tabs will take a screenshot.

            *****

            ### Users' manual
            Visit [our website](https://systempipe.org/sps/) for details!

            *****

            ### Developers
            As a shiny framework. SPS provides a lot of functions to help developers to
            add more or customize SPS components and outside the SPS framework, like
            users own shiny Apps. These developer tools are provided in supporting
            packages. Read [this section on our website](https://systempipe.org/sps/dev) for more details.
            ") %>%
                div(class = "sps-dash"),
            spsHr(),
            column(
                12, class = "desc-table",
                h3(class="text-center", 'Other packages in systemPipeShiny'),
                markdown(
                '
                | Package | Description | Documents | Function reference | Demo |
                | --- | --- | --- | :---: | --- |
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/sps_small.png?raw=true" align="right" height="30" width="30"/>[systemPipeShiny](https://github.com/systemPipeR/systemPipeShiny) | SPS main package |[website](https://systempipe.org/sps/)|[link](https://systempipe.org/sps/funcs/sps/reference/)  | [demo](https://tgirke.shinyapps.io/systemPipeShiny/)|
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/spscomps.png?raw=true" align="right" height="30" width="30" />[spsComps](https://github.com/lz100/spsComps) | SPS UI and server components |[website](https://systempipe.org/sps/dev/spscomps/)|[link](https://systempipe.org/sps/funcs/spscomps/reference/)  | [demo](https://lezhang.shinyapps.io/spsComps)|
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/drawer.png?raw=true" align="right" height="30" width="30" />[drawer](https://github.com/lz100/drawer) | SPS interactive image editing tool |[website](https://systempipe.org/sps/dev/drawer/)|[link](https://systempipe.org/sps/funcs/drawer/reference/)  | [demo](https://lezhang.shinyapps.io/drawer)|
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/spsutil.png?raw=true" align="right" height="30" width="30" />[spsUtil](https://github.com/lz100/spsUtil) | SPS utility functions |[website](https://systempipe.org/sps/dev/spsutil/)|[link](https://systempipe.org/sps/funcs/spsutil/reference/)  | NA|
                '
                )
            )
        )
    )
}

## server
core_dashboardServer <- function(id, shared){
    module <- function(input, output, session, shared){
        ns <- session$ns
    }
    moduleServer(id, module)
}
