## UI
#' @importFrom shinydashboardPlus carousel carouselItem
#' @noRd
core_dashboardUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("Welcome to systemPipeShiny"),
        tags$style(".sps-dash .desc-body.collapse:not(.in) {height: 400px;}"),
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

        1. **Workflow**: Choose, design, and run [systemPipeR](http://www.bioconductor.org/packages/devel/bioc/vignettes/systemPipeR/inst/doc/systemPipeR.html)
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

        ### Vignette
        There is a [vignette](https://systempipe.org/systemPipeShiny/articles/systemPipeShiny.html)
        to provide more details. If you are new to SPS, click on the top **right corner for interactive tutorials**.

        *****

        ### Developers
        As a shiny framework. SPS provides a lot of functions to help developers to
        add more or customize SPS components. Read our
        [Reference](https://systempipe.org/systemPipeShiny/reference/index.html)
        for more details.
        ") %>%
            div(class = "sps-dash"),
        spsHr(),
        fluidRow(
            shinydashboardPlus::carousel(width = 12,
                id = "dashcarousel",
                shinydashboardPlus::carouselItem(
                    caption = "SPS",
                    tags$img(style = "width: 500px;",
                             src = "img/sps.png",
                             class = "center-block")
                ),
                shinydashboardPlus::carouselItem(
                    caption = "SPS Structure",
                    tags$img(style = "width: 500px;",
                             src = "img/sps_structure.png",
                             class = "center-block")
                )
            )
        ),

    )
}

## server
core_dashboardServer <- function(id, shared){
    module <- function(input, output, session, shared){
        ns <- session$ns
    }
    moduleServer(id, module)
}
