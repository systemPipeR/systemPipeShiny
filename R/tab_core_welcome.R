## UI
#' @noRd
core_welcomeUI <- function(id, mod_missings){
    ns <- NS(id)
    mod_status <- lapply(c("wf", "rna", "ggplot", "canvas"), function(x) {
        if (x == "canvas") return(
            if(spsOption("tab_canvas")) "pass" else "disabled"
        )
        if(is.null(mod_missings[[x]])) "disabled" else {
            if(length(mod_missings[[x]]) == 0) "pass" else "missing"
        }
    })
    names(mod_status) <- c("wf", "rnaseq", "ggplot", "canvas")
    mod_src <- list(
        wf = list(img = "img/spr_notext.png", label = "Workflow", tabid = "wf"),
        rnaseq = list(img = "img/rnaseq_notext.png", label = "RNAseq", tabid = "rnaseq"),
        ggplot = list(img = "img/ggplot.png", label = "Quick ggplot", tabid = "vs_esq"),
        canvas = list(img = "img/drawer.png", label = "Canvas", tabid = "core_canvas")
    )

    modPopover <- function(tag, status) {
        if (status == "pass") return(tag)
        if (status == "missing") return(bsPop(
            tag, "Package(s) Missing", status = "warning", contentweight = "bold",
            titlesize = "2rem", contentsize = "1.5rem", titleweight = "bold",
            "Some required packages are not installed to use this module. You
            can check the module tab to see what needs to be installed."))
        if (status == "disabled") return(bsPop(
            tag, "Module Disabled", status = "danger", contentweight = "bold",
            titlesize = "2rem", contentsize = "1.5rem", titleweight = "bold",
            "This module is disabled by your SPS manager."))
    }
    div(
        style = "postition: relative",
        tags$script(src = "sps/js/sps_welcome.js"),
        tags$link(rel = "stylesheet", href = "sps/css/sps_welcome.css"),
        tags$style('.skin-blue .main-header .navbar {background-color: #2c8abf;}'),
        div(class = "welcome-header",
            div(id = "welcome-svg"),
            lapply(c("wf", "rnaseq", "ggplot", "canvas"), function(x) {
                div(
                    class = glue("card {x}"), `data-tilt`="", `data-tilt-max`="30",
                    `data-tilt-scale`="1.3", `data-tilt-speed`="800",
                    `data-desc` = x, `data-status` = mod_status[[x]],
                    tags$img(src = mod_src[[x]][['img']]),
                    tags$span(mod_src[[x]][['label']]),
                    onclick= glue("document.querySelector('a[href=\"#shiny-tab-{mod_src[[x]][['tabid']]}\"]').click()")
                ) %>% modPopover(mod_status[[x]])
            }),
            div(
                class = "mod-desc",
                h5(`data-desc` = "wf", "Workflow Module: Choose, design, and run systemPipeR workflows
                   interactively in a guided manner."),
                h5(`data-desc` = "rnaseq", "RNAseq Module: perform downstream
                   RNAseq analysis, like clustering, DEG, plotting, and more."),
                h5(`data-desc` = "ggplot", "Quick ggplot Module: Make ggplots
                   from any tabular-like datasets users provide."),
                h5(`data-desc` = "canvas", "Canvas Tool: Interactively edit plots you made in all
                   other modules.")
            ),
            actionBttn(
                inputId = ns("go_down"),
                label = "Read More",
                icon = icon("angle-double-down"),
                style = "jelly",
                color = "primary"
            )
        ),
        tags$script(src = "sps/js/vanilla-tilt.js"),
        fluidRow(
            class = "text-main welcome-text",
            spsHr(),
            markdown(
            "
            ## To start
            Start by choosing a **module** above or from the left sidebar.

            *****

            ### SPS Modules
            A SPS module is a complex app unit for a certain purpose. Usually a
            module is built by some smaller units, we call them *\"subtabs\"*. Under
            current version of SPS, there are 3 pre-built modules.

            1. [**Workflow**{blk}](https://systempipe.org/sps/modules/workflow/): Choose, design, and run [systemPipeR{blk}](https://systempipe.org/sp/)
               workflows interactively in a guided manner.
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
            br(),
            br()
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
