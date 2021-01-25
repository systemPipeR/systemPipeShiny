## UI
#' @importFrom colourpicker colourInput
#' @importFrom shinyWidgets sliderTextInput
#' @noRd
core_canvasUI <- function(id){
    ns <- NS(id)
    desc <- "
    ### About this tab
      This is the SPS canavs. You can make scientific figures from this tab.

    ### Use canvas
      To start with tab, you need to prepare a plot by clicking the
    `To Canvas` button at other different tabs. A screenshot of a plot will be added
    to the `Images/ Plots` section on the left sidebar.

    Simply drag a plot you want to canvas and start your creation.

    - `View`, `Canvas` menus provide some options to change the canvas.
    - `Help` menu has some detailed instructions.
    - Use `File` menu to save your work.

    "
    tagList(
        tabTitle("systemPipeShiny Canvas"),
        spsHr(), renderDesc(id = ns("desc"), desc), spsHr(),
        canvas(id, on_start = 'li [href=\'#shiny-tab-core_canvas\']'),
        tags$br(),tags$br()
    )
}

## server
#' @importFrom shinydashboardPlus boxPlus
#' @importFrom shinyjqui orderInput
#' @noRd
core_canvasServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        # receive snap singal
       ## very clean back-end for new canvas
    }
    moduleServer(id, module)
}


