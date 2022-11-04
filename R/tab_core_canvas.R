## UI
#' @noRd
core_canvasUI <- function(id){
    ns <- NS(id)
    desc <- '
    ### About this tool
    Read more instructions, view the video guide and more on [our website{blk}](https://systempipe.org/sps/dev/drawer/)

    #### Expand to watch the tutorial
    <div style="text-align: center;">
        <video style="width: 100%; aspect-ratio: 16 / 9"  controls>
            <source src="https://user-images.githubusercontent.com/35240440/199858040-9d5443ff-a0ef-4bbe-b4e7-93aa442e64fa.mp4" type="video/mp4">
            Video cannot be loaded or your browser does not support the video tag.
        </video>
    </div>

    ### Use canvas
    To start with tab, you need to prepare a plot by clicking the
    `To Canvas` button at other different tabs. A screenshot of a plot will be added
    to the `Images/ Plots` section on the left sidebar.

    Simply drag a plot you want to canvas and start your creation.

    - `View`, `Canvas` menus provide some options to change the canvas.
    - `Help` menu has some detailed instructions.
    - Use `File` menu to save your work.

    '
    tagList(
        tabTitle("systemPipeShiny Canvas"),
        spsHr(), renderDesc(id = ns("desc"), desc), spsHr(),
        canvas(id, on_start = 'li [href=\'#shiny-tab-core_canvas\']'),
        tags$br(),tags$br()
    )
}

## server
#' @noRd
core_canvasServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        ## very clean back-end for new canvas
    }
    moduleServer(id, module)
}


