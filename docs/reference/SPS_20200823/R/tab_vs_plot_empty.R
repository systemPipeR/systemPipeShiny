###################### SPS my first empty plot tab tab######################
## creation date: 2020-08-23 11:02:03
## Author:

## UI for plot_empty

plot_emptyUI <- function(id) {
    ns <- NS(id)
    tagList(
        # write your own UI
    )
}

## server for plot_empty

plot_emptyServer <- function(id, shared) {
    module <- function(input, output, session) {
        ns <- session$ns
        tab_id <- "plot_empty"
        # write your own server
    }
    moduleServer(id, module)
}
