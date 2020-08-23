###################### SPS my first empty data tab tab######################
## creation date: 2020-08-23 11:02:03
## Author:

# data_empty UI
data_emptyUI <- function(id) {
    ns <- NS(id)
    tagList(
        # write your own UI
    )
}

## data_empty server

data_emptyServer <- function(id, shared) {
    module <- function(input, output, session) {
        ns <- session$ns
        tab_id <- "data_empty"
        # write your own server
    }
    moduleServer(id, module)
}
