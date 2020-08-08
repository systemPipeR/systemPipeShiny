######################SPS #@tab_displayname@# tab######################
## creation date: #@crt_date@#
## Author: #@author@#

## UI for #@tab_id@#

#@tab_id@#UI <- function(id){
    ns <- NS(id)
    tagList(
        # write your own UI
    )
}

## server for #@tab_id@#

#@tab_id@#Server <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        tab_id <- "#@tab_id@#"
        # write your own server
    }
    moduleServer(id, module)
}
