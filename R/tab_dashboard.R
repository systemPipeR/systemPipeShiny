## UI
dashboardUI <- function(id){
    ns <- NS(id)
    tagList(
        h2("Dashboard"), 
        h2(strong("This app is under construction, please do not use it for production.")),
        tags$p(strong("This page controls targets file and configuration of other parameters.")),
        p("If you cannot click some buttons, that means they are disabled at current tab or you need to do other things first, e.g. upload a file."),
        
        fluidRow(
            carousel(width = 12, 
                id = "dashcarousel",
                carouselItem( 
                    caption = "SPS",
                    tags$img(style = "width: 70%;", src = "systemPipeR_site.png", class = "img-responsive center-block")
                ),
                carouselItem(
                    caption = "Preview",
                    tags$img(style = "width: 70%;", src = "sps_wf.png", class = "img-responsive center-block")
                )
            )
        ),

    )
}

## server
dashboardServer <- function(input, output, session){
    
}
