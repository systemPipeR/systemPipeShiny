#################### SPS loading screen functions ######################
# Use this on UI
renderLoading <- function(){
    switch(getOption("sps")$loading_theme %>% as.character(),
           "vhelix" = loadingVhelix(),
           loading404()
    )
}
# Use this on server
serverLoadingScreen <- function(input, output, session){
    onclick('loading-screen', {
        shinyjs::hide("loading-screen", anim = TRUE, animType = "fade")
        shinyjs::show("app-main", anim = TRUE, animType = "fade")
    })
}



loading404 <- function(){
    includeHTML("www/nyan.html")
}


loadingVhelix <- function(){
    tagList(
        tags$script(src="double-helix.js", type="text/javascript"),
        img(src = "systemPipeR_site.png", width = 300, height = 300,style = "left: 45%; top: 35%; position:fixed"),
        div(id="helix", style="height: 300px; width: 300px; left: 45%; top: 52%; position:relative")
    )
}
