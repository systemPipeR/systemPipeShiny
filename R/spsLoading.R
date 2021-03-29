#################### SPS loading screen functions ######################
# Use this on UI
renderLoading <- function(theme){
    switch(theme,
           "vhelix" = includeHTML("www/loading_themes/vhelix.html"),
           "hhelix" = includeHTML("www/loading_themes/hhelix.html"),
           "biomatrix" = includeHTML("www/loading_themes/biomatrix.html"),
           "empty" = includeHTML("www/loading_themes/empty.html"),
           includeHTML("www/loading_themes/nyan.html")
    )
}


