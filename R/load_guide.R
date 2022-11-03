


parseGuide <- function(){
    if(!.validateGuide()) return(list(
        guide_ui = tags$li(class = "dropdown"),
        guide_content = list(empty_value = NULL)
    ))
    list(
        guide_ui = guideUI(guide_ui),
        guide_content = guide_content
    )
}

.validateGuide <- function(){
    if (emptyIsFalse(checkNameSpace("cicerone", quietly = TRUE))) {
        spswarn('No guide is loaded, please install "cicerone" package from CRAN')
        return(FALSE)
    }

    if (!exists('guide_ui')) {
        spswarn('No guide is loaded, object "guide_ui" is not found')
        return(FALSE)
    }
    if (!exists('guide_content')) {
        spswarn('No guide is loaded, object "guide_content" is not found')
        return(FALSE)
    }
    if(!all(
        lapply(guide_ui, function(x){
            if(x[['name']] != 'li') {
                spswarn('Expect all guide UI to be "li" tag')
                return(FALSE)
            }
            return(TRUE)
        }) %>% unlist()
    )) {spswarn('no guide will be loaded'); return(FALSE)}

    if(!all(
        lapply(seq_along(guide_content), function(x){
            if(!inherits(guide_content[[x]], "Cicerone") || is.na(names(guide_content)[x])) {
                spswarn('Expect all guide content should be
                        in a named list and each have "Cicerone" class')
                return(FALSE)
            }
            return(TRUE)
        }) %>% unlist()
    )) {spswarn('no guide will be loaded'); return(FALSE)}
    return(TRUE)
}


guideUI <- function(guide_ui){
    shinydashboard::dropdownMenu(
        type = "messages",
        headerText = "Choose a guide",
        icon = icon("chalkboard-user"),
        .list = guide_ui
    )
}
