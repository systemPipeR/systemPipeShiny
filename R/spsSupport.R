#' @import  stringr magrittr glue 
NULL


#' Title Take steps output from subsetRmd and change to a nested list structure
#' Data prepare for ShinyTree
#' @param t_lvl positive integers, vector, levels of all title levels in Rmd
#' @param t_text character strings, vector, text of titles
#' @param start_lvl integer, default value is 0, but default level is 1 (0 + 1). level to start to create list
#'
#' @return a nested list
#'
#' @examples
#' library(shiny)
#' library(shinyTree)
#' tree = step2listTree(t_lvl, t_text)
#' str(tree)
#' 
#' tree_names = names(unlist(tree))
#' 
#' ui = shinyUI(
#'     pageWithSidebar(
#'         mainPanel(
#'             shinyTree("tree", stripes = TRUE, multiple = FALSE, animation = FALSE)
#'         )
#'     ))
#' server = shinyServer(function(input, output, session) {
#'     output$tree <- renderTree({
#'         tree
#'     })
#' })
#' shinyApp(ui, server)
step2listTree <- function(t_lvl, t_text, start_lvl = 0){
    if (t_lvl %>% unique() %>% length == 1){
        tmp_lst <- list()
        for (i in t_text){
            tmp_lst[[as.character(i)]] <- ""
        }
        return(tmp_lst)
    }
    start_lvl <- start_lvl + 1
    t_index <- which(t_lvl == start_lvl)
    if (!length(t_index) == 0){
        tmp_lst <- list()
        for (i in seq_along(t_index)){
            t_index <- c(t_index, length(t_lvl) + 1)
            if_children <- t_index[i]  + 1 == t_index[i+1]
            if (is.na(if_children) | if_children) {
                tmp_lst[[t_text[t_index[i]]]] <- ""
            } else {
                children_lvl <- t_lvl[(t_index[i] + 1): (t_index[i + 1] -1)]
                children_name <- t_text[(t_index[i] + 1): (t_index[i + 1] -1)]
                tmp_lst[[t_text[t_index[i]]]] <- step2listTree(children_lvl, children_name, start_lvl)
            }
        }
        return(tmp_lst)
    } else {return("")}
}



#' find parent steps of from output of jsTree
#'
#' @param step_names 
#'
#' @return vector strings of major and minor step numbers
#'
#' @examples
#' step_name <- c("1.1.1", "2.2.2")
#' findTreeParent(step_name)
findTreeParent <- function(step_names){
    lapply(step_names, function(each_name){
        if (str_detect(each_name, "\\.")) {
            step_p <- str_remove(step_names, "[^0-9]+.[^.]*$")
            tmp_holder <- c(step_p, findTreeParent(step_p))
            return(c(step_names, tmp_holder))
        } else {return(each_name)}
    }) %>%
    unlist() %>%
    unique() %>% str_sort(numeric = TRUE)
}


#' Create structure for networkD3 object
#'
#' @param t_lvl 
#' @param t_text 
#' @param start_lvl 
#'
#' @return list
#'
#' @examples
#' t_lvl = c(1, 3, 1, 2, 2, 3)
#' t_text = c('1', '1.1.1', '2', '2.1', '2.2', '2.2.1')
#' test = step2listD3(t_lvl, t_text)
#' str(test)
#' diagonalNetwork(test)
step2listD3 <- function(t_lvl, t_text, start_lvl = 0){
    if (is.null(t_lvl) | is.null(t_text)) return(list(name = "Nothing has been loaded"))
    findChildren <- function(t_lvl, t_text, start_lvl){
        start_lvl <- start_lvl + 1

        t_index <- NULL
        while (start_lvl <= max(na.omit(t_lvl))) {
            t_index <- which(t_lvl == start_lvl)
            if (length(t_index) == 0) {
                start_lvl <- start_lvl + 1
            } else {
                break()
            }
        }
        
        if (!length(t_index) == 0) {
            tmp_lst <- lapply(seq_along(t_index), function(i){
                t_index <- c(t_index, length(t_lvl) + 1)
                children_lvl <- t_lvl[(t_index[i] + 1) : (t_index[i+1] - 1)]
                children_name <- t_text[(t_index[i] + 1) : (t_index[i+1] - 1)]
                list(name =  t_text[t_index[i]], children = findChildren(children_lvl, children_name, start_lvl))
            })
            return(tmp_lst)
        } else { return(list(name = ""))}
    }
    if (t_lvl %>% unique() %>% length == 1){
        tmp_lst = list()
        for (i in t_text){
            tmp_lst <- append(tmp_lst, list(list(name = i, children = list(name = ""))))
        }
        return(list(name = "File", children = tmp_lst))
    }
    return(list(name = "File", children = findChildren(t_lvl, t_text, start_lvl)))
}


#' supress cat output
#'
#' @param x function or expression or value assignment expression
#'
#' @return
#'
#' @examples
quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
} 


#' check namespace 
#' Help you to check if you have certain packages and return missing package names
#' @param packages vector of strings
#' @param quietly bool, give you error on fail?
#' @param from  string, where this package is from like, "CRAN", "GitHub", only
#' for output message display purpose
#'
#' @return vector strings, of missing package names
#'
#' @examples
#' checkNameSpace("ggplot2")
checkNameSpace <- function(packages, quietly = FALSE, from = "") {
    missing_pkgs <- lapply(packages, function(pkg) {
        if (!requireNamespace(pkg, quietly = TRUE)) pkg
    }) %>% unlist()
    if (!quietly) message(glue("These packages are missing from {from}: {glue::glue_collapse(missing_pkgs, sep = ',')}"))
    return(missing_pkgs)
}

#' Find tab information from tabs.csv
#'
#' @importFrom readr read_csv
#' @param tabnames vector of strings, tab names you want to get
#'
#' @return a list contains tab labels, tab hyper reference, images
#' @export
#'
#' @examples
#' tabnames <- c("wf_wf", "d", "sas")
#' findTabInfo(tabnames)
findTabInfo <- function(tabnames) {
    assert_that(is.character(tabnames))
    tabs <- if (exists("tab_info")) {
        tab_info
    } else {
        read_csv("tabs.csv", comment = "#", na = character())
    }
    tabnames %in% tabs$Tab_name %>% {
        glue("Tab {tabnames[!.]} is not in the tab list") %>% lapply(warning, call. = FALSE) %>% quiet()
        list(
            tab_labels = tabs$Display_label[.],
            hrefs = glue("#shiny-tab-{tabs$Tab_name[.]}"),
            images = tabs$image[.]
        )
    } %>% return()
}
