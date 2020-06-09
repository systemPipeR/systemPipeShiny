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
    if (is.empty(packages)) return(NULL)
    missing_pkgs <- lapply(packages, function(pkg) {
        if (!requireNamespace(pkg, quietly = TRUE)) pkg
    })
    missing_pkgs <- lapply(packages, function(pkg) {
        if (!eval(parse(text = "requireNamespace(pkg, quietly = TRUE)"))) pkg
    }) %>% unlist()
    if (!quietly & not_empty(missing_pkgs)) {
        msg(glue("These packages are missing from",
                 "{from}: {glue::glue_collapse(missing_pkgs, sep = ',')}"))
        }
    return(missing_pkgs)
}

#' Find tab information from tabs.csv
#'
#' @importFrom vroom vroom
#'
#' @param tabnames vector of strings, tab names you want to get
#' @param type tab type, one of : core, wf, data, vs
#'
#' @return a list contains `tab_labels`, `hrefs` reference, `images`
#' @export
#'
#' @examples
#' tabnames <- c("wf_wf", "d", "sas")
#' findTabInfo(tabnames)
findTabInfo <- function(tabnames=NULL, type = NULL) {
    if(is.null(type)) assert_that(is.character(tabnames))

    appDir <- options()$sps$appDir
    tabs <- if (exists("tab_info")) {
        tab_info
    } else {
        vroom(glue("{appDir}/config/tabs.csv"), comment = "#", na = character())
    }
    if(!getOption('sps')$dev){
        tabs <- tabs[!str_detect(tabs$Tab_name, "_template$"), ]
        tabnames <- tabnames[!str_detect(tabnames, "_template$")]
        }
    if(not_empty(type)) {
        tabs <- tabs[tabs$type %in% type, ]
        tab_nos <- seq_len(nrow(tabs))
    } else {
        tab_nos <- vapply(tabnames, function(x) {
            tab_no <- str_which(glue("^{x}$"), tabs$Tab_name)
            if (!not_empty(tab_no)){
                stop(glue("Tab {x} is not in the tab list"), call. = FALSE)
                return(NA)}
            tab_no
        }, 1)
    }
    list(
        tab_labels = tabs$Display_label[tab_nos],
        hrefs = glue("#shiny-tab-{tabs$Tab_name[tab_nos]}"),
        images = tabs$image[tab_nos]
        ) %>% return()
}


#' SPS terminal message
#' @description If `crayon` is installed, the message will be colorful.
#' "INFO" level spawns `message`, "WARNING" is `warning`, "ERROR" spawns `stop`,
#' other levels use `cat`
#'
#' @param msg a character string of message or a vector of character strings,
#' each item in the vector presents one line of words
#' @param .other_color hex color code or named colors, when levels are not in
#' "INFO", "WARNING", "ERROR", this value will be used
#' @param level typically, one of "INFO", "WARNING", "ERROR", lower case OK.
#' Other levels ok.
#'
#' @return
#' @export
#'
#' @examples
#' msg("this is info")
#' msg("this is warning", "warning")
#' msg("this is error", "error")
#' msg("this is other", "error")
msg <- function(msg, level = "INFO", .other_color="white") {
    msg <- paste0(msg, collapse = "")
    info <- warn <- err <- other <- function(msg){return(msg)}
    if(is.null(getOption('sps')$use_crayon)) {
        options(sps = getOption("sps") %>% {.[['use_crayon']] <- TRUE; .})
    }
    if (getOption('sps')$use_crayon){
        if (!requireNamespace("crayon", quietly = TRUE)) {
            options(sps = getOption("sps") %>% {.[['use_crayon']] <- FALSE; .})
            message(glue("[INFO] {Sys.time()} If you want colorful",
                         "systemPipeShiny messages, try install 'crayon'",
                         "package"))
        } else{
            info <- crayon::blue$bold
            warn <- crayon::make_style("orange")$bold
            err <- crayon::red$bold
            other <- crayon::make_style(.other_color)$bold
        }
    }
    msg <- glue("[{level}] {Sys.time()} {msg}")
    switch (toupper(level),
        "WARNING" = warning(warn(msg), call. = FALSE, immediate. = TRUE),
        "ERROR" = stop(err(msg), call. = FALSE),
        "INFO" = message(info(msg)),
        cat(other(msg), sep = "\n")
    )
}

#' Remove ANSI color code
#' @description borrowed from crayon package, since this package is not required
#' by SPS, use some code from crayon
#' @param string string
remove_ANSI <- function(string) {
    ANSI <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})(?:(?:[0-9]{1,3})?(?:",
                   "(?:;[0-9]{0,3})*)?[A-M|f-m])|\\x{001b}[A-M]")
    gsub(ANSI, "", string, perl = TRUE)
}


timeline_icon <- function(bool = FALSE){
    if (bool) return("check") else return("times")
}

timeline_color <- function(bool = FALSE){
    if (bool) return("olive") else return("red")
}
timeline_pg <- function(bool = FALSE){
    if (bool) return(100) else return(0)
}

timline_pg_status <- function(progress = 0){
    if(progress <= 33.4) "warning"
    else if(progress < 66.7) "info"
    else "success"
}
