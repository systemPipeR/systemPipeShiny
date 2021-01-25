### Utility functions, can be run outside SPS

#' Take steps output from subsetRmd and change to a nested list structure
#' @description Data prepare for ShinyTree
#' @param t_lvl positive integers, vector, levels of all title levels in Rmd
#' @param t_text character strings, vector, text of titles
#' @param start_lvl integer, default value is 0, but default
#' level is 1 (0 + 1). level to start to create list
#' @noRd
#' @return a nested list
#'
# @examples
# library(shiny)
# library(shinyTree)
# tree = step2listTree(t_lvl, t_text)
# str(tree)
#
# tree_names = names(unlist(tree))
#
# ui = shinyUI(
#     pageWithSidebar(
#         mainPanel(
#             shinyTree("tree", stripes = TRUE,
#                       multiple = FALSE, animation = FALSE)
#         )
#     ))
# server = shinyServer(function(input, output, session) {
#     output$tree <- renderTree({
#         tree
#     })
# })
# shinyApp(ui, server)
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
                tmp_lst[[t_text[t_index[i]]]] <-
                    step2listTree(children_lvl, children_name, start_lvl)
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
#' @noRd
# @examples
# step_name <- c("1.1.1", "2.2.2")
# findTreeParent(step_name)
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
#' @param t_lvl title markdown heading levels, 1-5
#' @param t_text title text
#' @param start_lvl starting title level
#'
#' @return list
#' @importFrom stats na.omit
#' @noRd
# @examples
# t_lvl = c(1, 3, 1, 2, 2, 3)
# t_text = c('1', '1.1.1', '2', '2.1', '2.2', '2.2.1')
# test = step2listD3(t_lvl, t_text)
# str(test)
# diagonalNetwork(test)
step2listD3 <- function(t_lvl, t_text, start_lvl = 0){
    if (is.null(t_lvl) | is.null(t_text))
        return(list(name = "Nothing has been loaded"))
    findChildren <- function(t_lvl, t_text, start_lvl){
        start_lvl <- start_lvl + 1

        t_index <- NULL
        while (start_lvl <= max(stats::na.omit(t_lvl))) {
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
                list(name = t_text[t_index[i]],
                     children = findChildren(children_lvl,
                                             children_name,
                                             start_lvl)
                     )
            })
            return(tmp_lst)
        } else { return(list(name = ""))}
    }
    if (t_lvl %>% unique() %>% length == 1){
        tmp_lst = list()
        for (i in t_text){
            tmp_lst <- append(tmp_lst,
                              list(list(name = i, children = list(name = ""))))
        }
        return(list(name = "File", children = tmp_lst))
    }
    return(
        list(name = "File", children = findChildren(t_lvl, t_text, start_lvl))
    )
}


#' Suppress cat print output
#' @description Useful if you want to suppress cat or print
#' @param x function or expression or value assignment expression
#' @export
#' @return If your original fucntions has a return, it will return in
#' `invisible(x)`
#' @examples
#' quiet(print(1))
#' quiet(cat(1))
quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}


#' check namespace
#' @description  Help you to check if you have certain packages and
#' return missing package names
#' @param packages vector of strings
#' @param quietly bool, give you warning on fail?
#' @param from  string, where this package is from like, "CRAN", "GitHub", only
#' for output message display purpose
#' @importFrom shinyAce is.empty
#' @return vector strings, of missing package names
#' @noRd
# @examples
# checkNameSpace("ggplot2")
checkNameSpace <- function(packages, quietly = FALSE, from = "") {
    if (shinyAce::is.empty(packages)) return(NULL)
    missing_pkgs <- lapply(packages, function(pkg) {
        if (!eval(parse(text = "requireNamespace(pkg, quietly = TRUE)"))) pkg
    }) %>% unlist()
    if (!quietly & not_empty(missing_pkgs)) {
        spswarn(glue("These packages are missing from",
                 "{from}: {glue_collapse(missing_pkgs, sep = ',')}"))
        }
    return(missing_pkgs)
}

#' Find tab information from tabs.csv
#' If `type` is not empty, `tab_ids` will be ignored
#' @importFrom vroom vroom
#'
#' @param tab_ids vector of strings, tab names you want to get
#' @param type tab type and sub type, one of: core, wf, vs, data, plot, or
#' addition type you specific in type or type_sub column, first search type and
#' then type_sub
#' @param tab_file tab file path
#' @param force_reload bool, tab info usually stores at a variable
#' called `tab_info`. This function first look for that one, if not exists,
#' read from file. This argument forcedly read from file and ignore that
#' variable.
#' @importFrom shinyAce is.empty
#' @importFrom vroom vroom
#' @return a list contains `tab_id`, `tab_labels`, `hrefs`
#' reference, `image` path,
#' `tpye` and `tpye_sub`
#' @noRd
#'
# @examples
# tab_ids <- c("core_about", "vs_main")
# findTabInfo(tab_ids, tab_file = tab_file)
findTabInfo <- function(tab_ids=NULL, type = NULL,
                        tab_file = file.path("config", "tabs.csv"),
                        force_reload = FALSE) {
    if(is.null(type)) assert_that(is.character(tab_ids))
    tabs <- if(exists("tab_info") & !force_reload) {
        tab_info
    } else {
         suppressMessages(
             vroom::vroom(tab_file, comment = "#", na = character(),
                          altrep = FALSE))
    }
    # if(!spsOption('dev')){
    #     tabs <- tabs[!str_detect(tabs$tab_id, "_template$"), ]
    #     tab_ids <- tab_ids[!str_detect(tab_ids, "_template$")]
    #     }
    if(not_empty(type)) {
        type <- match.arg(type,
                          unique(c(tabs[['type']], tabs[['type_sub']])) %>%
                              {.[. != ""]})
        tab_nos <- tabs$type %in% type
        if (!any(tab_nos)) tab_nos <- tabs$type_sub %in% type
        if (!any(tab_nos)){
            spswarn(glue("This tab type '{type}'",
                         "contains no tab, check the type"))
            return(NULL)
        }
    } else {
        tab_nos <- vapply(tab_ids, function(x) {
            tab_no <- str_which(pattern = glue("^{x}$"), string = tabs$tab_id)
            if (shinyAce::is.empty(tab_no)){
                spserror(glue("Tab {x} is not in the tab list"))
            } else if(length(tab_no) > 1){
                glue_collapse(tabs$tab_id[duplicated(tabs$tab_id)], sep=", ")%>%
                    {spserror(glue("Find duplicated ID(s) {.}"))}
            }
            tab_no
        }, 1)
    }
    structure(
        list(
        tab_id = tabs$tab_id[tab_nos],
        tab_labels = tabs$display_label[tab_nos],
        hrefs = glue("#shiny-tab-{tabs$tab_id[tab_nos]}"),
        images = tabs$image[tab_nos],
        tpye = tabs$type[tab_nos],
        type_sub = tabs$type_sub[tab_nos]
        ),
        class = c("list", "sps-tabinfo")
    )
}

# LZ note: do not use spsOption function here,
# because it is depend on this function.
# If used, two functions depending on each other and creates infinite loop


#' @title SPS terminal message logging methods
#' @description If SPS `use_crayon`option is `TRUE`, the message will
#' be colorful.
#' "INFO" level spawns `message`, "WARNING" is `warning`, "ERROR" spawns `stop`,
#' other levels use `cat`.
#' `spsinfo`, `spswarn`, `spserror` are higher level wrappers of `msg`. The
#' only difference is they have `SPS-` prefix.
#'
#' `spsinfo` has an additional
#' arg `verbose`. This arg works similar to all other `verbose` args in this
#' package, if not specified, it follows the project option, can be be forced to
#' `TRUE` and `FALSE`. `TRUE` will forcefully generate the msg, and `FALSE`
#' will be no message.
#' @importFrom crayon blue make_style red
#' @param msg a character string of message or a vector of character strings,
#' each item in the vector presents one line of words
#' @param .other_color hex color code or named colors, when levels are not in
#' "INFO", "WARNING", "ERROR", this value will be used
#' @param level typically, one of "INFO", "WARNING", "ERROR", lower case OK.
#' Other levels ok.
#' @param info_text info level text prefix
#' @param warning_text warning level text prefix
#' @param error_text error level text prefix
#' @return see description
#' @details If you want to use this function to generate colorful log messages
#' but not in SPS framework. Use [spsOption] to set up the option on very top
#' of your scripts: `spsOption("use_crayon", TRUE)`.
#' @examples
#' msg("this is info")
#' msg("this is warning", "warning")
#' try(msg("this is error", "error"))
#' msg("this is other", "error2")
#' spsinfo("some msg, verbose false", verbose = FALSE) # will not show up
#' spsinfo("some msg, verbose true", verbose = TRUE)
#' spswarn("sps warning")
#' try(spserror("sps error"))
#'
#' @export
msg <- function(msg,
                level = "INFO",
                .other_color="white",
                info_text = "INFO",
                warning_text = "WARNING",
                error_text = "ERROR"){
    msg <- paste0(msg, collapse = "")
    info <- warn <- err <- other <- function(msg){return(msg)}
    if(!is.null(getOption('sps')[['use_crayon']])){
        if(getOption('sps')[['use_crayon']]){
            info <- crayon::blue$bold
            warn <- crayon::make_style("orange")$bold
            err <- crayon::red$bold
            other <- crayon::make_style(.other_color)$bold
        }
    }
    level_text <- switch(toupper(level),
        "WARNING" = warning_text,
        "ERROR" = error_text,
        "INFO" = info_text,
        level
    )
    msg <- if(str_detect(msg, "\\[.*\\] [0-9]{4}-[0-9]{2}")) msg
           else glue("[{level_text}] {Sys.time()} {msg}")

    switch(toupper(level),
        "WARNING" = warning("\r", warn(msg), call. = FALSE, immediate. = TRUE),
        "ERROR" = stop("\r", err(msg), call. = FALSE),
        "INFO" = message(info(msg)),
        cat(other(msg), sep = "\n")
    )
}


#' @param verbose bool, default get from sps project options, can be overwritten
#' @rdname msg
#' @export
spsinfo <- function(msg, verbose=NULL) {
    verbose <- if(is.null(verbose)) spsOption('verbose')
               else {assert_that(is.logical(verbose)); verbose}
    if(verbose) msg(msg, "info", info_text =  "SPS-INFO")
}

#' @rdname msg
#' @export
spswarn <- function(msg) msg(msg, "warning", warning_text = "SPS-WARNING")

#' @rdname msg
#' @export
spserror <- function(msg) msg(msg, "error", error_text = "SPS-ERROR")


#' Remove ANSI color code
#' @description borrowed from crayon package, since
#' this package is just suggested
#' by SPS, use some code from crayon
#' @param string string
#' @noRd
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

#' Empty objects and FALSE will return FALSE
#' @description judge if an object is empty or FALSE, and return FALSE if it is
#' @details  not working on S4 class objects.
#'
#' Useful for if statement. Normal empty object in if will spawn error. Wrap the
#' expression with `emptyIsFalse` can avoid this. See examples
#' @param x any R object
#'
#' @export
#' @return `NA`, `""`, `NULL`, `length(0)`, `nchar == 0` and `FALSE` will return
#' `FALSE`, otherwise `TRUE`.
#' @examples
#' emptyIsFalse(NULL)
#' emptyIsFalse(NA)
#' emptyIsFalse("")
#' try(`if(NULL) "not empty" else "empty"`) # will generate error
#' if(emptyIsFalse(NULL)) "not empty" else "empty" # this will work
#' # similar for `NA`, `""`, `character(0)` and more
emptyIsFalse <- function(x){
    if(is.function(x)) return(TRUE)
    if(length(x) > 1)  return(TRUE)
    if(length(x) == 0) return(FALSE)
    if(is.na(x)) return(FALSE)
    if(nchar(x) == 0) return(FALSE)
    if(isFALSE(x)) return(FALSE)
    else TRUE
}

# can't import shiny internal function, gives warnings, so rewrite here
reactiveStop <- function(message = "\r              ", class = NULL){
    cond <- structure(list(message = message),
                      class = c(c("shiny.silent.error", class),
                                "error",
                                "condition")
    )
    stop(cond)
}

## check and time out when opening a online file
#' @importFrom httr GET timeout stop_for_status
checkUrl <- function(url, timeout = 5){
    if(!stringr::str_detect(url, "^http")) stop("url need to start with 'http(s)'")
    if(!is.numeric(timeout) | timeout < 0) stop("timeout need to a > 0 number")
    tryCatch({
        httr::GET(url, httr::timeout(timeout)) %>%
            httr::stop_for_status()
        TRUE
            },
        error = function(e){
            spswarn(glue("Bad url {url}"))
            spswarn(e$message)
            return(FALSE)
        }
    )
}


## rewrite some imports here

