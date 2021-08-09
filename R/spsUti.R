### Utility functions, can be run outside SPS

# #' Take steps output from subsetRmd and change to a nested list structure
# #' @description Data prepare for ShinyTree
# #' @param t_lvl positive integers, vector, levels of all title levels in Rmd
# #' @param t_text character strings, vector, text of titles
# #' @param start_lvl integer, default value is 0, but default
# #' level is 1 (0 + 1). level to start to create list
# #' @noRd
# #' @return a nested list
# #'
# # @examples
# # library(shiny)
# # library(shinyTree)
# # tree = step2listTree(t_lvl, t_text)
# # str(tree)
# #
# # tree_names = names(unlist(tree))
# #
# # ui = shinyUI(
# #     pageWithSidebar(
# #         mainPanel(
# #             shinyTree("tree", stripes = TRUE,
# #                       multiple = FALSE, animation = FALSE)
# #         )
# #     ))
# # server = shinyServer(function(input, output, session) {
# #     output$tree <- renderTree({
# #         tree
# #     })
# # })
# # shinyApp(ui, server)
# step2listTree <- function(t_lvl, t_text, start_lvl = 0){
#     if (t_lvl %>% unique() %>% length == 1){
#         tmp_lst <- list()
#         for (i in t_text){
#             tmp_lst[[as.character(i)]] <- ""
#         }
#         return(tmp_lst)
#     }
#     start_lvl <- start_lvl + 1
#     t_index <- which(t_lvl == start_lvl)
#     if (!length(t_index) == 0){
#         tmp_lst <- list()
#         for (i in seq_along(t_index)){
#             t_index <- c(t_index, length(t_lvl) + 1)
#             if_children <- t_index[i]  + 1 == t_index[i+1]
#             if (is.na(if_children) | if_children) {
#                 tmp_lst[[t_text[t_index[i]]]] <- ""
#             } else {
#                 children_lvl <- t_lvl[(t_index[i] + 1): (t_index[i + 1] -1)]
#                 children_name <- t_text[(t_index[i] + 1): (t_index[i + 1] -1)]
#                 tmp_lst[[t_text[t_index[i]]]] <-
#                     step2listTree(children_lvl, children_name, start_lvl)
#             }
#         }
#         return(tmp_lst)
#     } else {return("")}
# }



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
                          col_types = "cccccncc",
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

# can't import shiny internal function, gives warnings, so rewrite here
reactiveStop <- function(message = "\r              ", class = NULL){
    cond <- structure(list(message = message),
                      class = c(c("shiny.silent.error", class),
                                "error",
                                "condition")
    )
    stop(cond)
}


# print error trace back
printTraceback <- function(calls){
    calls <- calls[c(-length(calls), -length(calls) + 1)]
    trace_files <- findTraceFile(calls)
    paste0(
        crayon::green$bold(seq_along(calls)), ". ",
        as.character(calls), " ",
        crayon::blue$bold(trace_files)
    ) %>% cat(sep = "\n")
}

# find errors trace back file and line
findTraceFile <- function(calls) {
    lapply(calls, function(ca) {
        if (!is.null(srcref <- attr(ca, "srcref"))) {
            srcfile <- attr(srcref, "srcfile")
            glue('{srcfile$filename}#{srcref[1]}')
        } else ""
    })
}


# icon checker, internal func modified from shiny
validateIcon <- function(icon){
    if (!inherits(icon, "shiny.tag") || !icon$name == "i") {
        stop("Invalid icon. Use Shiny's 'icon()' function to generate a valid icon")
    }
}




# queue

simepleStack <- R6::R6Class(
    classname = "simepleStack",
    public = list(
        initialize = function(items = list(), limit = Inf){
            stopifnot(is.list(items))
            private$stack = self$push(items)
        },

        len = function(){
            length(private$stack)
        },

        get = function() {
            return(private$stack)
        },

        clear = function(){
            private$stack <- list()
        },

        push = function(items, after = self$len()) {
            stopifnot(is.list(items))
            if(!is.numeric(after)) stop("Push position must be numeric")
            if(length(after) != 1) stop("Push position cannot be more than 1 number")
            if(after > self$len())  stop("Push position cannot be more than current stack length")
            if(after < 0)  stop("Push position cannot be less than 0")
            private$stack <- append(private$stack, items, after)
        },

        pop = function(len=1, tail = FALSE){
            stopifnot(is.logical(tail) && length(tail) == 1)
            if(!is.numeric(len)) stop("Pop numbers must be numeric")
            if(len < 1)  stop("Pop length cannot be less than 1")
            stack_len <- self$len()
            if(len > stack_len) stop("Pop length cannot be more than stack length")
            len <- as.integer(len)
            if (tail) {
                stack_len <- self$len()
                pop_index <- seq(stack_len, stack_len - len)
                pop_index <- pop_index[pop_index != 0]
            } else {
                pop_index <- seq_len(len)
            }
            pop_items <- private$stack[pop_index]
            private$stack <- private$stack[-pop_index]
            pop_items
        }
    ),
    private = list(
        stack = list()
    )
)

historyStack <- R6::R6Class(
    classname = "historyStack",
    public = list(
        initialize = function(items = NULL, limit = 25, verbose = TRUE){
            if(is.null(items)) items <- list()
            stopifnot(is.list(items))
            stopifnot(is.numeric(limit))
            stopifnot(is.logical(verbose) && length(verbose) == 1)
            if(limit < 1) stop("Cannot create a history stack with limit less than 1.")
            if(limit == Inf) stop("Limit cannot be Inf.")

            limit <- as.integer(limit)
            private$limit = limit
            if(length(items) > limit) stop("Initial items length larger than limit.")

            item_len <- length(items)
            private$stack <- append(private$stack, items)

            private$len <- private$pos <- item_len
            private$last <- if(private$pos >= 0) TRUE
            private$first <- if(private$pos <= 1) TRUE else FALSE
            private$verbose <- verbose

            if(private$verbose) message("Created a history stack which can record  ", limit, " steps")
            invisible(self)
        },
        clear = function(){
            limit <- private$limit
            verbose <- private$verbose
            self$initialize(limit = limit, verbose = verbose)

            if(private$verbose) message("Stack clear")
            invisible(self)
        },
        get = function(pos = private$pos){
            stopifnot(is.numeric(pos) && length(pos) == 1)
            if(pos == Inf) stop("Position cannot be Inf.")
            pos <- as.integer(pos)
            if(pos > private$len) stop("Position is larger than current max history storage")
            item <- if(pos == 0) list() else private$stack[[pos]]
            list(
                item = item,
                pos = pos,
                first = private$first,
                last = private$last
            )
        },

        getPos = function(){
            private$pos
        },

        status = function(){
            list(
                pos = private$pos,
                len = private$len,
                limit = private$limit,
                first = private$first,
                last = private$last
            )
        },

        forward = function(){
            if(private$last) {
                if(private$verbose) warning("Already the last history, cannot move forward")
                return(NULL)
            }
            inc(private$pos)
            private$last <- if(private$pos == private$len) TRUE else FALSE
            private$first <- if(private$pos == 1) TRUE else FALSE

            self$get(private$pos)
        },

        backward = function(){
            if (private$first){
                if(private$verbose) warning("Already the first history, cannot move backward")
                return(NULL)
            }
            inc(private$pos, -1)
            private$first <- if(private$pos == 1) TRUE else FALSE
            private$last <- if(private$pos == private$len) TRUE else FALSE

            self$get(private$pos)
        },

        add = function(item) {
            pos <- private$pos
            stack <- private$stack
            item <- list(item)
            if (pos != private$limit) {
                stack <- if(length(stack)) append(stack[seq_len(pos)], item) else item
                inc(pos)
            } else {
                stack <- append(stack[-1], item)
            }
            private$stack <- stack
            private$len <- length(stack)
            private$pos <- pos
            private$first <- if(pos == 1) TRUE else FALSE
            private$last <- TRUE
            if(private$verbose) message("Added one item to position ", pos)
            invisible(self)
        }
    ),
    private = list(
        stack = list(),
        pos = 0,
        len = 0,
        limit = 25,
        first = TRUE,
        last = FALSE,
        verbose = NULL
    )
)


# operators
inc <- function(e1,e2 = 1) eval.parent(substitute(e1 <- e1 + e2))
mult <- function(e1,e2 = 2) eval.parent(substitute(e1 <- e1 * e2))
dev <- function(e1,e2 = 2) eval.parent(substitute(e1 <- e1 / e2))



## temp fix before new spsUtil  is on CRAN
emptyIsFalse <- function (x) {
    if (is.function(x)) return(TRUE)
    if (is.environment(x)) return(TRUE)
    if (length(x) < 1 || all(is.na(x)) || is.null(x)) return(FALSE)
    if (nchar(x[1]) == 0) return(FALSE)
    if (isFALSE(x)) return(FALSE)
    else TRUE
}
