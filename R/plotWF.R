## Internal funcs for plotting Workflow


# Graphviz plot workflow
# df_wf: data frame, the standard df generated from subsetRmd function
# out_type: choose from 'html', 'svg', 'png'
# out_path: string, path of output plot, only apply from svg or png
# plot_style:  one of 'detect', 'none', 'linear'
# default is detect, ignoring level but detecting the the longest branch
# linking frist and last step as main branch.
# height: int, height of svg or png in pixels, default NULL, automatic
# width: int, width of svg or png in pixels, default NULL, automatic
# only selected steps will be plotted, make sure at least some steps are
# TRUE in 'selected' column in df_wf

#' @importFrom DOT dot
#' @importFrom rsvg rsvg_svg rsvg_png
#' @noRd
.plotWF <- function(df_wf, plot_style="detect", out_type='html',
                   out_path='default', height=NULL, width=NULL){
    # pre checks
    assert_that(out_type %in% c('html', 'png', 'svg', 'shiny'),
                msg = "output type needs to be one of 'html', 'png', 'svg'")
    assert_that(plot_style %in% c('detect', 'none', 'linear'),
                msg="plot style needs to be one of 'detect', 'none', 'linear'")
    assert_that(is.data.frame(df_wf))
    all(c("t_lvl", "t_number", "t_text","selected",
          "no_run", "no_success", "link_to") %in% names(df_wf)) %>%
        assert_that(msg=glue('One of following columns is missing: "t_lvl"',
                             '"t_number" "t_text" "selected"',
                             '"no_run" "no_success" "link_to"'))
    if (out_path == 'default' & !out_type %in% c('html', 'shiny')){
        assert_that(is.writeable(out_path))
        assert_that(is.count(height) | is.null(height))
            assert_that(is.count(width) | is.null(width))
        out_path = switch(out_type,
                          'svg' = paste0('wfplot',
                                         format(Sys.time(), "%Y%m%d%H%M%S"),
                                         '.svg'),
                          'png' = paste0('wfplot',
                                         format(Sys.time(), "%Y%m%d%H%M%S"),
                                         '.png')
        )
    }
    df_wf <- df_wf[df_wf$selected == TRUE, ]
    if (nrow(df_wf) == 0) return(cat("no step is selected"))

    wf <- .make_plot(df_wf, plot_style, is_main_branch=FALSE)
    wf <- append(wf, .make_plot(df_wf, plot_style, is_main_branch=TRUE),
                 after = length(wf) - 1)
    # special case for detection style plotting, need to move unneeded
    # nodes out of main branch
    if (plot_style == "detect") wf <- .change_branch(df_wf, wf)
    # collapse entire graph
    # return(wf)
    wf <- paste0(wf, collapse = "")
    # plot
    plot <- switch(out_type,
           'shiny' = DOT::dot(wf, return = "verbatim"),
           'html' = DOT::dot(wf),
           'svg' = DOT::dot(wf, return = "verbatim") %>%
               rsvg::rsvg_svg(file = out_path, height = height, width = width),
           'png' = DOT::dot(wf, return = "verbatim") %>% charToRaw() %>%
               rsvg::rsvg_png(file = out_path, height = height, width = width)
    )
    return(plot)
}

.find_long_branch <- function(t_number, link_to){
    track_back <- function(t_number, link_to, track_list){
        for (each_track_n in seq_len(length(track_list))){
            each_track = track_list[[each_track_n]] %>% unlist()
            previous_t_number <-
                names(link_to_list[
                    which(vapply(link_to_list,
                           function(x) any(x == t_number[each_track[1]]),
                           logical(1))
                          )
                    ]
                )
            for (each_num in seq_len(length(previous_t_number))) {
                previous_link <- which(t_number == previous_t_number[each_num])
                newtrack = append(previous_link, each_track)
                if (each_num < 2){
                    track_list[[each_track_n]] <- newtrack
                } else {
                    track_list[[each_track_n + each_num - 1]] <- newtrack
                }
            }
        }
        if (length(previous_t_number) == 0) return(track_list)
        track_list <- track_back(t_number, link_to, track_list)
        return(track_list)
    }
    link_to_list <- str_split(link_to, ",") %>%
        lapply(function(x) str_remove_all(x, " "))
    names(link_to_list) <- t_number
    last_step <- list(length(t_number))
            track_list <- track_back(t_number, link_to, last_step)
    long <- vapply(track_list,
                   function(x) all(c(1, length(t_number)) %in% x),
                   logical(1)) %>%
        track_list[.] %>%
        vapply(length, FUN.VALUE = 1L) %>%
        which.max() %>% track_list[.] %>%
        unlist()
    return(long)
}

.make_plot <- function(df_wf, plot_style, is_main_branch=TRUE){
    if (is_main_branch){
        # graph start
        wf <- "subgraph { rank=same;\n"
        df_wf <- switch(plot_style,
            "detect"=df_wf[.find_long_branch(df_wf$t_number, df_wf$link_to), ],
            "none"  =df_wf[0,],
            "linear"={
                df_wf$link_to[seq_len(nrow(df_wf) - 1)] <-
                    df_wf$t_number[seq_len(nrow(df_wf))[-1]]; df_wf
            }
        )
    } else{
        wf <- switch(plot_style,
                     "detect" = "digraph { rankdir=LR;\n",
                     "none"   = "digraph { rankdir=TB;\n",
                     "linear" = "digraph { rankdir=LR;\n")
        df_wf <- switch(plot_style,
            "detect"=df_wf[-.find_long_branch(df_wf$t_number, df_wf$link_to), ],
            "none"  =df_wf,
            "linear"=df_wf[0,]
        )
    }
    if (nrow(df_wf) == 0) return(c(wf, "}"))

    steps <- df_wf$t_number
    step_text <- str_replace_all(df_wf$t_text, '[\'\"]', "\\\\'")
    link_to <- df_wf$link_to
    # reslove 1 to n links
    link_to <- str_split(link_to, ",") %>%
        lapply(function(x) str_remove_all(x, " "))
    # set up colors
    step_color <- ifelse(
        df_wf$no_run == 0,
        'gray',
        ifelse(
            is.na(df_wf$no_run) | is.na(df_wf$no_success),
            "black",
            ifelse(df_wf$no_run != df_wf$no_success, 'red', 'green')
        )
    )
    # dot language
    # add steps
    for (t in seq_along(steps)){
        if (!is.na(link_to[t]) & t < length(steps)){
            for (nlink in link_to[t]){
                wf <- append(
                    wf,
                    paste0('  n', str_replace_all(steps[t], "\\.", "_"), " -> ",
                           '  n', str_replace_all(nlink, "\\.", "_"), ";\n")
                )
            }
        }
    }
    # add color, text
    wf <- c(wf, paste0('  n', str_replace_all(steps, "\\.", "_"),
                       ' [label=\"',
                       steps, step_text, ' ',
                       ifelse(
                           df_wf$no_run == 0, '',
                           paste0(df_wf$no_success, '/', df_wf$no_run)
                       ),
                       '\"', 'fontcolor=', step_color,
                       ' color=white',
                       '];\n'))
    # end graph
    wf <- paste0(c(wf, "}"))
    return(wf)
}

.change_branch <- function(df_wf, wf){
    long <- .find_long_branch(df_wf$t_number, df_wf$link_to)
                    plot_start <- wf %>% str_which("digraph")
    sub_start <- wf %>% str_which("subgraph ")
    sub_steps_lines <- wf %>% str_which(" -> [^\\[]") %>% .[. > sub_start]
    sub_number <- wf[sub_steps_lines] %>%
        str_remove_all("[->;\n]") %>% str_remove("^.*[ ]+") %>%
                str_remove("n") %>% str_replace_all("_", "\\.") %>%
        lapply(function(x) df_wf$t_number[df_wf$t_number == x]) %>%
            unlist()
    move_line_num <- sub_steps_lines[!sub_number %in% df_wf$t_number[long]]
    if (length(move_line_num) == 0) return(wf)
    move_lines <- wf[move_line_num]
    wf <- wf %>% .[-move_line_num] %>% append(move_lines, after = plot_start)
    return(wf)
}
## Function used on this code:
# importFrom(magrittr,"%>%")
# importFrom("assertthat", "assert_that", "is.count")
# importFrom("stringr", "str_replace_all", "str_split",
#            "str_remove", "str_which", "str_remove_all")
# importFrom("DOT", "dot")
# importFrom(assertthat,"is.writeable")
# importFrom("rsvg,"rsvg_svg", "rsvg_png")

# #test code
# source("subsetRmd.R")
# df_wf = subsetRmd("YOUR.Rmd")
# df_wf$no_success[3:8] = 1
# df_wf$no_run[3:5] = 10
# df_wf$no_run[6:8] = 1
# df_wf$selected[1:35] = TRUE
# df_wf$link_to = NA
# df_wf$link_to[1:(nrow(df_wf) - 1)] = df_wf$t_number[2:nrow(df_wf)]
# df_wf$link_to[3] = NA
# df_wf$link_to[1] = "1.1, 2"
# df_wf$link_to[4] = "2.1, 3"
# df_wf$link_to[14] = NA
# df_wf = df_wf[1:17,]
# df_wf$link_to[8] = "3, 2.5"
# df_wf$selected = TRUE
# .plotWF(df_wf, plot_style = "linear")






