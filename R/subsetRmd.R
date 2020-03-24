library(magrittr)
library(stringr)
library(assertthat)

subsetRmd <- function(p, input_steps=NULL, exclude_steps=NULL, p_out=NULL, save_rmd=TRUE){
    # function start, check inputs
    assert_that(file.exists(p))
    if (not_empty(input_steps)) assert_that(is.string(input_steps))
    if (not_empty(p_out)) assert_that(file.exists(dirname(p_out)))
    if (not_empty(exclude_steps)) assert_that(is.string(exclude_steps))
    # default out behavior, in ISO 8601 time format
    if (is.null(p_out)) p_out <- paste0('new', format(Sys.time(), "%Y%m%d_%H%M%S"), basename(p))
    # read file
    file <- readLines(p)
    # check for proper start and end
    t_start <- file %>% str_which("^#")
    if (length(t_start) == 0) stop("This Rmd does not have any '#' titles")
    # get code chunks
    chunk_start <- file %>% str_which("^```\\{.*\\}.*")
    chunk_end <- file %>% str_which("^```[[:blank:]]{0,}$")
    if (length(chunk_start) != length(chunk_end)) stop("unmatched number of code chunk starts and ends")
    for (i in seq_along(chunk_start)[-length(chunk_end)]){
        if (chunk_start[i+1] <= chunk_end[i]) stop(paste("A code chunk does not end: chunk line", chunk_start[i+1]))
    }
    # remove '#' titles in code chunk
    t_start <- t_start[!unlist(lapply(t_start, function(x) any(x >= chunk_start & x <= chunk_end)))]
    # get end
    t_end <- append((t_start - 1)[c(-1)], length(file))
    # get # levels and text
    t_text <- file[t_start] %>% str_remove("^[#]+")
    t_lvl <- file[t_start] %>% str_extract("^[#]+") %>% nchar()
    # parse levels
    for (lvl in unique(t_lvl)[-length(t_lvl)]){
        if (lvl == min(unique(t_lvl))){
            step_main <- which(t_lvl == lvl)
            names(t_lvl)[step_main] <- names(step_main) <- seq_along(step_main)
            step_main <- append(step_main, 9999)
        }
        sub_lvl <- lvl
        while (sub_lvl <= max(t_lvl)) {
            step_sub <- which(t_lvl == sub_lvl + 1)
            if (length(step_sub) < 1) {
                sub_lvl <- sub_lvl + 1
            } else {
                break()
            }
        }
        jump_step_glue <- if (sub_lvl - lvl == 0) "." else rep(".1.", sub_lvl - lvl) %>%
            paste0(collapse = "") %>%
            str_replace_all("\\.\\.", "\\.")
        for (i in seq_along(step_main[-1])) {
            subs <- step_sub[step_sub > step_main[i] & step_sub < step_main[i + 1]]
            names(t_lvl)[subs] <- names(step_sub)[step_sub %in% subs] <- paste0(names(step_main[i]), jump_step_glue, seq_along(subs))
        }
        step_main <- append(step_sub, 9999)
    }
    # get code in lists
    code_list <- lapply(seq_along(t_start), function(t_index) {
        code_start <- chunk_start[chunk_start %in% (t_start[t_index]: t_end[t_index])]
        code_end <- chunk_end[chunk_end %in% (t_start[t_index]: t_end[t_index])]
        code_lines <- lapply(seq_along(code_start), function(code_index) {
            (code_start[code_index]+1):(code_end[code_index]-1)
        }) %>% unlist()
        file[code_lines]
    })
    # create a df to store everything
    rmd_df <- data.frame(t_lvl = t_lvl, t_number = names(t_lvl),
                         t_text = t_text, selected = FALSE,
                         row.names = NULL, stringsAsFactors = FALSE)
    rmd_df$code <- code_list
    # add sample run/success, step link cols
    rmd_df$no_run <- NA
    rmd_df$no_run <- ifelse(sapply(rmd_df$code, length) == 0, 0, NA)
    rmd_df$no_success <- NA
    rmd_df$no_success <- ifelse(sapply(rmd_df$code, length) == 0, 0, NA)
    rmd_df$link_to <- NA
    rmd_df$link_to[1:(nrow(rmd_df) - 1)] <- rmd_df$t_number[2:nrow(rmd_df)]
    # list all steps if no input_steps
    if (!not_empty(input_steps)) {
        cat("No input_steps is given, list all sections and exit\n")
        cat("This file contains following sections\n")
        str_replace(t_text, "^", paste0(strrep("    ", (t_lvl - 1)), names(t_lvl), " ")) %>% 
            paste0(., collapse = '\n') %>% str_replace("$", "\n") %>% cat() 
        return(rmd_df)
    }
    # parse steps
    index_select <- .parse_step(t_lvl, input_steps)
    index_exclude <- .parse_step(t_lvl, exclude_steps)
    index_final <- index_select[!index_select %in% index_exclude]
    rmd_df$selected[index_final] <- TRUE
    # print again what will be write in the new file
    cat("The following sections are selected\n")
    str_replace(t_text[index_final],"^", 
                paste0(strrep("    ", (t_lvl[index_final] - 1)),
                            names(t_lvl[index_final]), " ")) %>% 
        paste0(., collapse = '\n') %>% str_replace("$", "\n") %>% cat()
    # to print new titles and return
    if (save_rmd == FALSE) return(rmd_df)
    # sebset lines
    t_start[index_final]
    t_end[index_final]
    final_lines <- mapply(seq, t_start[index_final], t_end[index_final]) %>%
        unlist() %>% 
        append(1:(t_start[1] - 1), .) %>% 
        unique()
    writeLines(file[final_lines], p_out)
    cat(paste("File write to", normalizePath(p_out), '\n'))
    return(rmd_df)
}

# internal parse function
.parse_step <- function(t_lvl, input_steps){
    t_lvl_name <- names(t_lvl)
    input_steps <- unlist(input_steps %>% str_remove_all(" ") %>% str_split(",") %>% list())
    # single steps
    nocolon_steps <- input_steps[str_which(input_steps, "^[^:]+$")]
    lapply(nocolon_steps, function(x) if (!any(t_lvl_name %in% x)) stop(paste('Step', x, 'is not found')))
    # dash linked steps
    dash_list <- NULL
    for (i in str_which(input_steps, ":")){
        dash_step <- unlist(str_split(input_steps[i], ":"))
        dash_parse <- unlist(lapply(dash_step, function(x) {
            which(t_lvl_name %in% x) %>% ifelse(length(.) > 0, ., stop(paste('Step', x, 'is not found')))
        })) %>% {
            t_lvl_name[.[1]: .[2]] 
        }
        dash_list <- append(dash_list, dash_parse)
    }
    # merge
    all_step_name <- unique(append(nocolon_steps, dash_list))
    # if upper level step is selected, all sub-level steps will be added
    unlist(lapply(all_step_name, function(x) str_which(t_lvl_name, paste0('^', x, '\\..*')))) %>% 
        append(which(t_lvl_name %in% all_step_name)) %>% 
        unique() %>% sort() %>% return()
}

######## INSTRUCTIONS #########
# subsetRmd(p, p_out=NULL, input_steps=NULL, exclude_steps=NULL)

# p: string, path to the Rmd file
# p_out: string, path to the out Rmd file
# input_steps: string, only one string of all steps you want to subset, ':' to jump steps, '.' for substeps, ',' to separate selections
# exclude_steps: string, only one string of all steps you want to exclude from input_steps
# save_rmd: bool, default TRUE, if FALSE, list new selected tiles and exit
## return: a dataframe of title levels, title numbers, title text, whether it is selected, and R code under this title

# if no input_steps, only list steps in a Rmd and return the dataframe but all titles are unselected (FALSE). 
# input_steps and exclude_steps must be ONE character string. 
# Jump from major step to sub-step is supported, but 
# if a major step is selected/excluded, all sub-steps of this major step will be 
# selected/excluded. Repeatedly selected steps will only result a unique step. 
# It is recommended to put major steps in `input_steps`, like '1:4, 6:8, 10'; 
# unwanted sub-steps in `exclude_steps`, like '1.1, 3.1.1-3.1.3, 6.5'. 
# Reverse selecting is supported e.g. '10:1'. 

##### TRY THE FOLLOWING CODE
# p = "../systemPipeR_testing/cwl_testing/systemPipeRNAseq.Rmd"
# input_steps = "1:2.1, 3.1:4.1.1, 4:6"
# exclude_steps  = '3.1, 4.1'
# p_out = 'test_out.Rmd'
# test = subsetRmd(p)
# dplyr::tibble(test)
# test = subsetRmd(p, input_steps, exclude_steps, save_rmd = FALSE)
# dplyr::tibble(test)
# test = subsetRmd(p, input_steps, exclude_steps, p_out)


# Run from cml
# Rscript subsetRmd.R <p>
# Rscript subsetRmd.R <p> <p_out> <input_steps> <exclude_steps>
# e.g Rscript subsetRmd.R ../cwl_testing/systemPipeR.Rmd test_out.Rmd "1-5" "4"
# in_args = commandArgs(TRUE)[1:4]
# input_steps = in_args[3]
# exclude_steps = in_args[4]
# if (is.na(input_steps)) input_steps = NULL
# if (is.na(exclude_steps)) exclude_steps = NULL
# subsetRmd(in_args[1], in_args[2], input_steps, exclude_steps)




