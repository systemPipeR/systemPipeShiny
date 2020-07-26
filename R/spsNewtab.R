

newTabData <- function(tab_id = "data1",
                   tab_displayname = "Tab Title",
                   type = "ez",
                   desc = "default",
                   common_validation = spsValidate({"pass"}),
                   prepro_methods = list(
                       makePrepro("nothing", "do nothing"),
                       makePrepro("md1", "method1",
                                  vd_expr = {nrow(df_filtered) > 1})
                   ),
                   app_path = getwd(),
                   out_folder_path = file.path(app_path, "R"),
                   eg_path = file.path(app_path, "data/iris.csv"),
                   author = "",
                   verbose = FALSE,
                   colorful = TRUE){
    verbose_old <- spsOption('verbose')
    spsOption('verbose', verbose)
    spsOption('use_crayon', colorful)
    spsinfo(glue("Asserting info for tab {tab_id}"))
    out_p <- file.path(out_folder_path, glue("tab_vs_df_{tab_id}.R"))
    .newtabAsserts(tab_id = tab_id, tab_displayname = tab_displayname,
                   desc = desc, author = author,
                   out_folder_path = out_folder_path, out_p = out_p,
                   type = type, app_path = app_path,
                   prepro_methods = prepro_methods)
    type <- match.arg(type, c("ez", "full"))
    spsinfo("Creates descrption")
    if(desc == "default") desc <- "
    #### Some Description of this data in markdown
    - you should ...
        1. eg 1.
        2. eg 2.
        - **Notice**: ...`this` ...


    ```
    some code demo ...
    ```
    "
    spsinfo("Parsing common validation")
    common_validation <- .expr2string(enquo(common_validation))
    spsinfo("Parsing preprocess methods")
    prepro <- .collectPrepro(prepro_methods)
    choices <-  prepro[['choices']]
    vds <- prepro[['vds']]
    pre <- prepro[['pres']]
    pt_options <- prepro[['pt_opts']]
    spsinfo("Ensure all template replacements are length 1 strings")
    c(tab_id, tab_displayname, desc, common_validation,
      choices, vds, pre, author, eg_path) %>%
        {names(.) <- c("tab Id", "display name", "description",
                       "common validation expressions",
                       "preprocess choices", "preprocess validation",
                       "preprocess method", "author", "example file")
        mapply(function(x, name){
            if(length(x) != 1){
                spserror(glue("Input {x} is not a length 1 string"))
            }
        }, x = ., name = names(.))
        }
    spsinfo("Start to inject to template...")
    crt_date <- Sys.time()
    tmp <- readLines("df_test.R") %>% glue_collapse(sep = '\n') %>%
        glue(.open = "#@", .close = "@#")
    spsinfo(glue("Write to file {out_p}"), TRUE)
    writeLines(tmp, out_p)
    spsinfo("Now register your new tab to config/tab.csv", TRUE)
    .tabRegister(tab_id, tab_displayname, app_path,
                 type = "vs", type_sub = "data", image = "",
                 displayed = 1)
    # reset verbose to whatever before the function runs
    spsOption('verbose', verbose_old)
    msg("New tab created!", "SPS-INFO", "green")
}


makePrepro <- function(method_id = "md1",
                       label = "New method1",
                       vd_expr = spsValidate(is.data.frame(df_filtered)),
                       pre_expr ={
                             return(df_filtered)
                             },
                       plot_options = "default",
                       use_string = FALSE){
    stopifnot(is.character(method_id))
    stopifnot(is.character(label))
    stopifnot(is.character(plot_options))
    spsinfo(glue("Creates preprocess method for {method_id}"))
    vd_expr_parsed <- .expr2string(enquo(vd_expr), use_string)
    pre_expr_parsed <- .expr2string(enquo(pre_expr), use_string)
    if(plot_options == 'default'){
        p_option <- ("type = 'plot'")
    } else if(length(plot_options) == 1){
        p_option <- glue("c({plot_options})")
    } else if(length(plot_options) > 1){
        p_option <- glue_collapse(plot_options, sep = ", ") %>%
            {glue("c({.})")}
    } else if(!emptyIsFalse(plot_options)) {
        spserror("plot_options can't be empty")
    }
    structure(
        list(id = method_id,
             label = label,
             vd = vd_expr_parsed,
             pre = pre_expr_parsed,
             pt_opts = p_option
        ),
        class = c("sps-prepro")
    )
}

removeTab <- function(app_path = getwd(), tab_id="core", multiple = FALSE){
    if(multiple) msg("You are allowing to remove more than 1 tabs at a time!",
                     "SPS-DANGER", "red")
    tab_file_path <- file.path(app_path, "config", "tabs.csv")
    if(!dir.exists(file.path(app_path, "R"))){
        spserror(glue("{file.path(app_path, 'R')} does not exist"))
    }
    if(!file.exists(tab_file_path)){
        spserror(glue("tabs.csv does not exist under config folder"))
    }
    tabs <- readLines(tab_file_path)
    header <- tabs[str_which(tabs, "^#")]
    tab_info <- suppressMessages(
        vroom::vroom(tab_file_path, comment = "#", na = character()))
    matched_rows <- str_which(tab_info[['tab_id']], tab_id)
    matched_ids <- dplyr::slice(tab_info, matched_rows) %>%
        dplyr::filter(type %in% c("wf", "core")) %>%
        dplyr::pull(tab_id)
    glue_collapse(matched_ids, sep = ", ") %>% {
        spsinfo(glue("Matched tab(s): {matched_ids}"), TRUE)
    }
    if(length(matched_rows) == 0){
        return(spswarn("No row matched"))
    } else if(length(matched_rows) > 1 & !multiple){
        glue_collapse(tab_info[['tab_id']][matched_rows], sep = ", ") %>%
            {spswarn(glue("matched: {.}"))}
        spserror("Remove multiple is FALSE, abort")
    }
    matched_ids %>% {
            if(length(.) > 0){
                glue_collapse(., sep = ", ") %>%
                {spserror(glue("Core and workflow tabs are not allowed to remove,
                             match: {.}"))}
            }
    }
    tabs_left <- dplyr::slice(tab_info, -matched_rows)
    c(header, names(tab_info) %>% glue_collapse(sep = ","),
       apply(tabs_left, 1, paste, collapse = ",")) %>%
        writeLines(tab_file_path)
    spsinfo(glue("{length(matched_rows)} tabs removed"), TRUE)

}

# collect all info from `makePrepro` lists
.collectPrepro <- function(prepro_methods){
    lapply(prepro_methods, function(md){
        assert_that(inherits(md, "sps-prepro"),
                    msg = "Input is not coming from `makePrepro` function")
    })
    prepro <- list(
        ids = lapply(prepro_methods, '[[', 'id') %>% unlist(),
        labels = lapply(prepro_methods, '[[', 'label') %>% unlist(),
        vds = lapply(prepro_methods, '[[', 'vd') %>% unlist(),
        pres = lapply(prepro_methods, '[[', 'pre') %>% unlist(),
        pt_opts = lapply(prepro_methods, '[[', 'pt_opts') %>% unlist()
    )
    if(any(duplicated(prepro[['ids']])))
        spserror(glue("Preprocess id must be unique, find duplicated: ",
                      "{prepro[['ids']][duplicated(prepro[['ids']])]}"))
    prepro[['choices']] <-
        glue("`{prepro[['labels']]}` = '{prepro[['ids']]}'") %>%
        glue_collapse(",\n") %>% {glue('c({.})')}
    prepro[['vds']] <- glue("'{prepro[['ids']]}' = {prepro[['vds']]}") %>%
        glue_collapse(",\n")
    prepro[['pres']] <- glue("'{prepro[['ids']]}' = {prepro[['pres']]}") %>%
        glue_collapse(",\n")
    prepro[['pt_opts']] <- glue("'{prepro[['ids']]}' = genGallery({prepro[['pt_opts']]})") %>%
        glue_collapse(",\n")
    prepro[['ids']] <- prepro[['labels']] <- NULL
    prepro
}

# deparse rlang quotes to strings
.expr2string <- function(quo_expr, use_string = FALSE){
    if(!use_string){
        quo_expr %>%
            expr_deparse() %>%
            remove_ANSI() %>%
            str_remove("^\\^") %>%
            glue_collapse(sep = "\n")
    } else{
        try(eval_tidy(quo_expr), silent = TRUE) %>% {
            if(!is.character(.))
                spserror(c("use_string is TRUE but input is not a string ",
                           "or an expression that returns a string"))}
        glue_collapse(eval_tidy(quo_expr), sep = "\n")
    }
}

# register a tab to config file
.tabRegister <- function(tab_id, display_name = "tab_title", app_path = ".",
                         type="vs", type_sub = "", image = "",
                         displayed = 1){
    tab_path <- file.path(app_path, "config", "tabs.csv")

    write_info <- glue_collapse(c(tab_id, display_name, type,
                                  type_sub, image, displayed), sep = ",")
    write(write_info, tab_path, append = TRUE)
}

# prechecks before create a new tab
.newtabAsserts <- function(tab_id, tab_displayname, desc, author,
                           out_folder_path, out_p, type,
                           app_path, prepro_methods){
    stopifnot(is.character(tab_id))
    stopifnot(is.character(author))
    stopifnot(is.character(tab_displayname))
    stopifnot(is.character(out_folder_path))
    if(file.exists(out_p))
        spserror(glue("File {out_p} exists."))
    if(str_detect(string = tab_displayname, ",")){
        spserror("comma ',' detected in display name, not allowed")
    }
    stopifnot(is.character(desc))
    stopifnot(is.character(type))
    stopifnot(is.list(prepro_methods))
    err <- try(findTabInfo(tab_id,
                           tab_file=file.path(app_path, "config/tabs.csv"),
                           force_reload = TRUE),
               silent = TRUE)
    if(inherits(err, "try-error")){
        if(str_detect(string = err[[1]], glue(".*SPS-ERROR.*{tab_id}.*"))){
            spsinfo("Tab id no conflict, continue.")
        } else {
            spserror(err[[1]])
        }
    } else {
        spserror(glue("Id '{tab_id}' exists, see your tabs.csv"))
    }

    assert_that(dir.exists(file.path(app_path, "R")),
                msg = glue('folder {file.path(app_path, "R")} is not there'))
}






