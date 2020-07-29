

#' Title
#'
#' @param tab_id
#' @param tab_displayname
#' @param desc
#' @param img
#' @param plot_expr
#' @param pkgs
#' @param plot_data
#' @param plot_out_func
#' @param plot_render_func
#' @param app_path
#' @param out_folder_path
#' @param author
#' @param preview
#' @param use_string
#' @param reformat
#' @param open_file
#' @param verbose
#' @param colorful
#'
#' @return
#' @export
#'
#' @examples
newTabPlot <- function(tab_id = "plot_id1",
                       tab_displayname = "Plot Tab Title",
                       desc = "default",
                       img = "",
                       plot_expr = plotly::ggplotly(
                           ggplot(mydata$data,
                                  aes_string(names(mydata$data)[1],
                                             names(mydata$data)[2])) +
                               geom_point(aes(
                                   color = seq_len(nrow(mydata$data))
                               ))
                       ),
                       pkgs = list(
                           cran_pkg = c("base"),
                           bioc_pkg = c(""),
                           github = c("")
                       ),
                       plot_data = list(makePlotData("data", "Raw data")),
                       plot_out_func = plotly::plotlyOutput,
                       plot_render_func = plotly::renderPlotly,
                       app_path = getwd(),
                       out_folder_path = file.path(app_path, "R"),
                       plot_control_ui = tagList(h3("Some plotting options")),
                       author = "",
                       preview = FALSE,
                       use_string = FALSE,
                       reformat = TRUE,
                       open_file = TRUE,
                       verbose = spsOption("verbose"),
                       colorful = spsOption("use_crayon")){
    verbose_old <- spsOption('verbose')
    colorful_old <- spsOption('use_crayon')
    spsOption('verbose', verbose)
    spsOption('use_crayon', colorful)
    spsinfo("Asserting tab ID")
    spsinfo(glue("Asserting info for tab {tab_id}"))
    out_p <- file.path(out_folder_path, glue("tab_vs_{tab_id}.R"))
    .newtabAsserts(tab_id = tab_id, tab_displayname = tab_displayname,
                   desc = desc, author = author,
                   out_folder_path = out_folder_path,
                   out_p = out_p, app_path = app_path,
                   plot_data = plot_data,
                   reformat = reformat, open_file = open_file,
                   type_sub = "plot", preview = preview, img = img)
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
    spsinfo("Parsing plot `data` methods")
    pt_data <- .collectPlotData(plot_data)
    pg_title <-  pt_data[['pg_title']]
    pg_id <-  pt_data[['pg_id']]
    hreftab <-  pt_data[['hreftab']]
    select_input <- pt_data[['select_input']]
    getdata <- pt_data[['getdata']]
    vd <- pt_data[['vd']]
    spsinfo("Parsing plot expression")
    plot_expr <- .expr2string(enquo(plot_expr), use_string)
    spsinfo("Parsing plot output and render function")
    p_out_func <- .expr2string(enquo(plot_out_func), use_string)
    p_render_func <- .expr2string(enquo(plot_render_func), use_string)
    spsinfo("Parsing plot UI control HTML")
    control_ui <- .expr2string(enquo(plot_control_ui), use_string)
    spsinfo("Parsing package requirements")
    pkgs <- .resolveTabPkg(pkgs)
    spsinfo("Parsing author(s)")
    author <- glue_collapse(author, sep = ", ")
    spsinfo("Ensure all template replacements are length 1 strings")
    c(tab_id, tab_displayname, desc,
      pg_id, pg_title, select_input,
      getdata, vd, plot_expr, p_out_func, p_render_func,
      author, pkgs, hreftab, control_ui) %>%
        {check_names <- c("tab Id", "display name", "description",
                          "progress data ID", "progress data title",
                          "data input selection", "get data expr",
                          "data validation expr", "plot expression",
                          "plot output func", "plot render func",
                          "author", "Package requirements",
                          "data href tab", "plot control UI")
        mapply(function(x, name){
            if(length(x) != 1){
                spserror(glue("Injection {name} is not a length 1 string:
                              {glue_collapse(x, '\n---\n')}"))
            }
        }, x = ., name = check_names)
        }
    spsinfo("Start to inject to template...")
    crt_date <- Sys.time()
    tmp <- readLines(
        system.file(file.path("app", "templates", "plot_tab_template.R"),
                    package = "systemPipeShiny")
    ) %>%
        glue_collapse(sep = '\n') %>%
        glue(.open = "#@", .close = "@#")
    if(preview) return(cat(tmp))
    spsinfo(glue("Write to file {out_p}"), TRUE)
    writeLines(tmp, out_p)
    if(reformat){
        spsinfo("reformat output R file")

        reformat_result <- shinyCatch(styler::style_file(
            out_p,
            transformers =
                styler::tidyverse_style(indent_by = 4,
                                        scope = "line_breaks")
        ), shiny = FALSE)
        if(!emptyIsFalse(reformat_result[['changed']])){
            spswarn("Can't reformat your R file, delete created R file")
            file.remove(out_p)
            spserror("Abort")
        }
    }
    spsinfo("Now register your new tab to config/tab.csv", TRUE)
    register_result <- shinyCatch(
        .tabRegister(tab_id, tab_displayname, app_path,
                     type = "vs", type_sub = "plot", image = "",
                     displayed = 1),
        shiny = FALSE
    )
    if(is.null(register_result)){
        spswarn("Can't register your tab, delete created R file")
        file.remove(out_p)
        spserror("Abort")
    }
    if(open_file){
        spsinfo("Now open the file for you")
        if(rstudioapi::isAvailable() & open_file)
            rstudioapi::navigateToFile(out_p)
    }
    # reset verbose to whatever before the function runs
    spsOption('verbose', verbose_old)
    msg("New tab created!", "SPS-INFO", "green")
    spsOption('use_crayon', colorful_old)
    return(invisible())
}


#' Title
#'
#' @param dataset_id
#' @param dataset_label
#' @param receive_datatab_ids
#' @param vd_expr
#' @param use_string
#'
#' @return
#' @export
#'
#' @examples
makePlotData <- function(dataset_id = "data",
                       dataset_label = "Raw data",
                       receive_datatab_ids = "data_template",
                       vd_expr = spsValidate({
                           if(is.data.frame(mydata$data)) TRUE
                           else stop("Data xx needs to be a dataframe or tibble")
                       }),
                       use_string = FALSE){
    stopifnot(is.character(dataset_id) & length(dataset_id) == 1)
    stopifnot(is.character(dataset_label) & length(dataset_label) == 1)
    stopifnot(is.character(receive_datatab_ids))
    spsinfo(glue("Creates plot data method for {dataset_id}"))
    rec_tab_labels <- lapply(
        receive_datatab_ids,
        function(x){
            label <- findTabInfo(x, force_reload = TRUE)[['tab_labels']]
            if(nchar(label) == 0){
                as.character(x)
            } else label
        }
    ) %>% unlist()
    vd_expr_parsed <- .expr2string(enquo(vd_expr), use_string)
    structure(
        list(id = dataset_id,
             dataset_label = dataset_label,
             receive_datatab_ids = receive_datatab_ids,
             rec_tab_labels = rec_tab_labels,
             vd = vd_expr_parsed
        ),
        class = c("sps-plotdata")
    )
}


.collectPlotData <- function(plotdata = list(makePlotData())){
    lapply(plotdata, function(md){
        assert_that(inherits(md, "sps-plotdata"),
                    msg = "Input is not coming from `makePlotData` function")
    })
    # parse items from plotdata list
    ids <- lapply(plotdata, '[[', 'id') %>% unlist()
    labels <- lapply(plotdata, '[[', 'dataset_label') %>% unlist()
    receive_ids <- lapply(plotdata, '[[', 'receive_datatab_ids')
    receive_labels <-  lapply(plotdata, '[[', 'rec_tab_labels')
    vds = lapply(plotdata, '[[', 'vd') %>% unlist()
    pt_data <- list()
    if(any(duplicated(ids)))
        spserror(glue("Plot data id must be unique, find duplicated: ",
                      "{ids[duplicated(ids)]}"))
    # progress titles
    pg_title_input <- glue("'Input from {labels}'") %>%
        glue_collapse(",\n") %>% {glue("{.},")}
    pg_title_vd <- glue("'Validate {labels}'") %>%
        glue_collapse(",\n")
    pt_data[['pg_title']] <- glue_collapse(c(pg_title_input, pg_title_vd), "\n")
    # progress ids
    pg_id_input <- glue("ns('{ids}')") %>%
        glue_collapse(",\n") %>% {glue("{.},")}
    pg_id_vd<- glue("ns('vd_{ids}')") %>%
        glue_collapse(",\n")
    pt_data[['pg_id']] <- glue_collapse(c(pg_id_input, pg_id_vd), "\n")
    # select input dropdown menu
    input_choices <- mapply(function(rec_id, rec_lab){
        glue('"{rec_lab}" = "{rec_id}"') %>%
            glue_collapse(",\n")
    }, rec_id = receive_ids, rec_lab = receive_labels) %>% unlist()
    pt_data[['select_input']] <-
        glue('column(6, shinyWidgets::pickerInput(ns("source_{ids}"), "{labels}",
                choices = c({input_choices}),
                options = list(style = "btn-primary")))') %>%
        glue_collapse(",\n")
    # href tabs
    href_ids <- lapply(receive_ids, function(x){
        glue_collapse(x, '", "') %>%
            {glue('"{.}"')}
    }) %>% unlist()
    pt_data[['hreftab']] <-
        glue('
        column(6, genHrefTab(
               c({href_ids}),
               title = "You need to prepare {labels} from these tabs:"
        ))'
        ) %>% glue_collapse(',\n')
    # server get data
    pt_data[['getdata']] <-
    glue('mydata${ids} <- getData(isolate(input$source_{ids}), shared)
          pgPaneUpdate("pg", "{ids}", 100)') %>%
        glue_collapse('\n')
    # validates
    pt_data[['vd']] <-
    glue('{vds}
         pgPaneUpdate("pg", "vd_{ids}", 100)')
     pt_data
}


#' Title
#'
#' @param tab_id
#' @param tab_displayname
#' @param desc
#' @param pkgs
#' @param common_validation
#' @param prepro_methods
#' @param app_path
#' @param out_folder_path
#' @param eg_path
#' @param author
#' @param reformat
#' @param open_file
#' @param verbose
#' @param colorful
#' @param preview
#' @param use_string
#'
#' @return
#' @export
#'
#' @examples
newTabData <- function(tab_id = "data_id1",
                   tab_displayname = "Data Tab Title",
                   desc = "default",
                   pkgs = list(
                       cran_pkg = c("base"),
                       bioc_pkg = c(""),
                       github = c("")
                   ),
                   common_validation = spsValidate({"pass"}, "common"),
                   prepro_methods = list(
                       makePrepro("nothing", "do nothing"),
                       makePrepro("md1", "method1",
                                  vd_expr = {nrow(data_filtered) > 1})
                   ),
                   app_path = getwd(),
                   out_folder_path = file.path(app_path, "R"),
                   eg_path = file.path(app_path, "data", "iris.csv"),
                   author = "",
                   preview = FALSE,
                   reformat = TRUE,
                   open_file = TRUE,
                   use_string = FALSE,
                   verbose = spsOption("verbose"),
                   colorful = spsOption("use_crayon")){
    verbose_old <- spsOption('verbose')
    colorful_old <- spsOption('use_crayon')
    spsOption('verbose', verbose)
    spsOption('use_crayon', colorful)
    spsinfo("Asserting tab ID")
    spsinfo(glue("Asserting info for tab {tab_id}"))
    out_p <- file.path(out_folder_path, glue("tab_vs_{tab_id}.R"))
    .newtabAsserts(tab_id = tab_id, tab_displayname = tab_displayname,
                   desc = desc, author = author,
                   out_folder_path = out_folder_path,
                   out_p = out_p, app_path = app_path,
                   prepro_methods = prepro_methods,
                   reformat = reformat, open_file = open_file,
                   type_sub = "data", preview = preview)
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
    common_validation <- .expr2string(enquo(common_validation), use_string)
    spsinfo("Parsing preprocess methods")
    prepro <- .collectPrepro(prepro_methods)
    choices <-  prepro[['choices']]
    vds <- prepro[['vds']]
    pre <- prepro[['pres']]
    pt_options <- prepro[['pt_opts']]
    spsinfo("Parsing package requirements")
    pkgs <- .resolveTabPkg(pkgs)
    spsinfo("Parsing author(s)")
    author <- glue_collapse(author, sep = ", ")
    spsinfo("Ensure all template replacements are length 1 strings")
    c(tab_id, tab_displayname, desc, common_validation,
      choices, vds, pre, author, eg_path, pkgs) %>%
        {check_names <- c("tab Id", "display name", "description",
                       "common validation expressions",
                       "preprocess choices", "preprocess validation",
                       "preprocess method", "author", "example file",
                       "Package requirements")
        mapply(function(x, name){
            if(length(x) != 1){
                spserror(glue("Injection {x} is not a length 1 string"))
            }
        }, x = ., name = check_names)
        }
    spsinfo("Start to inject to template...")
    crt_date <- Sys.time()
    tmp <- readLines(
        system.file(file.path("app", "templates", "data_tab_template.R"),
                    package = "systemPipeShiny")
    ) %>%
        glue_collapse(sep = '\n') %>%
        glue(.open = "#@", .close = "@#")
    if(preview) return(cat(tmp))
    spsinfo(glue("Write to file {out_p}"), TRUE)
    writeLines(tmp, out_p)
    if(reformat){
        spsinfo("reformat output R file")

        reformat_result <- shinyCatch(styler::style_file(
            out_p,
            transformers =
                styler::tidyverse_style(indent_by = 4,
                                        scope = "line_breaks")
        ), shiny = FALSE)
        if(!emptyIsFalse(reformat_result[['changed']])){
            spswarn("Can't reformat your R file, delete created R file")
            file.remove(out_p)
            spserror("Abort")
        }
    }
    spsinfo("Now register your new tab to config/tab.csv", TRUE)
    register_result <- shinyCatch(
        .tabRegister(tab_id, tab_displayname, app_path,
                 type = "vs", type_sub = "data", image = "",
                 displayed = 1),
        shiny = FALSE
    )
    if(is.null(register_result)){
        spswarn("Can't register your tab, delete created R file")
        file.remove(out_p)
        spserror("Abort")
    }
    if(open_file){
        spsinfo("Now open the file for you")
        if(rstudioapi::isAvailable() & open_file)
            rstudioapi::navigateToFile(out_p)
    }
    # reset verbose to whatever before the function runs
    spsOption('verbose', verbose_old)
    msg("New tab created!", "SPS-INFO", "green")
    spsOption('use_crayon', colorful_old)
    return(invisible())
}

#' Title
#'
#' @param method_id
#' @param label
#' @param vd_expr
#' @param pre_expr
#' @param plot_options
#' @param use_string
#'
#' @return
#' @export
#'
#' @examples
makePrepro <- function(method_id = "md1",
                       label = "New method1",
                       vd_expr = spsValidate(is.data.frame(data_filtered)),
                       pre_expr ={
                           cat("do nothing")
                           data_filtered
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
        p_option <- glue("c('{plot_options}')")
    } else if(length(plot_options) > 1){
        p_option <- glue_collapse(plot_options, sep = "', '") %>%
            {glue("c('{.}')")}
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

#' Title
#'
#' @param tab_id
#' @param force
#' @param app_path
#' @param multiple
#' @param verbose
#' @param colorful
#'
#' @return
#' @export
#'
#' @examples
removeSpsTab <- function(tab_id="none", force = FALSE,
                      app_path = getwd(), multiple = FALSE,
                      verbose = spsOption('verbose'),
                      colorful = spsOption('use_crayon')){
    assert_that(is.character(tab_id) & length(tab_id) == 1)
    assert_that(is.logical(multiple))
    assert_that(is.logical(verbose))
    assert_that(is.logical(force))
    spsinfo("checking tabs.csv and R folder", verbose)
    if(multiple) msg("You are allowing to remove more than 1 tabs at a time!",
                     "SPS-DANGER", "red")
    tab_file_path <- file.path(app_path, "config", "tabs.csv")
    if(!dir.exists(file.path(app_path, "R"))){
        spserror(glue("{file.path(app_path, 'R')} does not exist"))
    }
    if(!file.exists(tab_file_path)){
        spserror(glue("tabs.csv does not exist under config folder"))
    }
    spsinfo("Reading tabs.csv", verbose)
    tabs <- readLines(tab_file_path)
    header <- tabs[str_which(tabs, "^#")]
    tab_info <- suppressMessages(
        vroom::vroom(tab_file_path, comment = "#", na = character(),
                     altrep = FALSE))
    spsinfo("Check matching", verbose)
    matched_rows <- str_which(tab_info[['tab_id']], tab_id)
    matched_ids <- dplyr::slice(tab_info, matched_rows) %>%
        dplyr::pull(tab_id)
    if(length(matched_rows) == 0){
        return(spswarn("No row matched"))
    } else if(length(matched_rows) > 1 & !multiple){
        glue_collapse(tab_info[['tab_id']][matched_rows], sep = ", ") %>%
            {spswarn(glue("matched more than one: {.}"))}
        spserror("Remove multiple is FALSE, abort")
    }
    glue_collapse(matched_ids, sep = ", ") %>% {
        spsinfo(glue("Matched tab(s): {.}"), TRUE)
    }
    if(!force){
        switch(menu(c("YES", "NO"), title = "Continue?"),
               {},
               return(spsinfo("Abort", TRUE))
        )
    }
    dplyr::slice(tab_info, matched_rows) %>%
        dplyr::filter(type %in% c("core", "wf")) %>%
        dplyr::pull(tab_id) %>%
        {if(length(.) > 0){
                glue_collapse(., sep = ", ") %>%{
                spserror(glue("Core and workflow tabs are not allowed to remove,
                             match: {.}"))}
        }
        }
    spsinfo("Saving back to tabs.csv", verbose)
    tabs_left <- dplyr::slice(tab_info, -matched_rows)
    c(header, names(tab_info) %>% glue_collapse(sep = ","),
       apply(tabs_left, 1, paste, collapse = ",")) %>%
        writeLines(tab_file_path)
    spsinfo(glue("{length(matched_rows)} tabs removed from tabs.csv"), TRUE)
    spsinfo("Now remove tab R files", verbose)
    remove_files <- .makeFileNames(dplyr::slice(tab_info, matched_rows))
    remove_paths <- file.path(app_path, "R", remove_files)
    for(i in remove_paths){
        spsinfo(glue("Now remove file {i}"), TRUE)
        shinyCatch(file.remove(i), shiny = FALSE)
    }
    msg("Job complete", "SPS-INFO", "green")
    return(invisible())
}

.makeFileNames <- function(removing_df){
    mapply(function(id, subtype){
        if(subtype == "data"){
            glue("tab_vs_{id}.R")
        } else if(subtype == "plot"){
            glue("tab_vs_{id}.R")
        } else {
            spserror(glue("tab ID {id} subtype is '{subtype}' not ",
                          "data or plot, can't remove"))
        }
    },
    subtype = dplyr::pull(removing_df, type_sub),
    id = dplyr::pull(removing_df, tab_id),
    SIMPLIFY = TRUE)
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
            rlang::expr_deparse() %>%
            remove_ANSI() %>%
            str_remove("^\\^") %>%
            glue_collapse(sep = "\n")
    } else{
        try(eval_tidy(quo_expr), silent = TRUE) %>% {
            if(!is.character(.))
                spserror(c("use_string is TRUE but input is not a string ",
                           "or an expression that returns a string"))}
        glue_collapse(rlang::eval_tidy(quo_expr), sep = "\n")
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
    return(TRUE)
}

# prechecks before create a new tab
.newtabAsserts <- function(tab_id, tab_displayname, desc, author,
                           out_folder_path, out_p,
                           app_path, prepro_methods = list(),
                           plot_data = list(),
                           reformat, open_file,
                           type_sub, preview, img = ""){
    assert_that(is.character(tab_id) & length(tab_id) == 1)
    assert_that(is.character(img) & length(img) == 1)
    stopifnot(is.character(author))
    stopifnot(is.character(tab_displayname))
    stopifnot(is.character(out_folder_path))
    stopifnot(is.logical(reformat))
    stopifnot(is.logical(open_file))
    stopifnot(is.logical(preview))
    switch (type_sub,
        'data' = {
            if(!str_detect(tab_id, "^data_"))
                spserror("Tab ID must start with 'data_'")
        },
        'plot' = {
            if(!str_detect(tab_id, "^plot_"))
                spserror("Tab ID must start with 'plot_'")
        }
    )
    if(file.exists(out_p))
        spserror(glue("File {out_p} exists."))
    if(str_detect(string = tab_displayname, ",")){
        spserror("comma ',' detected in display name, not allowed")
    }
    stopifnot(is.character(desc))
    stopifnot(is.list(prepro_methods))
    stopifnot(is.list(plot_data))
    err <- try(findTabInfo(glue("{type_sub}_{tab_id}"),
                           tab_file = file.path(app_path, "config", "tabs.csv"),
                           force_reload = TRUE),
               silent = TRUE)
    if(inherits(err, "sps-tabinfo")){
        spserror(glue("Id '{tab_id}' exists, see your tabs.csv"))
    } else if(inherits(err, "try-error")){
        if(str_detect(
            err[[1]],
            glue(".*SPS-ERROR.*{glue('{type_sub}_{tab_id}')}"))){
            spsinfo("Tab id no conflict, continue.")
        } else spserror(err[[1]])

    }
    assert_that(dir.exists(file.path(app_path, "R")),
                msg = glue('folder {file.path(app_path, "R")} is not there'))
}


.resolveTabPkg <- function(pkgs){
    assert_that(is.list(pkgs))
    cran_pkg <- if(is.null(pkgs[['cran_pkg']])) "" else  pkgs[['cran_pkg']]
    bioc_pkg <- if(is.null(pkgs[['bioc_pkg']])) "" else  pkgs[['bioc_pkg']]
    github <- if(is.null(pkgs[['github']])) "" else  pkgs[['github']]
    assert_that(is.character(cran_pkg))
    assert_that(is.character(bioc_pkg))
    assert_that(is.character(github))
    lapply(github, function(x){
        if(str_detect(x, "/") | !emptyIsFalse(github)){
            if(str_split(x, "/", simplify = TRUE) %>% length >1 &
               !emptyIsFalse(github)){
                spserror(glue("github pkgs should has only one '/', find {x}"))
            }
        } else spserror(glue("github pkgs should be 'owner/repo', find {x}"))
    })
    cran_text <- glue_collapse(cran_pkg, sep = "', '") %>%
        {glue("'{.}'")} %>% {glue("cran_pkg = c({.}),")}
    bioc_text <- glue_collapse(bioc_pkg, sep = "', '") %>%
        {glue("'{.}'")} %>% {glue("bioc_pkg = c({.}),")}
    github_text <- glue_collapse(github, sep = "', '") %>%
        {glue("'{.}'")} %>% {glue("github = c({.})")}
    glue_collapse(c(cran_text, bioc_text, github_text), sep='\n')
}


