############################ SPS New Tab Functions ############################

#' Create a new SPS Tab
#' @description functions to create a new SPS tab. It is recommended to create
#' the data tab first then linked plot tabs.
#' @param tab_id character string, length 1, must start with "*plot_*" for plot
#' tabs and "*data_*" for data tabs. Must be a unique value.
#' @param tab_displayname character string, length 1, the name to be displayed
#' on sidebar tab list and tab
#' @param desc character string, length 1 in markdown format. Tab description
#' and instructions. You can make type it in multiple lines but in only one
#' string (one pair of quotes). e.g.
#' ```
#' "
#' # some desc
#' ## second line,
#' - bullet 1
#' - bullet 2
#' "
#' ```
#' @param img an example image of the kind of plot users can make out of this
#' tab. It can be a internet link or a local link which uses the *www* folder as
#' the root. e.g. drop your image *plot.png* inside *www/plot_list*, then the
#' link here is "plot_list/plot.png". Only needed for plot tabs.
#' @param plot_control_ui  additional UI components you want to
#' add to control plotting options, like additional slider bar, some on/off
#' switches, text input etc. If more than one components, put them in a
#' [shiny::tagList]
#' @param plot_expr the plot expression, like all other expression in other
#' shiny reactive expressions. e.g for more than one line use *{}*. default is
#' ```
#' plotly::ggplotly(
#'     ggplot(mydata$data,
#'            aes_string(names(mydata$data)[1],
#'                       names(mydata$data)[2])) +
#'         geom_point(aes(
#'             color = seq_len(nrow(mydata$data))
#'         ))
#' )
#' ```
#'
#' @param pkgs which packages you require users to install, list. specify
#' CRAN, bioconductor or github packages in a vector. see
#' [shinyCheckPkg]
#' ```
#' list(
#'     cran_pkg = c("base"),
#'     bioc_pkg = c(""),
#'     github = c("")
#' )
#' ```
#' @param plot_data a list of *makePlotData()* results, see [makePlotData] for
#' details.
#' ```
#' list(
#'     makePlotData("plot_1", ...),
#'     makePlotData("plot_2", ...),
#'     ...
#' )
#' ```
#' @param plot_out_func the plot output function to use on UI,
#' like [shiny::plotOutput], just the function name without quotes
#' and without *()*. default is [plotly::plotlyOutput].
#' @param plot_render_func The render plot function to use on server. No quotes,
#' no *()*. Must be **paired** with *plot_out_func*. e.g
#' If [plotly::plotlyOutput] is used on UI, server must use
#' [plotly::renderPlotly]. If you use [shiny::renderPlot],
#' plot will not show up.
#' @param app_path string, app directory, default is current directory
#' @param out_folder_path string, which directory to write the new tab file,
#' default is the *R* folder a SPS project
#' @param author character string, or a vector of strings. authors of the tab
#' @param empty bool, for **advanced developers**, if you don't want to
#' use SPS default tab UI and server structure, you can use turn this to *TRUE*.
#' A very simple template will be generated and you need to write all UI and
#' server parts by yourself. In this case, only `tab_id`, `display_name`,
#' `author` are needed, other
#' tab args can be ignored, system args are still working, like `verbose`,
#' `preview`, `style`, `colorful`
#' @param preview bool, *TRUE* will print the new tab code to console and will
#' not write the file and will not register the tab
#' @param use_string bool, sometimes parsing an expression in R may not be
#' totally accurate. To aviod this problem, turn this to *TRUE* and for
#' *plot_control_ui*, *plot_expr*, *p_out_func*, *p_render_func*, wrap your
#' expression in a quoted string. What you have provided in the string will be
#' what on the new tab file, no expression parsing will happen. Can only be
#' controlled as a group, which means use string for all of them or none of
#' them in plot tabs. For data tab, the affected argument is *common_validation*
#' . When turn this to *TRUE*, be careful with quotes in your expression,
#' escape or use alternative of single/double quotes.
#' ```
#' newTabPlot(
#'     ...
#'     use_string = TRUE,
#'     plot_control_ui = "
#'     tagList(clearableTextInput('id1', 'label')), h5('this title')
#'     ",
#'     plot_expr = "
#'     plotly::ggplotly(
#'         ggplot(mydata$data,
#'                aes_string(names(mydata$data)[1],
#'                           names(mydata$data)[2])) +
#'             geom_point(aes(
#'                 color = seq_len(nrow(mydata$data))
#'             ))
#'     )
#'     ",
#'     p_out_func = "plotly::plotlyOutput"
#'     ...
#' )
#' ```
#' @param reformat bool, whether to use [styler::style_file] reformat the code
#' @param open_file bool, if Rstudio is detected, open the new tab file?
#' @param verbose bool, default follows the project verbosity level.
#'  *TRUE* will give you more information on progress and debugging
#' @param colorful bool, whether the message will be colorful or not
#' @details
#' - Must use this function inside a SPS project, use *spsInit()* if
#' there is no project.
#' - For a new data tab, different preprocessing methods, their
#'  pre-prequirements and what plotting options available after each
#'  preprocess is controlled by the [makePrepro] function. Each call from
#'  this function specify one preprocessing method. All preprocess methods
#'  should be provided in a list to the *prepro_methods* argument.
#' - A new plot tab can have more than one data set as the input. For example a
#' plot can require a metadata table and a log transformed table as inputs.
#' There maybe multiple data tabs can preprocess and produce the same log table.
#' So you need to specify how many data inputs this plot requires; for each
#' input which data tab(s) this plot tab can receive data from; for each input
#' data type, what validations (data format checks) you want to do. All of these
#' are controlled by [makePlotData] and return(s) of this function should be
#' provided in a list to the *plot_data* argument.
#' is controlled by the [makePrepro] function. Each call from
#'  this function specify one preprocessing method. All preprocess methods
#'  should be provided in a list to the *prepro_methods* argument.
#' - One step in creating plot tab is specify incoming data source options by
#' the [makePlotData] method. It requires the data tab IDs exist in the
#' config file *config/tabs.csv*. So, it is best to create all required data
#' tabs first. Or specify it to any existing data tab like 'data_raw' and
#' when the template is created, manually change it.
#' @return a tab file in R folder and tab info registered on config/tabs.csv
#' @export
#' @importFrom rlang enquo
#' @examples
#' spsInit(change_wd = FALSE, overwrite = TRUE)
#' newTabData(
#'     tab_id = "data_new",
#'     tab_displayname = "my first data tab",
#'     prepro_methods = list(makePrepro(label = "do nothing",
#'                                      plot_options = "plot_new")),
#'     app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}")
#' )
#' newTabPlot(
#'     tab_id = "plot_new1",
#'     tab_displayname = "my first plot tab",
#'     plot_data = list(
#'         makePlotData(dataset_label = "Data from my new tab",
#'                      receive_datatab_ids = "data_new",
#'                      app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}"))
#'     ),
#'     app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}")
#' )
#' newTabData(
#'     tab_id = "data_empty",
#'     tab_displayname = "my first empty data tab",
#'     empty = TRUE,
#'     app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}")
#' )
#' newTabPlot(
#'     tab_id = "plot_empty",
#'     tab_displayname = "my first empty plot tab",
#'     empty = TRUE,
#'     app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}")
#' )
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
                       plot_data = list(makePlotData(app_path = app_path)),
                       plot_out_func = plotly::plotlyOutput,
                       plot_render_func = plotly::renderPlotly,
                       app_path = getwd(),
                       out_folder_path = file.path(app_path, "R"),
                       plot_control_ui = tagList(h3("Some plotting options")),
                       author = "",
                       empty = FALSE,
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
    plot_expr <- .expr2string(rlang::enquo(plot_expr), use_string)
    spsinfo("Parsing plot output and render function")
    p_out_func <- .expr2string(rlang::enquo(plot_out_func), use_string)
    p_render_func <- .expr2string(rlang::enquo(plot_render_func), use_string)
    spsinfo("Parsing plot UI control HTML")
    control_ui <- .expr2string(rlang::enquo(plot_control_ui), use_string)
    spsinfo("Parsing package requirements")
    pkgs <- .resolveTabPkg(pkgs)
    spsinfo("Parsing author(s)")
    author <- glue_collapse(author, sep = ", ")
    spsinfo("Ensure all template replacements are length 1 strings")
    list(tab_id, tab_displayname, desc,
      pg_id, pg_title, select_input,
      getdata, vd, plot_expr,
      p_out_func, p_render_func, author,
      pkgs, hreftab, control_ui) %>%
        {
            check_names <- c("tab Id", "display name", "description",
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
    tmp_file <- if(!empty)"plot_tab_template.R" else "plot_tab_template_empty.R"
    tmp <- readLines(
        system.file(file.path("app", "templates", tmp_file),
                    package = "systemPipeShiny")
    ) %>%
        glue_collapse(sep = '\n') %>%
        glue(.open = "#@", .close = "@#")
    if(preview) return(cat(tmp))
    spsinfo(glue("Write to file {out_p}"), TRUE)
    writeLines(tmp, out_p)
    # reformat
    .reformatTab(reformat, out_p)
    # register tab
    .registerTabWrapper(type_sub = "plot", img, tab_id, tab_displayname,
                        app_path, out_p, open_file)
    # reset verbose to whatever before the function runs
    spsOption('verbose', verbose_old)
    msg("New tab created!", "SPS-INFO", "green")
    spsOption('use_crayon', colorful_old)
    return(invisible())
}


#' @rdname newTabPlot
#' @param common_validation expression, use '{}' to wrap around multiple line
#' expressions. Usually a [spsValidate] object. You can use shiny's built in
#' [shiny::req] or [shiny::validate] for a simpler version.
#' @param prepro_methods a list of [makePrepro] method returns, read help for
#' that function for details
#' @param eg_path example data set path. Each data tab requires an example data
#' set to be displayed when users don't have anything to upload. Usually this
#' data file is a tabular file and stored in the *data* folder in a SPS
#' project
#' @export
#' @importFrom rlang enquo
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
                       empty = FALSE,
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
    common_validation <- .expr2string(rlang::enquo(common_validation), use_string)
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
    list(tab_id, tab_displayname, desc, common_validation,
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
    tmp_file <- if(!empty)"data_tab_template.R" else "data_tab_template_empty.R"
    tmp <- readLines(
        system.file(file.path("app", "templates", tmp_file),
                    package = "systemPipeShiny")
    ) %>%
        glue_collapse(sep = '\n') %>%
        glue(.open = "#@", .close = "@#")
    if(preview) return(cat(tmp))
    spsinfo(glue("Write to file {out_p}"), TRUE)
    writeLines(tmp, out_p)
    # reformat
    .reformatTab(reformat, out_p)
    # register tab
    .registerTabWrapper(type_sub = "data", img = "", tab_id, tab_displayname,
                        app_path, out_p, open_file)
    # reset verbose to whatever before the function runs
    # reset verbose to whatever before the function runs
    spsOption('verbose', verbose_old)
    msg("New tab created!", "SPS-INFO", "green")
    spsOption('use_crayon', colorful_old)
    return(invisible())
}


#' Create data receive methods for plot tabs
#' @description This function specify for each input data type in a plot tab,
#' where the data is coming from, how to validate incoming data. To use this
#' function, make sure there is a SPS project and *config/tabs.csv* exists.
#' @param dataset_id string, length 1, a unique ID within this plot
#'  tab.
#' @param dataset_label string, length 1, what label to display on UI for this
#' type of input data
#' @param receive_datatab_ids a vector of tab IDs: For this kind of data input,
#' which data tabs that can be used as input source(s). For example, if this
#' plot tab requires a dataframe and can be produced from "data_df1" or
#' "data_df2", *receive_datatab_ids = c("data_df1", "data_df2")*. These options
#' are later rendered as a drop down menu for users to choose for this where
#' they have prepared the required data from.
#' @param app_path SPS project folder
#' @param vd_expr what expression to validate(check) the incoming data set.
#' Usually it is a [spsValidate] object
#' @param use_string bool, if you don't want to parse *vd_expr*, use quoted
#' string for *vd_expr* amd turn this to TRUE. see [newTabPlot]
#'
#' @return a special list storing the incoming data info
#' @export
#' @details For the validation expression, the incoming data is stored in a
#' reactive values object, and you can access this data object
#' by *mydata$dataset_id*,
#' e.g. the dataset_id is "raw_data", then when the time you validate this
#' type of incoming data set, a variable *mydata$raw_data* is accessible.
#'
#' It is recommended to create data tabs first before running this function,
#' because *receive_datatab_ids* required data tab id exists in the *tabs.csv*
#' file.
#' @importFrom rlang enquo
#' @examples
#' spsInit(change_wd = FALSE, overwrite = TRUE)
#' newTabData("data_df1", "df 1",
#'            app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}"),
#'            open_file = FALSE)
#' newTabData("data_df2", "df 2",
#'            app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}"),
#'            open_file = FALSE)
#' plotdata_raw <- makePlotData("raw", "raw data",
#'              receive_datatab_ids = "data_df1",
#'              vd_expr = spsValidate({
#'                  if(!is.data.frame(mydata$raw))
#'                      stop("Input raw data need to be a dataframe")
#'              }, vd_name = "Validate raw data"),
#'              app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}"))
#' plotdata_meta <- makePlotData("meta", "meta data",
#'              receive_datatab_ids = c("data_df1", "data_df2"),
#'              vd_expr = spsValidate({
#'                  if(!is.data.frame(mydata$meta))
#'                      stop("Input raw data need to be a dataframe")
#'                  if(nrow(mydata$meta) < 1)
#'                      stop("Input raw data need to have at least one row")
#'              }, vd_name = "Validate meta data"),
#'              app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}"))
#' newTabPlot("plot_test1",
#'            app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}"),
#'            plot_data = list(plotdata_raw, plotdata_meta))
makePlotData <- function(dataset_id = "data",
                       dataset_label = "Raw data",
                       receive_datatab_ids = "data_template",
                       vd_expr = spsValidate({
                           if(is.data.frame(mydata$data)) TRUE
                           else stop("Data xx needs to be a dataframe or tibble")
                       }),
                       app_path = getwd(),
                       use_string = FALSE){
    stopifnot(is.character(dataset_id) & length(dataset_id) == 1)
    stopifnot(is.character(dataset_label) & length(dataset_label) == 1)
    stopifnot(is.character(receive_datatab_ids))
    spsinfo(glue("Creates plot data method for {dataset_id}"))
    rec_tab_labels <- lapply(
        receive_datatab_ids,
        function(x){
            label <- findTabInfo(
                x, force_reload = TRUE,
                tab_file =
                    file.path(app_path, "config", "tabs.csv"))[['tab_labels']]
            if(nchar(label) == 0){
                as.character(x)
            } else label
        }
    ) %>% unlist()
    vd_expr_parsed <- .expr2string(rlang::enquo(vd_expr), use_string)
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


#' Create data tab preprocess methods
#' @description Given the same uploaded data set, users can choose different
#' ways to preprocess the data and therefore different preprocessing methods
#' will lead to different plot tab options
#' @param method_id string, length 1, a unique ID within this data tab.
#' @param label string, length 1, what label to display on UI for users to
#' choose as a preprocess option
#' @param vd_expr expression, usually a [spsValidate] object. Before preprocess
#' if there is any additional validation that is special to this preprocess
#' method, you can specify here
#' @param pre_expr The actual preprocess expression. You should use
#' a pre-created variable called *data_filtered* to start and this is the
#' object that contains filtered data after users filtering on the UI. In the
#' end of this expression, you should return a preprocessed dataframe or
#' whatever object type that can be accepted by the desired plot tab. It is
#' recommended to write the preprocess method into a function and directly use
#' the function here, e.g.
#' ```
#' myPreprocess <- function(data){
#'     if(is.numeric(data[ ,1]))
#'         data[ ,1] <- data[ ,1] + 1
#'     return(data)
#' }
#' makePrepro(
#'  ...,
#'  pre_expr = myPreprocess(data_filtered),
#'  ...
#' )
#' ```
#' @param plot_options plot tab IDs: if data is preprocessed by this method,
#' what kind of plots can it make, specify plot tab IDs in a vector. Note:
#' unlike the *receive_datatab_ids* argument in [makePlotData] that requires
#' the *config/tabs.csv* exists, this argument doesn't require the config file
#' or the plot tab to be existing. One can use any ID(s) here. The ID checking
#' is postponed when the [genGallery] function runs on app start. "default"
#' means all possible plot tabs, the same as *type = 'plot'* in [genGallery].
#' @param use_string same as the same argument in [newTabPlot], controls
#' *vd_expr* and *pre_expr* in this function
#' @return a special list that contains all info for a preprocess method
#' @export
#' @details for *vd_expr*, *pre_expr* a variable called *data_filtered* is
#' accessible and is the object where data stored. One should use this
#' object to do validation or preprocess. See examples.
#' @importFrom rlang enquo
#' @examples
#' spsInit(change_wd = FALSE, overwrite = TRUE)
#' prepro_log <- makePrepro(
#'     "log", "take log of first column",
#'     vd_expr = spsValidate({
#'         if(!is.data.frame(data_filtered))
#'             stop("Input input data need to be a dataframe")
#'     }, vd_name = "log method pre-checks"),
#'     pre_expr = {
#'         if(is.numeric(data_filtered[ ,1]))
#'             {if(all(data_filtered[ ,1] > 0)){
#'                 data_filtered[ ,1] <- log(data_filtered[ ,1])}
#'             }
#'         data_filtered
#'     },
#'     plot_options = c("plot_xx1", "plot_xx2")
#'     )
#' ##### remember to save these helper functions in a R scripts under the R
#' ##### folder. They will be automatically sourced when app starts.
#' myPreprocess <- function(data){
#'     if(is.numeric(data[ ,1]))
#'         data[ ,1] <- data[ ,1] + 1
#'     return(data)
#' }
#' myVd <- function(data, vd_name){
#'     spsValidate({
#'         if(!is.data.frame(data))
#'             stop("Input input data need to be a dataframe")
#'     }, vd_name = vd_name)
#' }
#' #####
#' prepro_addone <- makePrepro(
#'     "addone", "add one to first column",
#'     vd_expr = myVd(data_filtered, "add one method pre-checks"),
#'     pre_expr = myPreprocess(data_filtered),
#'     plot_options = c("plot_xx1")
#' )
#' newTabData("data_test1",  "test 1",
#'            app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}"),
#'            prepro_methods = list(prepro_log, prepro_addone)
#'            )
makePrepro <- function(method_id = "md1",
                       label = "New method1",
                       vd_expr = spsValidate(is.data.frame(data_filtered)),
                       pre_expr ={data_filtered},
                       plot_options = "default",
                       use_string = FALSE){
    stopifnot(is.character(method_id))
    stopifnot(is.character(label))
    stopifnot(is.character(plot_options))
    spsinfo(glue("Creates preprocess method for {method_id}"))
    vd_expr_parsed <- .expr2string(rlang::enquo(vd_expr), use_string)
    pre_expr_parsed <- .expr2string(rlang::enquo(pre_expr), use_string)
    if(plot_options[1] == 'default'){
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

#' Remove a SPS tab
#' @description remove a tab R file and remove from the tabs.csv config file
#' @param tab_id tab ID, string, length 1, supports regular expressions, so
#' be careful. If more than one tabs are matched, stop by default
#' @param force bool, whether to ask for confirmation
#' @param app_path app directory
#' @param multiple bool, if matched more than one tab, trun this to *TRUE* can
#' remove more than one tab at a time. Be careful.
#' @param verbose bool, follows project setting, but can be overwrite.
#' *TRUE* will give you more information
#' @param colorful bool, whether the message will be colorful or not
#' @return remove the tab file and register info in *tabs.csv*
#' @export
#' @importFrom dplyr slice pull filter
#' @importFrom rlang enquo
#' @importFrom vroom vroom
#' @importFrom utils menu
#' @examples
#' spsInit(change_wd = FALSE, overwrite = TRUE)
#' newTabData(
#'     tab_id = "data_new_remove",
#'     tab_displayname = "my first data tab",
#'     prepro_methods = list(makePrepro(label = "do nothing",
#'                                      plot_options = "plot_new")),
#'     app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}")
#' )
#' removeSpsTab("data_new_remove", force = TRUE,
#'              app_path = glue("SPS_{format(Sys.time(), '%Y%m%d')}"))
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
        switch(utils::menu(c("YES", "NO"), title = "Continue?"),
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

## Internal

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
         pgPaneUpdate("pg", "vd_{ids}", 100)') %>%
        glue_collapse("\n")
     pt_data
}

#' @importFrom dplyr pull
#' @noRd
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
#' @importFrom rlang eval_tidy
.expr2string <- function(quo_expr, use_string = FALSE){
    if(!use_string){
        quo_expr %>%
            rlang::expr_deparse() %>%
            remove_ANSI() %>%
            str_remove("^\\^") %>%
            glue_collapse(sep = "\n")
    } else{
        try(rlang::eval_tidy(quo_expr), silent = TRUE) %>% {
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

#' @importFrom rstudioapi isAvailable navigateToFile
#' @noRd
.registerTabWrapper <- function(type_sub = "plot", img = "", tab_id,
                                tab_displayname, app_path, out_p,
                                open_file){
    spsinfo("Now register your new tab to config/tab.csv", TRUE)
    register_result <- shinyCatch(
        .tabRegister(tab_id, tab_displayname, app_path,
                     type = "vs", type_sub = type_sub, image = img,
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
}

# must be used within main tab function
#' @importFrom styler style_file tidyverse_style
#' @noRd
.reformatTab <- function(reformat, out_p){
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
    spsinfo("checking tab ID")
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
    spsinfo("checking output path")
    if(file.exists(out_p))
        spserror(glue("File {out_p} exists."))
    if(str_detect(string = tab_displayname, ",")){
        spserror("comma ',' detected in display name, not allowed")
    }
    stopifnot(is.character(desc))
    stopifnot(inherits(prepro_methods, "list"))
    stopifnot(inherits(plot_data, "list"))
    spsinfo("checking tab ID in tabs.csv")
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
    spsinfo("checking R folder")
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


