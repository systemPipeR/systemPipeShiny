######################## SPS R6 Classes #######################

#' SPS snapshots container
#'
#' @description  Initiate this container at the global level.
#' This container is used to communicate plotting tabs with the canvas tab
#' @importFrom R6 R6Class
#' @importFrom rlang eval_tidy parse_expr
#' @export
#' @examples
#' if(interactive()){
#'     library(shiny)
#'     library(shinydashboard)
#'     library(shinyjs)
#'     plots = plotContainer$new()
#'     mod1_UI <- function(id) {
#'         ns <- NS(id)
#'         tagList(
#'             h1("a tiny example of how Canvas work in SPS"),
#'             actionButton(ns("render"), "render"),
#'             jqui_resizable(plots$addUI(plotlyOutput(ns("plot1")), id)),
#'             sliderInput(ns("slide"), label = "rows",
#'                         min = 1, max = nrow(iris),
#'                         value = nrow(iris))
#'
#'         )
#'     }
#'     mod1 <- function(input, output, session, shared) {
#'         observeEvent(input$render, {
#'             output$plot1 <- plots$addServer(renderPlotly, 'mod1', {
#'                 ggplotly(ggplot(iris[1:input$slide, ],
#'                                 aes(Sepal.Length, Sepal.Width)) +
#'                              geom_point(aes(colour = Species)))
#'             })
#'             shared$snap_signal <- plots$notifySnap("mod1")
#'             req(shared$snap_signal)
#'             toastr_info(
#'                 glue("Snapshot {glue_collapse(shared$snap_signal, '-')}",
#'                      "added to canvas"),
#'                         position = "bottom-right")
#'         })
#'     }
#'     mod2_UI <- function(id){
#'         ns <- NS(id)
#'         tagList(
#'             actionButton(ns("refresh"), 'refresh'),
#'             sliderTextInput(
#'                 inputId = ns("ncols"),
#'                 label = "Number of columns per row to initiate canvas:",
#'                 choices = c(1:4, 12), selected = 2, grid = TRUE
#'             ),
#'             fluidRow(uiOutput(ns("new")), class = "sps-canvas")
#'         )
#'     }
#'
#'     mod2 <- function(input, output, session, shared) {
#'         ns <- session$ns
#'         make_plots <- reactiveValues(ui = list(), server = list())
#'         observeEvent(shared$snap_signal, {
#'             tab_id <- shared$snap_signal[1]
#'             new_plot_id <- glue("{tab_id}-{shared$snap_signal[2]}")
#'             print(new_plot_id)
#'             make_plots$ui[[new_plot_id]] <-
#'                 list(plots$getUI(tab_id, ns(new_plot_id)), ns(new_plot_id))
#'             make_plots$server[[new_plot_id]] <- plots$getServer(tab_id)
#'         })
#'         observeEvent(input$refresh, {
#'             ui <- make_plots$ui
#'             output$new <- renderUI({
#'                 sapply(seq_along(ui), function(i){
#'                     column(
#'                         width = 12/isolate(input$ncols),
#'                         class = "collapse in",
#'                         id = glue("{ui[[i]][2]}-container"),
#'                         div(class = "snap-drag bg-primary",
#'                             h4(glue("Plot {ui[[i]][2]}")),
#'                             tags$button(
#'                                 class = "btn action-button canvas-close",
#'                                 icon("times"),
#'                                 `data-toggle`="collapse",
#'                                 `data-target`=glue("#{ui[[i]][2]}-container"),
#'                                 `plot-toggle` = ui[[i]][2]
#'                             )
#'                         ),
#'                         jqui_resizable(make_plots$ui[[i]][[1]]),
#'                         tags$script(glue(.open = '@', .close = '@',
#'                                          '$("#@ui[[i]][2]@-container")',
#'                                          '.draggable({ handle: ".snap-drag"})'))
#'                     )
#'                 }, simplify = FALSE) %>%{
#'                     fluidRow(id = ns('plots'),
#'                              tags$head(tags$style(
#'                                  '
#'                              .snap-drag {
#'                              opacity: 0;
#'                              };'),
#'                                  tags$style(
#'                                      '
#'                              .snap-drag button{
#'                              position: absolute;
#'                              outline: none;
#'                              background-color: Transparent;
#'                              top: 2px;
#'                              right: 4px;
#'                              };'),
#'                                  tags$style(
#'                                      '
#'                              .snap-drag h4{
#'                              margin-bottom: 0;
#'                              padding-bottom: 10px;
#'                              };'),
#'                                  tags$style(
#'                                      '
#'                              .snap-drag button:focus {
#'                              outline: 0 !important;
#'                              box-shadow: none !important;
#'                              };'),
#'                                  tags$style('.snap-drag:hover {
#'                              opacity: 1;
#'                              }
#'                              '
#'                                  )),
#'                              tagList(.)
#'                     )
#'                 }
#'             })
#'             for(plot_id in names(make_plots$server)){
#'                 output[[plot_id]] <- make_plots$server[[plot_id]]
#'             }
#'         })
#'         observeEvent(input$hide_title, {
#'             shinyjs::toggleClass(selector = ".snap-drag", class = "collapse")
#'         })
#'
#'     }
#'     ui <- dashboardPagePlus(
#'         header = dashboardHeaderPlus(),
#'         sidebar = dashboardSidebar(),
#'         body = dashboardBody(
#'             useShinyjs(),
#'             useSps(),
#'             mod1_UI("mod1"),
#'             mod2_UI("mod2")),
#'         title = "Test",
#'     )
#'     server <- function(input, output, session) {
#'         shared = reactiveValues()
#'         callModule(mod1, "mod1",  shared)
#'         callModule(mod2, "mod2",  shared)
#'     }
#'     shinyApp(ui, server)
#' }
plotContainer <- R6::R6Class("plot_container",
    public = list(
        initialize = function(){
            spsinfo("Created a plot container R6 object", verbose = TRUE)
        },

        #' @field plot_ui a list of plot UI snapshots will be stored here.
        #' You shouldn't manually edit this list, use `add/getUI` method
        plot_ui = list(),

        #' @field plot_server a list of plot server snapshots will be stored
        #' here. You shouldn't manually edit this list, use `add/getServer`
        #' method
        plot_server = list(),

        #' @description
        #' add plot UI to the container. use it to wrap around the original plot
        #'  out put function
        #' @param plot_DOM Plotting output function, like `plotOutput`
        #' @param tab_id unique ID, usually use in a module and use the tab ID
        addUI = function(plot_DOM, tab_id){
            if(!inherits(plot_DOM, c("shiny.tag", "shiny.tag.list")))
                msg("plot UI is not shinytag or shiny.taglist", "error")
            DOM_id <- private$tagAttrib(plot_DOM, 'id')
            if (length(DOM_id) < 1)
                spswarn(
                    c("Can't find ID for this plot UI output function. If you ",
                      "are sure you have use the right function, then this ",
                      "plotting UI function is an exception.\n",
                      "Please open an issue on our Github page.",
                      "https://github.com/systemPipeR/systemPipeShiny"))
            if(not_empty(self$plot_ui[[tab_id]]))
                spsinfo(glue("Plot UI for this tab `{tab_id}` already exists ",
                         "in the container, overwrite"))
            self$plot_ui[[tab_id]] <- plot_DOM
            return(plot_DOM)
        },

        #' @description Get plot UI to the container.
        #' @param tab_id unique ID, usually the tab ID if used in a module
        #' @param plot_id_new string, usually if taking a
        #' snapshot of a plot DOM,
        #' they can't use the same ID on HTML. When get the UI, change the ID
        #' to a new value
        #' @return a saved plotting DOM
        getUI = function(tab_id, plot_id_new = NULL){
            ui <- self$plot_ui[[tab_id]]
            if(is.null(ui)) spserror(glue("Can't find `{id}` in plot_ui list"))
            if(!is.null(plot_id_new)){
                ui <- private$tagAttrib(ui, 'id', plot_id_new)
            }
            return(ui)
        },


        #' @description
        #' add plot server function to the container. use it to wrap around the
        #' original plot output server function, like `renderPlot`,
        #' `renderPlotly`
        #' @param render_func plot output server function
        #' @param tab_id unique ID, usually the tab ID if used in a module
        #' @param expr the reactive expression to render the plot,
        #' same as `expr`
        #' in other render function
        #' @param env default `parent.frame()`, see shiny documents
        #' @param quoted Is you `expr` quoted?
        #' @param ... additional args to pass to the original render function
        addServer = function(render_func, tab_id, expr,
                             env = parent.frame(), quoted = FALSE, ...){
            expr_func <- exprToFunction(expr, env, quoted)
            if(!inherits(render_func, "function"))
                stop("plot server is not a tab_id")
            if(not_empty(self$plot_server[[tab_id]])){
                msg(glue("Plot server with id {tab_id} already exists ",
                         "in the container, overwrite"))}
            self$plot_server[[tab_id]] <- list(func = render_func,
                                               args = list(
                                                   isolate(expr_func()), ...)
                                               )
            return(render_func(expr_func(), ...))
        },

        #' @description Get plot UI to the container.
        #' @param tab_id unique ID, usually the tab ID if used in a module
        #' @return saved plot server function
        getServer = function(tab_id){
            render_expr <- self$plot_server[[tab_id]]
            if(is.null(render_expr))
                msg(glue("Can't find `{tab_id}` in plot_server list"), "error")
            return(base::do.call(render_expr$func, render_expr$args))
        },

        #' @description notify the snapshot tab a snapshot has been added
        #'
        #' @param tab_id unique ID, usually the tab ID if used in a module
        #' @param skip positive integer, This function is usually bound to the
        #' plotting button. When clicked, the first number of `skip` clicks will
        #' not add the snapshot to the canvas tab.
        #' @param reset bool, to reset the count and return nothing
        #' @param set_to positive integer, reset the count of a tab to be a
        #' certain number
        #' @return a vector of `tab_id` and count number as a character string
        notifySnap = function(tab_id, skip = 1, reset = FALSE, set_to = NULL){
            if(reset) {
                private$notify_list[[tab_id]] <- NULL
                return(invisible())
            }
            if(is.number(set_to)){
                private$notify_list[[tab_id]] <- abs(round(set_to))
                return(invisible())
            }
            assert_that(is.number(skip))
            notify_index <- private$notify_list[[tab_id]]
            if(is.null(notify_index)) {
                private$notify_list[[tab_id]] <- 1}
            else {private$notify_list[[tab_id]] <- notify_index + 1}
            notify_index <- private$notify_list[[tab_id]]
            if (notify_index <= skip) return(NULL)
            else return(c(tab_id, notify_index - skip))
        }
    ),
    private = list(
        tagAttrib = function(taglist, attrib, changeto = NULL){
            is_tag <- FALSE
            attr_value <- NULL
            if(inherits(taglist, "shiny.tag"))
                {taglist <- tagList(taglist); is_tag <- TRUE}
            for (i in seq_along(taglist)){
                if (!inherits((taglist[[i]]), "shiny.tag")) next
                attr_value = taglist[[i]]$attribs[[attrib]]
                if(!is.null(attr_value)){
                    if (is.null(changeto)){return(attr_value)}
                    else {
                        taglist[[i]]$attribs[[attrib]] <- as.character(changeto)
                        if(is_tag) return(taglist[[1]])
                        else return(taglist)
                    }
                }
            }
            if(is.null(attr_value))
                msg(glue("Can't find attribute `{attrib}`"), "warning")
        },
        notify_list = list()
    )
)


# Imports for database handling classes
#' @importFrom R6 R6Class
#' @importFrom RSQLite dbDisconnect dbListTables dbWriteTable dbGetQuery
#' @importFrom RSQLite dbSendStatement dbGetRowsAffected
#' @importFrom RSQLite dbClearResult dbConnect SQLite
#' @importFrom dplyr tribble tbl collect pull
#' @importFrom openssl rsa_keygen encrypt_envelope decrypt_envelope
NULL

#' SPS database functions
#'
#' @description Initiate this container at global level.
#' Methods in this class can help admin to
#' manage general information of sps. For now it only stores some meta data and
#' the encryption key pairs. You can use this database to store
#' other useful things, like user pass hash, IP, browsing info ...
#'
#' A SQLite database by default is created inside `config` directory.
#' If not, you
#' can use `createDb` method to create one. On initiation, this class checks
#' if the default db is there and gives warnings if not.
#' @export
#' @examples
#' dir.create("config")
#' mydb <- spsDb$new()
#' mydb$createDb()
#' mydb$queryValue("sps_meta")
#' mydb$queryInsert("sps_meta", value = "'new1', '1'")
#' mydb$queryValue("sps_meta")
#' mydb$queryInsert("sps_meta", value = c("'new2'", "'2'"))
#' mydb$queryValue("sps_meta")
#' mydb$queryUpdate("sps_meta", value = '234',
#'                  col = "value", WHERE = "info = 'new1'")
#' mydb$queryValue("sps_meta")
#' \dontrun{
#'     library(dplyr)
#'     mydb$queryValueDp(
#'         "sps_meta",
#'         dp_expr="filter(., info %in% c('new1', 'new2') %>% select(2)")
#' }
#' mydb$queryDel("sps_meta", WHERE = "value = '234'")
spsDb <- R6::R6Class("spsDb",
    public = list(
        initialize = function(){
            spsinfo("Created SPS database method container", verbose = TRUE)
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            fail_msg <- c("Can't find default SPS db. ",
                          "Use `createDb` method",
                          " to create new or be careful ",
                          "to change default db_name ",
                          "when you use other methods")
            con <- private$dbConnect("config/sps.db")

            if(is.null(con)){
                spswarn(fail_msg)
            } else if(!all(c("sps_raw", "sps_meta") %in%
                           RSQLite::dbListTables(con))){
                spsinfo("Connected, but tables missing, seems like a new db.",
                        TRUE)
                spsinfo("Use CreateDb method to create tables", TRUE)
            } else {
                spsinfo("Default SPS-db found and is working")
            }
        },

        #' @description Create a snap hash database
        #'
        #' @param db_name database path, you need to
        #' manually create parent directory
        #' if not exists
        createDb = function(db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            if(!dir.exists(dirname(db_name)))
                dir.create(dirname(db_name), recursive = TRUE)
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                spserror("Can't create db. Make sure your wd is writeable")
            } else {
                spsinfo("Creating SPS db...", TRUE)
                sps_meta <- dplyr::tribble(
                    ~info, ~value,
                    "creation_date",
                    as.character(format(Sys.time(), "%Y%m%d%H%M%S")),
                )
                RSQLite::dbWriteTable(con, 'sps_meta',
                                      sps_meta, overwrite = TRUE)
                spsinfo("Db write meta")
                key <- private$genkey()
                key_encode <- key %>% serialize(NULL)
                sps_raw <- dplyr::tribble(
                    ~info, ~value,
                    "key", key_encode,
                )
                RSQLite::dbWriteTable(con, 'sps_raw', sps_raw, overwrite = TRUE)
                spsinfo("Key generated and stored in db")
                msg(c(glue("Db created at '{db_name}'. "),
                      "DO NOT share this file with others"),
                    "SPS-DANGER", "red")
                msg(glue("Key md5 {glue_collapse(key$pubkey$fingerprint)}"),
                    "SPS-INFO", "orange")
            }
        },

        #' @description Query database
        #'
        #' @param db_name  database path
        #' @param table  table name
        #' @param SELECT  SQL select grammar
        #' @param WHERE SQL select where
        #' @return query result, usually a dataframe
        queryValue = function(table, SELECT="*",
                              WHERE="1", db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                spserror("Can't find db.")
            } else {
                results <- RSQLite::dbGetQuery(con, glue("
                    SELECT {SELECT}
                    FROM {table}
                    WHERE {WHERE}
                "))
                spsinfo("Query sent")
                return(results)
            }
        },

        #' @description Query database with dplyr grammar
        #'
        #' Only supports simple selections, like comparison, %in%, `between()`,
        #' `is.na()`, etc. Advanced selections like wildcard,
        #' using outside dplyr functions like `str_detect`, `grepl`
        #' are not supported.
        #'
        #' @param db_name  database path
        #' @param table  table name
        #' @param dp_expr dplyr chained expression, must use '.' in first
        #' component of the chain expression
        #' @return query result, usually a tibble
        queryValueDp = function(table, dp_expr="select(., everything())",
                                 db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                msg("Can't find db.", "error")
            } else {
                results <- dplyr::tbl(con, table) %>%
                    {rlang::eval_tidy(rlang::parse_expr(dp_expr))} %>%
                    dplyr::collect()
                spsinfo("Query sent")
                return(results)
            }
        },

        #' @description update(modify) the value in db
        #'
        #' @param db_name  database path
        #' @param table  table name
        #' @param value  new value
        #' @param col  which column
        #' @param WHERE SQL where statement, conditions to select rows
        queryUpdate =  function(table, value, col,
                                WHERE="1", db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                spserror("Can't find db.")
            } else {
                res <- RSQLite::dbSendStatement(con, glue(
                    "UPDATE {table} SET {col}={value} WHERE {WHERE}"))
                if({aff_rows <- RSQLite::dbGetRowsAffected(res)} == 0){
                    spsinfo("No row updated", verbose = TRUE)
                } else {
                    spsinfo(glue("Updated {aff_rows} rows"), verbose = TRUE)
                }
                RSQLite::dbClearResult(res)
            }
        },

        #' @description delete value in db
        #'
        #' @param db_name  database path
        #' @param table  table name
        #' @param WHERE SQL where statement, conditions to select rows
        queryDel =  function(table, WHERE="1", db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                spserror("Can't find db.")
            } else {
                res <- RSQLite::dbSendStatement(con, glue(
                    "DELETE FROM {table} WHERE {WHERE}"))
                if({aff_rows <- RSQLite::dbGetRowsAffected(res)} == 0){
                    spsinfo("No row deleted", verbose = TRUE)
                } else {
                    spsinfo(glue("Deleted {aff_rows} rows"), verbose = TRUE)
                }
                RSQLite::dbClearResult(res)
            }
        },

        #' @description Insert value to db
        #'
        #' @param db_name  database path
        #' @param table  table name
        #' @param value  new values for the entire row
        #' @param WHERE SQL where statement, conditions to select rows
        queryInsert =  function(table, value, db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                msg("Can't find db.", "error")
            } else {
                value <- glue_collapse(value, sep = ", ")
                res <- RSQLite::dbSendStatement(con, glue(
                    "INSERT INTO {table} VALUES ({value})"))
                if({aff_rows <- RSQLite::dbGetRowsAffected(res)} == 0){
                    spswarn("No row inserted")
                } else {
                    spsinfo(glue("Inerted {aff_rows} rows"), verbose = TRUE)
                }
                RSQLite::dbClearResult(res)
            }
        }
    ),
    private = list(
        dbConnect = function(db_name){
            if(!is.character(db_name) | length(db_name) != 1)
                spserror("Invalid db name")
            con <- tryCatch(
                RSQLite::dbConnect(
                    RSQLite::SQLite(),
                    normalizePath(db_name, mustWork = FALSE)
                ),
                error = function(e){
                    spswarn(e)
                    return(NULL)
                }
            )
            spsinfo("Db connected")
            return(con)
        },
        genkey = function(){
            return(openssl::rsa_keygen())
        }
    )
)

#' SPS encryption functions
#'
#' @description Initiate this container at global level.
#' Methods in this class can help admin to encrypt files been output from sps.
#' For now it is only used to encypt and decrypt snapshots.
#' This class requires the SPS database. This class inherits all functions from
#' the db class, so there is no need to initiate the `spsDb` container.
#' @export
#' @examples
#' dir.create("config")
#' spsOption('verbose', TRUE)
#' my_ecpt <- spsEncryption$new()
#' my_ecpt$createDb()
#' my_ecpt$keyChange()
#' writeLines("test", "test.txt")
#' my_ecpt$encrypt("test.txt", "test.bin")
#' my_ecpt$decrypt("test.bin", "test_decpt.txt", TRUE)
#' readLines('test_decpt.txt')
spsEncryption <- R6::R6Class(
    "spsencrypt",
    inherit = spsDb,
    public = list(
        initialize = function(){
            spsinfo("Created SPS encryption method container", verbose = TRUE)
            spsinfo("This container inherits all functions from spsDb class")
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            fail_msg <- c("Can't create default SPS db. ",
                          "Default name 'config/sps.db'. Use `createDb` method",
                          " to create new or be careful ",
                          "to change default db_name ",
                          "when you use other methods")
            con <- private$dbConnect("config/sps.db")

            if(is.null(con)){
                spswarn(fail_msg)
            } else if(!all(c("sps_raw", "sps_meta") %in%
                           RSQLite::dbListTables(con))){
                spsinfo("Connected, but tables missing, seems like a new db.")
            } else {
                spsinfo("Default SPS-db found and is working", verbose = TRUE)
            }
        },

        #' @description Change encryption key of a SPS project
        #'
        #' @param db_name  database path
        keyChange = function(db_name="config/sps.db"){
            on.exit(if(!is.null(con)) RSQLite::dbDisconnect(con))
            con <- private$dbConnect(db_name)
            if(is.null(con)){
                spserror("Can't find db.")
            } else {
                old_value <- tryCatch(
                    RSQLite::dbGetQuery(con,
                        "SELECT `value`
                         FROM `sps_raw`
                         WHERE (`info` = 'key')") %>% dplyr::pull() ,
                    error = function(e){
                        spswarn(e)
                        return(NULL)
                    }
                )
                if(length(old_value) == 0) msg("Not a standard sps db", "error")
                key <- private$genkey()
                key_encode <- key %>% serialize(NULL)
                res <- RSQLite::dbSendStatement(con, glue(
                    "UPDATE sps_raw SET value=x'{glue_collapse(key_encode)}'",
                    "WHERE info='key'"))
                if(RSQLite::dbGetRowsAffected(res) != 0){
                    spsinfo("You new key has been generated and saved in db")
                    msg(glue("md5 {glue_collapse(key$pubkey$fingerprint)}"),
                        "SPS-INFO", "orange")
                } else {spserror("Update key failed")}
                RSQLite::dbClearResult(res)
            }
        },

        #' @description Get encryption key from db of a SPS project
        #'
        #' @param db_name  database path
        keyGet = function(db_name="config/sps.db"){
            key <- self$queryValue(table = 'sps_raw', SELECT="value",
                                   WHERE="info='key'", db_name) %>%
                dplyr::pull() %>% unlist() %>% unserialize()
                spsinfo(glue("OpenSSL key found md5",
                             "{glue_collapse(key$pubkey$fingerprint)}"))
            key
        },

        #' @description Encrypt raw data or a file with key from a SPS project
        #'
        #' @param data  raw vector or a file path
        #' @param db_name  database path
        #' @param out_path if provided, encrypted data will be write to a file
        #' @param overwrite if `out_path` file exists, overwrite?
        encrypt = function(data, out_path=NULL,
                           overwrite = FALSE, db_name="config/sps.db"){
            if (!is.null(out_path) & file.exists(out_path) & !overwrite) {
                spserror(glue("File {normalizePath(out_path)} exists"))
            }
            key <- self$keyGet()
            data_ecpt <- openssl::encrypt_envelope(data, key$pubkey)
            class(data_ecpt) <- c(class(data_ecpt), "sps_ecpt")
            spsinfo("Data encrypted")
            if(is.null(out_path)) return(data_ecpt)
            else saveRDS(data_ecpt, out_path)
            spsinfo(glue("File write to {normalizePath(out_path)}"))
            return(invisible(out_path))
        },

        #' @description Decrypt raw data or a file with key from a SPS project
        #'
        #' @param data  raw vector or a file path
        #' @param out_path if provided, encrypted data will be write to a file
        #' @param overwrite if `out_path` file exists, overwrite?
        #' @param db_name  database path
        decrypt = function(data, out_path=NULL,
                           overwrite = FALSE, db_name="config/sps.db"){
            if (!is.null(out_path) & file.exists(out_path) & !overwrite) {
                spserror(glue("File {normalizePath(out_path)} exists"))
            }

            data <- tryCatch({
                if (is.raw(data)) {data}
                else if (all(is.character(data) & length(data) == 1)) {
                    content <- readRDS(normalizePath(data, mustWork = TRUE))
                    spsinfo("File read into memory")
                    content
                }
                else {spserror("data must be raw or a valid file")}
                }, error = function(e){
                    spswarn(e)
                    spserror("Not a standard SPS encrypted object")
                })
            if(!inherits(data, "sps_ecpt"))
                spserror("Not a standard SPS encrypted object")
            key <- self$keyGet()
            data_dcpt <- tryCatch({
                suppressWarnings(
                    openssl::decrypt_envelope(
                        data$data,
                        iv = data$iv,
                        session =data$session,
                        key = key))
                },  error = function(e){
                    spswarn(e)
                    spserror("Cannot decrypt this file")
                })
            spsinfo("Data decrypted")
            if(is.null(out_path)) return(data_dcpt)
            else {
                writeBin(object = data_dcpt, con = out_path)
            }
            spsinfo(glue("File write to {normalizePath(out_path)}"))
            return(invisible(out_path))
        }
    )
)


