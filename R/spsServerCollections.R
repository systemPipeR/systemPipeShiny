################## A Collections of server utilities############################
# Can be used in other shiny projects, no need to use under SPS framework
## use on top of shiny


#' Shiny exception handling
#' @description Exception in Shiny apps can crash the app. Most time we don't
#' want the app to crash but just stop this block, inform users and continue
#' with other code blocks. This function is designed to handle these issues.
#' @details  Show error, warning, message by a toast bar on client end and
#' also log the text on backend console. It will return original value if not
#' blocking at "warning" "message" level, and return `NULL` at error level.
#' If blocks at `error`, function will be stopped and other code in the same
#' reactive context will be blocked. If blocks at `warning` level, warning and
#' error will be blocked; `message` level blocks all 3 levels. The blocking works
#' similar to shiny's [shiny::req()] and [shiny::validate()].
#' If anything inside fails, it will
#' block the rest of the code in your reactive expression domain.
#'
#' Messages will be displayed for 3s, 5s for warnings and errors will never
#' go away on UI unless users' mouse hover on the bar or manually close it.
#' @param expr expression
#' @param position client side message bar position, one of:
#' c("top-right", "top-center", "top-left","top-full-width", "bottom-right",
#' "bottom-center", "bottom-left","bottom-full-width").
#' @param blocking_level  what level you want to block the execution, one
#' of "error", "warning", "message"
#' @param shiny bool, It is also possible to use without a shiny session. Only
#' shows on console log, works very similar as [tryCatch()] and can block at
#' multiple levels
#' @return see description
#' @importFrom shinytoastr toastr_info toastr_warning toastr_error
#' @export
#'
#' @examples
#' if(interactive()){
#'     ui <- fluidPage(
#'         useSps(),
#'         actionButton("btn1","error and blocking"),
#'         actionButton("btn2","error no blocking"),
#'         actionButton("btn3","warning but still returns value"),
#'         actionButton("btn4","warning but blocking returns"),
#'         actionButton("btn5","message"),
#'
#'         textOutput("text")
#'     )
#'     server <- function(input, output, session) {
#'         fn_warning <- function() {
#'             warning("this is a warning!")
#'             return(1)
#'         }
#'         observeEvent(input$btn1, {
#'             shinyCatch(stop("error with blocking"), blocking_level = "error")
#'             print("You shouldn't see me")
#'         })
#'         observeEvent(input$btn2, {
#'             shinyCatch(stop("error without blocking"))
#'             print("I am not blocked by error")
#'         })
#'         observeEvent(input$btn3, {
#'             return_value <- shinyCatch(fn_warning())
#'             print(return_value)
#'         })
#'         observeEvent(input$btn4, {
#'             return_value <- shinyCatch(fn_warning(), blocking_level = "warning")
#'             print(return_value)
#'             print("other things")
#'         })
#'         observeEvent(input$btn5, {
#'             shinyCatch(message("message"))
#'         })
#'     }
#'     shinyApp(ui, server)
#' }
#' #outside shiny examples
#' shinyCatch(message("this message"), shiny = FALSE)
#' try({shinyCatch(stop("this error"), shiny = FALSE); "no block"}, silent = TRUE)
#' try({shinyCatch(stop("this error"), shiny = FALSE, blocking_level = "error")
#'     "blocked"}, silent = TRUE)
shinyCatch <- function(expr, position = "bottom-right",
                       blocking_level = "none", shiny = TRUE) {
    assert_that(is.logical(shiny))
    toastr_actions <- list(
        message = function(m) {
            msg(m$message, "SPS-INFO", "blue")
            if(shiny) shinytoastr::toastr_info(message = remove_ANSI(m$message),
                        position = position, closeButton = TRUE,
                        timeOut = 3000, preventDuplicates = TRUE)
        },
        warning = function(m) {
            msg(m$message, "SPS-WARNING", "orange")
            if(shiny) shinytoastr::toastr_warning(
                message = remove_ANSI(m$message),
                position = position, closeButton = TRUE,
                timeOut = 5000, preventDuplicates = TRUE)
        },
        error = function(m) {
            msg(m$message, "SPS-ERROR", "red")
            if(shiny) shinytoastr::toastr_error(
                    message = remove_ANSI(m$message), position = position,
                    closeButton = TRUE, timeOut = 0, preventDuplicates = TRUE,
                    title = "There is an error", hideDuration = 300)
        }
    )
    switch(tolower(blocking_level),
           "error" = tryCatch(
               suppressMessages(suppressWarnings(withCallingHandlers(
                   expr,
                   message = function(m) toastr_actions$message(m),
                   warning = function(m) toastr_actions$warning(m)
               ))),
               error = function(m) {
                   toastr_actions$error(m)
                   reactiveStop(class = "validation")
               }),
           "warning" = tryCatch(
               suppressMessages(withCallingHandlers(
                   expr,
                   message = function(m) toastr_actions$message(m)
               )),
               warning = function(m) {
                   toastr_actions$warning(m)
                   reactiveStop(class = "validation")
                   },
               error = function(m) {
                   if(!is.empty(m$message)) toastr_actions$error(m)
                   reactiveStop(class = "validation")
               }),
           "message" = tryCatch(
               expr,
               message = function(m) {
                   message = toastr_actions$message(m)
                   reactiveStop(class = "validation")
                   },
               warning = function(m) {
                   toastr_actions$warning(m)
                   reactiveStop(class = "validation")
                   },
               error = function(m) {
                   if(!is.empty(m$message)) toastr_actions$error(m)
                   reactiveStop(class = "validation")
               }),
           tryCatch(
               suppressMessages(suppressWarnings(withCallingHandlers(
                   expr,
                   message = function(m) toastr_actions$message(m),
                   warning = function(m) toastr_actions$warning(m)
               ))),
               error = function(m) {
                   toastr_actions$error(m)
                   return(NULL)
               }
           )
    )
}


#' Shiny package checker
#' @description  check package name space for some required packages and pop up
#' warnings in shiny telling users how to install the missing packages.
#'
#' This is useful when some of packages are required by a shiny app. Before
#' running into that part of code, using this function to check the required
#' pakcage and pop up warnings will prevent app to crash.
#' @param session shiny session
#' @param cran_pkg vector of package names
#' @param bioc_pkg vector of package names
#' @param github vector of github packages, github package must use the format of
#'  "github user name/ repository name", eg. c("user1/pkg1", "user2/pkg2")
#' @param quietly bool, should warning messages be suppressed?
#' @importFrom shinyAce is.empty
#' @importFrom shinytoastr toastr_success
#' @importFrom shinyWidgets sendSweetAlert
#' @return TRUE if pass, sweet alert massage and FALSE if fail
#' @export
#'
#' @examples
#' if(interactive()){
#'     shinyApp(ui = shinyUI(
#'         fluidPage(actionButton("haha", "haha"))
#'     ), server = function(input, output, session){
#'         observeEvent(input$haha,
#'             shinyCheckPkg(session, cran_pkg = c("pkg1", "pkg2"),
#'                           bioc_pkg = "haha", github = "sdasdd/asdsad"))
#'     })
#' }
shinyCheckPkg <- function(session, cran_pkg = NULL, bioc_pkg = NULL, github = NULL, quietly = FALSE) {
    missing_cran <- checkNameSpace(cran_pkg, quietly, from = "CRAN")
    missing_bioc <- checkNameSpace(bioc_pkg, quietly, from = "BioC")
    github_pkg <- github %>% str_remove("^.*/")
    missing_github_pkg <- checkNameSpace(github_pkg, quietly, from = "GitHub")
    missing_github <- github[github_pkg %in% missing_github_pkg]
    cran_cmd <- if (shinyAce::is.empty(missing_cran)) "" else
        paste0("install.packages(c('", paste0(missing_cran, collapse = "', '"), "'))")
    bioc_cmd <- if (shinyAce::is.empty(missing_bioc)) "" else
        paste0(
            'if (!requireNamespace("BiocManager", quietly=TRUE))
        install.packages("BiocManager")\n',
            "BiocManager::install(c('", paste0(missing_bioc, collapse = "', '"), "'))"
        )
    github_cmd <- if (shinyAce::is.empty(missing_github)) "" else
        paste0(
            'if (!requireNamespace("BiocManager", quietly=TRUE))
                install.packages("BiocManager")\n',
            "BiocManager::install(c('", paste0(missing_github, collapse = "', '"), "'))"
        )

    if (length(missing_cran) + length(missing_bioc) + length(missing_github) > 0) {
        shinyWidgets::sendSweetAlert(
            session = session,
            title = "Please install required packages manually",
            text = tags$div(style = "
                        background-color: #FA5858;
                        text-align: left;
                        overflow: auto;
                        white-space: pre;
                        color: black;

                        ",
                            p(cran_cmd),
                            p(bioc_cmd),
                            p(github_cmd)
            ),
            html = TRUE,
            type = "error"
        )
        return(FALSE)
    } else {
        shinytoastr::toastr_success(
            message = "You have all required packages for this tab",
            position = "bottom-right")
        return(TRUE)
    }
}

#' Server side function for dynamicFile
#' @description Server side function for [dynamicFile] to parse the uploaded
#' file path
#' @param input shiny server input
#' @param session shiny server session
#' @param id input file element ID.
#' Do not us `ns()` to wrap the id on server side if inside module
#'
#' @return reactive dataframe, need to extract the value inside reactive
#' expression, observe, or inside `isolate`
#' @export
#' @importFrom shinyFiles getVolumes shinyFileChoose parseFilePaths
#' @examples
#' if(interactive()){
#'     library(shinyjs)
#'     options(sps = list(mode='server')) # Change the mode to 'local' to see difference
#'     ui <- fluidPage(
#'         useShinyjs(),
#'         dynamicFile("getFile"),
#'         textOutput("txt_file")
#'     )
#'
#'     server <- function(input,output,session){
#'         runjs('$(".sps-file input").attr("readonly", true)')
#'         myfile <- dynamicFileServer(input, session, id = "getFile")
#'         observe({
#'             print(myfile()) # remember to use `()` for reactive value
#'         })
#'     }
#'     shinyApp(ui = ui, server = server)
#' }
dynamicFileServer <- function(input, session, id){
    file_return <- reactiveVal(NULL)
    if (spsOption('mode') == "local") {
        roots <- c(current=getwd(), shinyFiles::getVolumes()())
        shinyFiles::shinyFileChoose(input, id, roots = roots, session = session)
        observeEvent(input[[id]],
                     file_return({
                         req(is.list(input[[id]]))
                         file_selected <- shinyFiles::parseFilePaths(roots, input[[id]])
                         updateTextInput(inputId = glue("{id}-text"),
                                         session = session,
                                         placeholder = unname(file_selected$datapath))
                         as.data.frame(file_selected)
                     })
        )
        file_return
    } else {
        observe(file_return(input[[id]]))
        file_return
    }
}

#' A draggable progress panel
#' @description  Creates a panel that displays multiple progress items.
#' Use `pgPaneUI` on UI side and use `pgPaneUpdate` to update it.
#' The UI only
#' renders correctly inside [shinydashboard::dashboardPage()] or
#' [shinydashboardPlus::dashboardPagePlus()].
#'
#' A overall progress is automatically calculated on the bottom.
#' @param pane_id Progress panel main ID, use `ns` wrap it on `pgPaneUI` but not
#' on `pgPaneUpdate` if using shiny module
#' @param pg_id a character string of ID indicating which progress within this
#' panel you want to update.
#'  Do not use `ns(pg_id)` to wrap it on server
#' @param value 0-100 number to update the progress you use `pg_id` to
#' choose
#' @param session current shiny session
#' @importFrom shinyWidgets updateProgressBar
#' @return HTML elements
#' @export
#' @examples
#' if(interactive()){
#'     library(shinydashboard)
#'     # try to slide c under 0
#'     ui <- dashboardPage(header = dashboardHeader(),
#'                         sidebar = dashboardSidebar(),
#'                         body = dashboardBody(
#'                             useSps(),
#'                             h4("you need to open up the progress
#'                                 tracker, it is collapsed ->"),
#'                             actionButton("a", "a"),
#'                             actionButton("b", "b"),
#'                             sliderInput("c", min = -100,
#'                                         max = 100, value = 0,
#'                                         label = "c"),
#'                             pgPaneUI("thispg",
#'                                      c("this a", "this b", " this c"),
#'                                      c("a", "b", "c"), "Example Progress")
#'                         )
#'     )
#'     server <- function(input, output, session) {
#'         observeEvent(input$a, {
#'             for(i in 1:10){
#'                 pgPaneUpdate("thispg", "a", i*10)
#'                 Sys.sleep(0.3)
#'             }
#'         })
#'         observeEvent(input$b, {
#'             for(i in 1:10){
#'                 pgPaneUpdate("thispg", "b", i*10)
#'                 Sys.sleep(0.3)
#'             }
#'         })
#'         observeEvent(input$c, pgPaneUpdate("thispg", "c", input$c))
#'     }
#'     shinyApp(ui, server)
#' }
pgPaneUpdate <- function(pane_id, pg_id, value,
                         session = getDefaultReactiveDomain()){
    shinyCatch({
        assert_that(is.character(pane_id))
        assert_that(is.character(pg_id))
        assert_that(value >= 0 & value <= 100,
                    msg = "Progress value needs to be 0-100")
        shinyWidgets::updateProgressBar(session,
                                        id = glue("{pg_id}-pg"),
                                        value = value)
        if(inherits(session, "session_proxy")){
            pane_id <- session$ns(pane_id)
            pg_id <- session$ns(pg_id)
        }
        session$sendCustomMessage(
            type = "sps-update-pg",
            message = list(
                panel_id = pane_id,
                which_pg = pg_id,
                value = value
            ))
    }, blocking_level = "error")

}

#' Add and get data between SPS tabs
#'
#' These functions are designed to be used inside SPS tabs
#' @param data any type of R object you want to store and use in other tabs
#' @param shared the a `shared` reactivevalues object that is defined on the top
#' level server. Read vignette for more details about this object
#' @param tab_id tab ID of current tab if using `addData` method and tab
#' ID to get data from if using `getData`.
#' @return Nothing to return with `add` method and returns original object for
#' the `get` method
#' @importFrom shinytoastr toastr_info
#' @export
#' @examples
#' if(interactive()){
#'     spsInit()
#'     options(sps = list(verbose = TRUE))
#'     ui <- fluidPage(
#'         useToastr(),
#'         actionButton("add", "add"),
#'         actionButton("get", "get"),
#'         actionButton("wrong", "when it gets wrong")
#'     )
#'     server <- function(input, output, session) {
#'         shared <- reactiveValues()
#'         data <- tibble::tibble(this = 123)
#'         cat('before adding\n')
#'         print(shared)
#'         observeEvent(input$add, {
#'             addData(data, shared, "core_about")
#'             cat('after adding\n')
#'             print(shared) # watch the data_intask object is created
#'         })
#'         observeEvent(input$get, {
#'             cat("get data\n")
#'             print(getData('core_about', shared))
#'         })
#'         observeEvent(input$wrong, {
#'             cat("get wrong data\n")
#'             getData('not_there', shared)
#'         })
#'     }
#'     shinyApp(ui, server)
#'}
addData <- function(data, shared, tab_id) {
    shinyCatch({
        assert_that(inherits(shared, "reactivevalues"))
        assert_that(is.character(tab_id))
        findTabInfo(tab_id)
        if(emptyIsFalse(shared[['data']][[tab_id]]) & spsOption('verbose'))
            message(c(glue("found {tab_id} has already been added to "),
                      "`shared$data_intask` list, overwrite"))
        shared[['data']][[tab_id]] <- data
        if(spsOption('verbose')) {
            info <- glue("Data for namespace {tab_id} added")
            message(info)
            shinytoastr::toastr_info(info,
                                     timeOut = 3000,
                                     position = "bottom-right")
        }
    }, blocking_level = "error")
}


#' @rdname addData
#' @export
#' @importFrom shinytoastr toastr_info
getData <- function(tab_id, shared){
    shinyCatch({
        assert_that(inherits(shared, "reactivevalues"))
        assert_that(is.character(tab_id) & length(tab_id) < 2,
                    msg = "A character string of one tab name each time")
        tab_info <- findTabInfo(tab_id)$tab_labels
        if(!not_empty(shared[['data']][[tab_id]]))
            stop(glue("Data from tab `{tab_info}` is empty"))
        if(spsOption('verbose')){
            success_info <- glue("data for tab `{tab_info} found`")
            shinytoastr::toastr_info(success_info,
                                     timeOut = 3000,
                                     position = "bottom-right")
        }
        return(shared[['data']][[tab_id]])
    }, blocking_level = "error")
}

