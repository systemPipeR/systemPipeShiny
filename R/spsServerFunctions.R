################## A Collections of server utilities############################

## use on top of shiny

#' @import shiny shinytoastr stringr magrittr glue htmltools shinyWidgets
NULL


#' Catch  error, warning, message text by a toastr bar on shiny
#'
#' @param expr 
#' @param position c("top-right", "top-center", "top-left",
# "top-full-width", "bottom-right", "bottom-center", "bottom-left",
# "bottom-full-width")
#'
#' @return toastr pop-up
#' @export
#'
#' @ example
#' ui <- fluidPage(
#'     useToastr(),
#'     actionButton("btn1","Click me1"),
#'     actionButton("btn2","Click me2"),
#'     actionButton("btn3","Click me3"),
#' 
#'     textOutput("text")
#' )
#' server <- function(input, output, session) {
#'     observeEvent(input$btn1, {
#'         shinyCatch(stop("stop"))
#'     })
#'     observeEvent(input$btn2, {
#'         shinyCatch(warning("warning"))
#'     })
#'     observeEvent(input$btn3, {
#'         shinyCatch(message("message"))
#'     })
#' }
#' shinyApp(ui, server)
shinyCatch <- function(expr, position = "bottom-right") {
    tryCatch(
        {expr},
        error = function(e) {
            print(glue("{Sys.time()} There is a(n) error:\n {e$message}"))
            toastr_error(
                message = e$message, position = position, closeButton = TRUE, timeOut = 0, 
                title = "There is an error", hideDuration = 300,
                    )
        },
        warning = function(w) {
            print(glue("{Sys.time()} There is a(n) warning:\n {w$message}"))
            toastr_warning(message = w$message, position = position, closeButton = TRUE, timeOut = 5000)
        },
        message = function(m) {
            print(glue("{Sys.time()} There is a(n) message:\n {m$message}"))
            toastr_info(message = m$message, position = position, closeButton = TRUE, timeOut = 3000)
        })
}



#' check name space in server
#' check name space and pop up warnings in shiny if package is missing
#' 
#' @param session shiny session
#' @param cran_pkg vector of strings
#' @param bioc_pkg vector of strings
#' @param github vector of strings, github package must specify user name, c("user1/pkg1", "user2/pkg2")
#' @param quietly bool, should progress and error messages be suppressed?
#'
#' @return sweet alert massage
#' @export
#'
#' @ example
#' shinyApp(ui = shinyUI(
#'     fluidPage(actionButton("haha", "haha"))
#' ), server = function(input, output, session) {
#'     observeEvent(input$haha, shinyCheckSpace(session, cran_pkg = "1", bioc_pkg = "haha", github = "sdasdd/asdsad"))
#' })
shinyCheckSpace <- function(session, cran_pkg = NULL, bioc_pkg = NULL, github = NULL, quietly = FALSE) {
    missing_cran <- checkNameSpace(cran_pkg, quietly, from = "CRAN")
    missing_bioc <- checkNameSpace(bioc_pkg, quietly, from = "BioC")
    github_pkg <- github %>% str_remove("^.*/")
    missing_github_pkg <- checkNameSpace(github_pkg, quietly, from = "GitHub")
    missing_github <- github[github_pkg %in% missing_github_pkg]
    cran_cmd <- if (length(missing_cran) < 1) "" else 
        paste0("install.packages(c('", paste0(missing_cran, collapse = "', '"), "'))")
    bioc_cmd <- if (length(missing_bioc) < 1) "" else 
        paste0(
        'if (!requireNamespace("BiocManager", quietly=TRUE))
        install.packages("BiocManager")\n',
        "BiocManager::install(c('", paste0(missing_bioc, collapse = "', '"), "'))"
        )
    github_cmd <- if (length(missing_github) < 1) "" else 
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
        shinyWidgets::sendSweetAlert(
            session = session, 
            title = "No package missing",
            text = "You have all required package(s).",
            type = "success"
        )
        return(TRUE)
    }
}





#' Find tab information from tabs.csv
#'
#' @importFrom readr read_csv
#' @param tabnames vector of strings, tab names you want to get
#'
#' @return a list contains tab labels, tab hyper reference, images
#' @export
#'
#' @examples
#' tabnames <- c("wf_wf", "d", "sas")
#' findTabInfo(tabnames)
findTabInfo <- function(tabnames) {
    assert_that(is.character(tabnames))
    tabs <- if (exists("tab_info")) {
        tab_info
    } else {
        assertthat::is.readable("tabs.csv")
        read_csv("tabs.csv", comment = "#", na = character())
    }
    tabnames %in% tabs$Tab_name %>% {
        glue("Tab {tabnames[!.]} is not in the tab list") %>% lapply(warning, call. = FALSE) %>% quiet()
        list(
            tab_labels = tabs$Display_label[.],
            hrefs = glue("#shiny-tab-{tabs$Tab_name[.]}"),
            images = tabs$image[.]
        )
    } %>% return()
}


genGallery <- function(tabnames, Id = NULL, title = "Gallery",
                       title_color = "#0275d8", image_frame_size = 4, img_height = "300px",
                       img_width = "480px") {
    
}
