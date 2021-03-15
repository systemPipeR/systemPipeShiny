################## A Collections of HTML components#############################
# can be used outside SPS framework, like other shiny projects
## use on top of shiny

# internal css loader unit
spsLoader <- function(id=NULL){
    if(is.null(id)) id = paste0("loader", sample(1000000, 1))
    tagList(
        singleton(
            tags$style('
            .sps-loader {
              height: auto;
              display: inline-block;
              align-items: center;
              justify-content: center;
            }
            .sps-loader .container {
              width: 80px;
              height: 60px;
              text-align: center;
              font-size: 10px;
            }
            .sps-loader .container .boxLoading {
              background-color: #3c8dbc;
              height: 100%;
              width: 6px;
              display: inline-block;
              -webkit-animation: sps-loading 1.2s infinite ease-in-out;
              animation: sps-loading 1.2s infinite ease-in-out;
            }
            .sps-loader .container .boxLoading2 {
              -webkit-animation-delay: -1.1s;
              animation-delay: -1.1s;
            }
            .sps-loader .container .boxLoading3 {
              -webkit-animation-delay: -1s;
              animation-delay: -1s;
            }
            .sps-loader .container .boxLoading4 {
              -webkit-animation-delay: -0.9s;
              animation-delay: -0.9s;
            }
            .sps-loader .container .boxLoading5 {
              -webkit-animation-delay: -0.8s;
              animation-delay: -0.8s;
            }

            @-webkit-keyframes sps-loading {
              0%,
              40%,
              100% {
                -webkit-transform: scaleY(0.4);
              }
              20% {
                -webkit-transform: scaleY(1);
              }
            }
            @keyframes sps-loading {
              0%,
              40%,
              100% {
                transform: scaleY(0.4);
                -webkit-transform: scaleY(0.4);
              }
              20% {
                transform: scaleY(1);
                -webkit-transform: scaleY(1);
              }
            }
          ')
        ),
        tags$div(
            id = id, class = "sps-loader",
            HTML('
            <div class="container">
                <div class="boxLoading boxLoading1"></div>
                <div class="boxLoading boxLoading2"></div>
                <div class="boxLoading boxLoading3"></div>
                <div class="boxLoading boxLoading4"></div>
                <div class="boxLoading boxLoading5"></div>
            </div>
           ')
        )
    )
}


#' Generate gallery by only providing tab names
#' @description A fast way in SPS to generate a gallery to display plot tab
#' screenshots
#' @param tab_ids  a vector of tab IDs
#' @param Id element ID
#' @param title gallery title
#' @param title_color title color, common colors or hex code
#' @param image_frame_size integer, 1-12
#' @param type If this value is not `NULL`, filter by tab type, and tab_ids
#' will be ignored. One of c("core", "wf", "data", "vs"). use [spsTabInfo()]
#' to see tab information
#' @param app_path app path, default current working directory
#' @export
#' @return gallery div
#' @details require a SPS project and the config/tabs.csv file. If you want to
#' use gallery outside a SPS project, use [spsComps::gallery]
#' @examples
#' if(interactive()){
#'     spsInit()
#'     ui <- fluidPage(
#'         genGallery(c("plot_example1")),
#'         genGallery(type = "plot")
#'     )
#'     server <- function(input, output, session) {
#'
#'     }
#'     shinyApp(ui, server)
#' }
genGallery <- function(tab_ids=NULL, Id = NULL, title = "Gallery", type = NULL,
                       title_color = "#0275d8", image_frame_size = 3,
                       app_path = NULL) {
    # use user input path > project setting > default
    if(!emptyIsFalse(app_path)){
        app_path <- spsOption("app_path")
        if(!emptyIsFalse(app_path)) app_path <- getwd()
    }
    tabs <- findTabInfo(
        tab_ids, type,
        force_reload = TRUE,
        tab_file = file.path(app_path, "config", "tabs.csv"))
    if (is.null(tabs)) return(div("Nothing to display in gallery"))
    tabs$images[tabs$images %in% c("", NA)] <- "img/noimg.png"
    gallery(Id = Id, title = title, title_color = title_color,
            image_frame_size = image_frame_size,
            texts = tabs[['tab_labels']], hrefs = tabs[['hrefs']],
            images = tabs[['images']])
}


#genHrefTab <- function(tab_ids, Id = NULL, title = "A bar to list tabs",
#                       text_color = "#0275d8", app_path = NULL, ...) {
#    # use user input path > project setting > default
#    if(!emptyIsFalse(app_path)){
#        app_path <- spsOption("app_path")
#        if(!emptyIsFalse(app_path)) app_path <- getwd()
#    }
#    tabs <- findTabInfo(
#        tab_ids, tab_file = file.path(app_path, "config", "tabs.csv"), force_reload = TRUE)
#    hrefTab(Id = Id, title = title, text_color = text_color,
#            label_text =  tabs[['tab_labels']], hrefs = tabs[['hrefs']], ...)
#}

#' Generate a table that lists tabs by rows
#' @description  A fast way in SPS to generate a table that lists some SPS tabs
#' @param rows a named list of character vector, the item names in the list
#' will be the row names and each item should be a vector of tab IDs.
#' Or you can use one of 'core', 'wf', 'vs', 'data', 'plot' to specify a tab
#' type, so it will find
#' all tabs matching that type. See `tab_info.csv` under `config` directory for
#' type info.
#' @param Id element ID
#' @param title table title
#' @param text_color text color for table
#' @param ... any additional arguments to the html element, like class, style...
#' @param app_path app path, default is current working directory
#' @details For `rows`, there are some specially reserved characters
#' for type and sub-types, one of c('core', 'wf', 'vs', 'data', 'plot').
#' If indicated, it will return a list of tabs matching
#' the indicated tabs instead of searching individual tab names. See examples.
#' @return HTML elements
#' @details This function requires a SPS project and the config/tabs.csv file.
#' If you want to use hrefTable outside a SPS project, or want to create some
#' links pointing to outside web resources, use [spsComps::hrefTable]
#' @export
#' @examples
#' if(interactive()){
#'     spsInit()
#'     # will be two rows, one row is searched by tab IDs and the other is
#'     # searched by type.
#'     rows <- list(row1 = c("core_canvas", "core_about"),
#'                  row2 =  "data")
#'     ui <- fluidPage(
#'         genHrefTable(rows)
#'     )
#'     server <- function(input, output, session) {
#'
#'     }
#'     shinyApp(ui, server)
#' }
genHrefTable <- function(rows, Id = NULL, title = "A Table to list tabs",
                         text_color = "#0275d8", app_path = NULL, ...) {
    # use user input path > project setting > default
    if(!emptyIsFalse(app_path)){
        app_path <- spsOption("app_path")
        if(!emptyIsFalse(app_path)) app_path <- getwd()
    }
    tab_list <- mapply(rows, FUN = function(x) {
        if (length(x) == 1 & x[1] %in% c('core', 'wf', 'vs', 'data', 'plot')){
            findTabInfo(type = x, tab_file = file.path(app_path, "config", "tabs.csv"), force_reload = TRUE)
        } else {
            findTabInfo(x, tab_file = file.path(app_path, "config", "tabs.csv"), force_reload = TRUE)
        }
    }, SIMPLIFY = TRUE)
    hrefTable(Id = Id, title = title,
              text_color = text_color, item_titles = names(rows),
              item_labels = tab_list[2,], item_hrefs = tab_list[3,], ...)
}



