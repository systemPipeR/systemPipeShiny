############################### SPS UIs#########################################
# Can only be used inside SPS framework

#' Generate SPS main UI
#'
#' @param tabs custom tabs
#'
#' @return shiny dash page
#' @details Workflow tabs and other `core` tabs are loaded by default. You can
#' only optionally choose visualization tabs. See config/tabs.csv for tab info.
#' @importFrom rlang eval_tidy parse_expr
#' @importFrom shinydashboard menuSubItem tabItem dashboardSidebar
#' @importFrom shinydashboard sidebarSearchForm sidebarMenu menuItem tabItems
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboardPlus dashboardHeaderPlus dashboardPagePlus
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom shinyjs useShinyjs
#' @noRd
# @examples
# spsUI()
spsUI <- function(tabs){
    spsinfo("Start to generate UI")
    menu_tab <- if(nrow(tabs) > 0){
        lapply(seq_len(nrow(tabs)), function(x){
            shinydashboard::menuItem(text = tabs$tab_labels[x],
                                        tabName = tabs$tab_id[x])
        }) %>% tagList()
    } else tagList()


    spsinfo("Loading custom tab UI ...")
    tab_items <- c(tabs[['tab_id']]) %>% {.[. != ""]} %>%
        lapply(function(x){
            tab_ui <- glue('{x}UI("{x}")') %>% rlang::parse_expr()
            spsinfo(glue("Loading UI for {x}"))
            shinydashboard::tabItem(tabName = x, rlang::eval_tidy(tab_ui))
        })
    # header
    spsinfo("Create UI header ...")
    dashboardHeader <- shinydashboardPlus::dashboardHeaderPlus(
        title = tagList(
            span(class = "logo-lg", "systemPipeShiny"),
            img(src = "img/sps_small.png")
        ),
        enable_rightsidebar = FALSE,
        rightSidebarIcon = "clipboard-check",
        left_menu = if(spsOption("module_wf")) core_topUI("core_top") else NULL
    )
    # side bar
    spsinfo("Create UI sidebar menu ...")
    dashboardSidebar <-  shinydashboard::dashboardSidebar(
        shinydashboard::sidebarSearchForm(textId = "searchText",
                                          buttonId = "searchButton",
                                          label = "Search..."),
        shinydashboard::sidebarMenu(id = "left_sidebar",
                    shinydashboard::menuItem("Welcome",
                                             tabName = "core_dashboard",
                                             icon = icon("sitemap")
                    ),
                    shinydashboard::menuItem(
                        text = "Modules",
                        icon = icon("layer-group"),
                        tabName = "module_main",
                        if(spsOption("module_wf"))
                            shinydashboard::menuItem(
                                text = "Workflow Mangement",
                                tabName = "wf"
                            )
                        else "",
                        if(spsOption("module_rnaseq"))
                            shinydashboard::menuItem(
                                text = "RNA-Seq",
                                tabName = "vs_rnaseq"
                            )
                        else "",
                        if(spsOption("module_ggplot"))
                            shinydashboard::menuItem(
                                text = "Quick {ggplot}",
                                tabName = "vs_esq"
                            )
                        else ""
                    ),
                    shinydashboard::menuItem(
                        "Custom tabs",
                        icon = icon("images"),
                        tabName = "vs_main",
                        menu_tab ## add tabs sidebar
                    ),
                    shinydashboard::menuItem(
                        "Canvas",
                        tabName = "core_canvas",
                        icon = icon("paint-brush")
                    ),
                    shinydashboard::menuItem(
                        "About",
                        icon = icon("info"),
                        tabName = "core_about"
                    )
        )
    )
    # body
    spsinfo("Create UI tab content ...")
    sps_tabs <- shinydashboard::tabItems(

        # VS tabs
        shinydashboard::tabItem(tabName = "vs_main", vs_mainUI("vs_main")),
        # core tabs
        shinydashboard::tabItem(tabName = "module_main", module_mainUI("module_main")),
        shinydashboard::tabItem(tabName = "core_dashboard",
                                core_dashboardUI("core_dashboard")),
        shinydashboard::tabItem(tabName = "core_canvas",
                                core_canvasUI("core_canvas")),
        shinydashboard::tabItem(tabName = "core_about",
                                core_aboutUI("core_about")),
        #  modules
        if(spsOption("module_wf")) shinydashboard::tabItem(tabName = "wf", wfUI("wf"))
        else shinydashboard::tabItem(tabName = "empty"),
        if(spsOption("module_rnaseq")) shinydashboard::tabItem(tabName = "vs_rnaseq", vs_rnaseqUI("vs_rnaseq"))
        else shinydashboard::tabItem(tabName = "empty"),
        if(spsOption("module_ggplot")) shinydashboard::tabItem(tabName = "vs_esq", vs_esqUI("vs_esq"))
        else shinydashboard::tabItem(tabName = "empty")
    )
    sps_tabs$children <- append(sps_tabs$children, tab_items)
    spsinfo("Add tab content to body ...")
    dashboardBody <- shinydashboard::dashboardBody(
        class = "sps",
        tags$head(
            tags$link(rel="shortcut icon", href="img/sps_small.png"),
            shinyWidgets::useSweetAlert(),
            useSps(),
        ),
        spsGoTop(),
        disconUI(),
        sps_tabs
    )
    # right side bar, not in use at this moment
    # rightsidebar <- rightSidebar(
    #     background = "light", icon = "clipboard-check", width = 400,
    #     core_rightUI("core_right")
    # )
    # app main UI
    spsinfo("Merge header, menu, body to dashboard ...")
    mainUI <- shinydashboardPlus::dashboardPagePlus(
        header = dashboardHeader,
        sidebar = dashboardSidebar,
        title = "systemPipeShiny",
        body =  dashboardBody #,rightsidebar = rightsidebar
    )
    # merge everything together
    spsinfo("Add overlay loading screen, admin panel.
            Merge everything to app container ...")
    spsUIwrapper(mainUI)
}



#' Generate gallery by only providing tab names
#' @description A fast way in SPS to generate a [gallery] to display plot tab
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
#' use gallery outside a SPS project, use [gallery()]
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
        tab_file = file.path(app_path, "config", "tabs.csv"))
    if (is.null(tabs)) return(div("Nothing to display in gallery"))
    tabs$images[tabs$images %in% c("", NA)] <- "img/noimg.png"
    gallery(Id = Id, title = title, title_color = title_color,
            image_frame_size = image_frame_size,
            texts = tabs[['tab_labels']], hrefs = tabs[['hrefs']],
            images = tabs[['images']])
}

#' @rdname hrefTab
#' @param tab_ids tab names, must have the \code{tab_info} dataframe
#' @param text_color Table text color
#' @param app_path app path, default is current working directory
#' @export
genHrefTab <- function(tab_ids, Id = NULL, title = "A bar to list tabs",
                       text_color = "#0275d8", app_path = NULL, ...) {
    # use user input path > project setting > default
    if(!emptyIsFalse(app_path)){
        app_path <- spsOption("app_path")
        if(!emptyIsFalse(app_path)) app_path <- getwd()
    }
    tabs <- findTabInfo(
        tab_ids, tab_file = file.path(app_path, "config", "tabs.csv"))
    hrefTab(Id = Id, title = title, text_color = text_color,
            label_text =  tabs[['tab_labels']], hrefs = tabs[['hrefs']], ...)
}

#' Generate a table that lists tabs by rows
#' @description  A fast way in SPS to generate a table that lists SPS tabs
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
#' links pointing to outside web resources, use [hrefTable]
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
            findTabInfo(type = x, tab_file = file.path(app_path, "config", "tabs.csv"))
        } else {
            findTabInfo(x, tab_file = file.path(app_path, "config", "tabs.csv"))
        }
    }, SIMPLIFY = TRUE)
    hrefTable(Id = Id, title = title,
              text_color = text_color, item_titles = names(rows),
              item_labels = tab_list[2,], item_hrefs = tab_list[3,], ...)
}


spsGoTop <- function(){
    HTML(
    '
    <div class="sps-gotop" id="gotop" data-toggle="tooltip" data-placement="left" title="Go Top" onclick="goTop()">
      <svg viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg">
        <path d="M526.60727968 10.90185116
          a27.675 27.675 0 0 0-29.21455937 0
          c-131.36607665 82.28402758-218.69155461 228.01873535-218.69155402
          394.07834331a462.20625001 462.20625001 0 0 0 5.36959153 69.94390903
          c1.00431239 6.55289093-0.34802892 13.13561351-3.76865779 18.80351572-32.63518765
          54.11355614-51.75690182 118.55860487-51.7569018 187.94566865a371.06718723 371.06718723 0 0 0 11.50484808 91.98906777
          c6.53300375 25.50556257 41.68394495 28.14064038 52.69160883 4.22606766 17.37162448-37.73630017
          42.14135425-72.50938081 72.80769204-103.21549295 2.18761121 3.04276886 4.15646224 6.24463696
          6.40373557 9.22774369a1871.4375 1871.4375 0 0 0 140.04691725 5.34970492 1866.36093723 1866.36093723 0 0 0 140.04691723-5.34970492
          c2.24727335-2.98310674 4.21612437-6.18497483 6.3937923-9.2178004 30.66633723 30.70611158
          55.4360664 65.4791928 72.80769147 103.21549355 11.00766384 23.91457269 46.15860503 21.27949489
          52.69160879-4.22606768a371.15156223 371.15156223 0 0 0
          11.514792-91.99901164c0-69.36717486-19.13165746-133.82216804-51.75690182-187.92578088-3.42062944-5.66790279-4.76302748-12.26056868-3.76865837-18.80351632a462.20625001
          462.20625001 0 0 0 5.36959269-69.943909c-0.00994388-166.08943902-87.32547796-311.81420293-218.6915546-394.09823051zM605.93803103
          357.87693858a93.93749974 93.93749974 0 1 1-187.89594924 6.1e-7 93.93749974 93.93749974 0 0 1 187.89594924-6.1e-7z">
        </path>
        <path d="M429.50777625 765.63860547C429.50777625 803.39355007 466.44236686
          1000.39046097 512.00932183 1000.39046097c45.56695499 0 82.4922232-197.00623328
          82.5015456-234.7518555 0-37.75494459-36.9345906-68.35043303-82.4922232-68.34111062-45.57627738-0.00932239-82.52019037
          30.59548842-82.51086798 68.34111062z">
        </path>
      </svg>
    </div>
    '
    )
}


disconUI <- function(){
    if(Sys.getenv("SHINY_PORT", "") == "") {
        HTML('
            <div id="ss-connect-dialog" style="display: none;">
              <a id="ss-reload-link" href="#" onclick="window.location.reload(true);"></a>
            </div>
            <div id="ss-overlay" style="display: none;"></div>
        ')
    } else div()
}
