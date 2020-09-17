############################### SPS UIs#########################################
# Can only be used inside SPS framework

#' Generate SPS main UI
#'
#' @param tabs_df vs data tab IDs
#' @param tabs_plot vs plot tab IDs
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
spsUI <- function(tabs_df, tabs_plot){
    spsinfo("Start to generate UI")
    menu_df <- if(nrow(tabs_df) > 0){
        lapply(seq_len(nrow(tabs_df)), function(x){
            shinydashboard::menuSubItem(text = tabs_df$tab_labels[x],
                                        tabName = tabs_df$tab_id[x])
        }) %>% tagList()
    } else tagList()
    menu_plot <- if(nrow(tabs_plot) > 0){
        lapply(seq_len(nrow(tabs_plot)), function(x){
            shinydashboard::menuSubItem(text = tabs_plot$tab_labels[x],
                                        tabName = tabs_plot$tab_id[x])
        }) %>% tagList()
    } else tagList()

    spsinfo("Loading custom tab UI ...")
    tab_items <- c(tabs_df[['tab_id']],
                   tabs_plot[['tab_id']]) %>% {.[. != ""]} %>%
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
        left_menu = core_topUI("core_top")
    )
    # side bar
    spsinfo("Create UI sidebar menu ...")
    dashboardSidebar <-  shinydashboard::dashboardSidebar(
        shinydashboard::sidebarSearchForm(textId = "searchText",
                                          buttonId = "searchButton",
                                          label = "Search..."),
        shinydashboard::sidebarMenu(id = "left_sidebar",
                    shinydashboard::menuItem("Dashboard",
                                             tabName = "core_dashboard",
                                             icon = icon("sitemap")
                    ),
                    shinydashboard::menuItem(id = 'wf-control',
                             HTML('Workflow Mangement<small class="badge
                                  pull-right bg-olive">Beta</small>'),
                             tabName = "wf_main",
                             shinydashboard::menuSubItem(text="Targets",
                                                         tabName="wf_targets"),
                             shinydashboard::menuSubItem(text="Workflow File",
                                                         tabName="wf_wf"),
                             shinydashboard::menuSubItem(text="Workflow Config",
                                                         tabName="wf_config"),
                             shinydashboard::menuSubItem(text="Run Workflow",
                                                         tabName="wf_run")
                    ),
                    shinydashboard::menuItem(
                        "Visualization",
                        icon = icon("images"),
                        tabName = "vs_main",
                        shinydashboard::menuItem(
                            text = "Prepare dataset",
                            ## vs dfs add to sidebar
                            egComponents("ui_menu_df"),
                            menu_df
                        ),
                        shinydashboard::menuItem(
                            text = "Collection of plots",
                            ## vs plots add to sidebar
                            egComponents("ui_menu_plot"),
                            menu_plot
                        )
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
        # WF tabs
        shinydashboard::tabItem(tabName = "wf_main", wf_mainUI("wf_main")),
        wfPanel(),
        shinydashboard::tabItem(tabName = "wf_targets",
                                wf_targetUI("wf_targets")),
        shinydashboard::tabItem(tabName = "wf_wf", wf_wfUI("wf_wf")),
        shinydashboard::tabItem(tabName = "wf_config",
                                wf_configUI("wf_config")),
        shinydashboard::tabItem(tabName = "wf_run", wf_runUI("wf_run")),
        # VS tabs
        shinydashboard::tabItem(tabName = "vs_main", vs_mainUI("vs_main")),
        egComponents("ui_tab_df"),
        egComponents("ui_tab_plot"),
        # core tabs
        shinydashboard::tabItem(tabName = "core_dashboard",
                                core_dashboardUI("core_dashboard")),
        shinydashboard::tabItem(tabName = "core_canvas",
                                core_canvasUI("core_canvas")),
        shinydashboard::tabItem(tabName = "core_about",
                                core_aboutUI("core_about"))
    )
    sps_tabs$children <- append(sps_tabs$children, tab_items)
    spsinfo("Add tab content to body ...")
    dashboardBody <- shinydashboard::dashboardBody(
        tags$head(
            tags$link(rel="shortcut icon", href="img/sps_small.png"),
            shinyjs::useShinyjs(),
            shinyWidgets::useSweetAlert(),
            useSps(),
        ),
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
#'
#' @export
#' @return gallery div
#' @details require a SPS project and the config/tabs.csv file. If you want to
#' use gallery outside a SPS project, use [gallery()]
#' @examples
#' if(interactive()){
#'     spsInit()
#'     ui <- fluidPage(
#'         genGallery(c("plot_example")),
#'         genGallery(type = "plot")
#'     )
#'     server <- function(input, output, session) {
#'
#'     }
#'     shinyApp(ui, server)
#' }
genGallery <- function(tab_ids=NULL, Id = NULL, title = "Gallery", type = NULL,
                       title_color = "#0275d8", image_frame_size = 3) {
    tabs <- findTabInfo(tab_ids, type)
    if (is.null(tabs)) return(div("Nothing to display in gallery"))
    tabs$images[tabs$images == ""] <- "img/noimg.png"
    gallery(Id = Id, title = title, title_color = title_color,
            image_frame_size = image_frame_size,
            texts = tabs[['tab_labels']], hrefs = tabs[['hrefs']],
            images = tabs[['images']])
}

#' @rdname hrefTab
#' @param tab_ids tab names, must have the \code{tab_info} dataframe
#' @param text_color Table text color
#' @export
genHrefTab <- function(tab_ids, Id = NULL, title = "A bar to list tabs",
                       text_color = "#0275d8", ...) {
    tabs <- findTabInfo(tab_ids)
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
#'     rows <- list(row1 = c("core_canvas", "core_about"), row2 =  "data")
#'     ui <- fluidPage(
#'         genHrefTable(rows)
#'     )
#'     server <- function(input, output, session) {
#'
#'     }
#'     shinyApp(ui, server)
#' }
genHrefTable <- function(rows, Id = NULL, title = "A Table to list tabs",
                         text_color = "#0275d8", ...) {
    tab_list <- sapply(rows, function(x) {
        if (length(x) == 1 & x[1] %in% c('core', 'wf', 'vs', 'data', 'plot')){
            findTabInfo(type = x)
        } else {findTabInfo(x)}
    })
    hrefTable(Id = Id, title = title,
              text_color = text_color, item_titles = names(rows),
              item_labels = tab_list[2,], item_hrefs = tab_list[3,], ...)
}


#' Create template tabs and servers
#' @description generate UI and server part for template components if `dev`
#' option is TRUE. See ui.R and server.R for example. Normally there is no need
#' to change code of this function ui or server scripts.
#' @param element choose from "ui_menu_df", "ui_menu_plot","ui_tab_df",
#' "ui_tab_plot", "server"
#' @param shared use only when `element` is 'server'
#'
#' @return ui_xx returns html tags, server will return a server function
#'
#' @importFrom shinydashboard menuSubItem tabItem
#' @noRd
egComponents <- function(element, shared=NULL){
    element <- match.arg(element, c("ui_menu_df", "ui_menu_plot",
                                    "ui_tab_df", "ui_tab_plot", "server"))

    if(spsOption('eg_tab')){
        switch(element,
                "ui_menu_df" = shinydashboard::menuSubItem(
                    text = "Template data", tabName = "data_example"),
                "ui_menu_plot" = shinydashboard::menuSubItem(
                    text = "Template Plot", tabName = "plot_example"),
                "ui_tab_df" = shinydashboard::tabItem(
                    tabName = "data_example", data_exampleUI("data_example")),
                "ui_tab_plot" = shinydashboard::tabItem(
                    tabName = "plot_example",
                    plot_exampleUI("plot_example")
                ),
                "server" = {
                    data_exampleServer("data_example", shared)
                    plot_exampleServer("plot_example", shared)
                }
        )
    } else {shinydashboard::tabItem("")}
}


#' Workflow progress tracker UI
#' @description call it on top level UI not inside a module. Call this function
#' only once. Do not repeat this function.
#' @noRd
# @examples
# wfPanel()
wfPanel <- function(){
    div(class = "tab-pane", id = "wf-panel",
        absolutePanel(
            top = "3%", right = "1%", draggable = TRUE, width = "300",
            height = "auto", class = "control-panel", cursor = "inherit",
            style = "background-color: white; z-index:999;",
            fluidRow(
                column(2),
                column(8, h4("Workflow Progress")),
                column(2, HTML('<button class="action-button
                                  bttn bttn-simple bttn-xs bttn-primary
                                  bttn-no-outline" data-target="#wf-panel-main"
                                  data-toggle="collapse">
                                  <i class="fa fa-minus"></i></button>'))
            ),
            div(class = "collapse",
                id = "wf-panel-main",
                uiOutput("wf_panel"))
        )
    )
}


