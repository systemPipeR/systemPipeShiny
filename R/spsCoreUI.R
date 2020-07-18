############################### SPS UIs#########################################
# Can only be used inside SPS framework

#' Generate SPS UI
#'
#' @param tabs_df vs data tab IDs
#' @param tabs_plot vs plot tab IDs
#'
#' @return shiny dash page
#' @details Workflow tabs and other `core` tabs are loaded by default. You can
#' only optionally choose visualization tabs. See config/tabs.csv for tab info.
#' @examples
#' spsUI()
spsUI <- function(tabs_df, tabs_plot){
    spsinfo("Start to generate UI")
    menu_df <- if(nrow(tabs_df) > 0){
        sapply(seq_len(nrow(tabs_df)), function(x){
            menuSubItem(text = tabs_df$tab_labels[x], tabName = tabs_df$tab_name[x])
        }, simplify = FALSE) %>% tagList()
    } else tagList()
    menu_plot <- if(nrow(tabs_plot) > 0){
        sapply(seq_len(nrow(tabs_plot)), function(x){
            menuSubItem(text = tabs_plot$tab_labels[x], tabName = tabs_plot$tab_name[x])
        }, simplify = FALSE) %>% tagList()
    } else tagList()

    spsinfo("Loading custom tab UI ...")
    tab_items <- c(tabs_df[['tab_name']], tabs_plot[['tab_name']]) %>% {.[. != ""]} %>%
        sapply(function(x){
            tab_ui <- glue('{x}UI("{x}")') %>% parse_expr()
            spsinfo(glue("Loading UI for {x}"))
            tabItem(tabName = x, eval_tidy(tab_ui))
        }, simplify = FALSE)
    # header
    spsinfo("Create UI header ...")
    dashboardHeader <- dashboardHeaderPlus(
        title = tagList(
            span(class = "logo-lg", "systemPipeShiny"),
            img(src = "img/systemPipe_small.png")
        ),
        enable_rightsidebar = FALSE,
        rightSidebarIcon = "clipboard-check",
        left_menu = core_topUI("core_top")
    )
    # side bar
    spsinfo("Create UI sidebar menu ...")
    dashboardSidebar <-  dashboardSidebar(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        sidebarMenu(id = "left_sidebar",
                    menuItem("Dashboard", tabName = "core_dashboard", icon = icon("sitemap")
                    ),
                    menuItem(id = 'wf-control',
                             HTML('Workflow Mangement<small class="badge pull-right bg-olive">Beta</small>'),
                             tabName = "wf_main",
                             menuSubItem(text = "Targets", tabName = "wf_targets", ),
                             menuSubItem(text = "Workflow File", tabName = "wf_wf"),
                             menuSubItem(text = "Workflow Config", tabName = "wf_config"),
                             menuSubItem(text = "Run Workflow", tabName = "wf_run")
                    ),
                    menuItem(
                        "Visualization", icon = icon("images"), tabName = "vs_main",
                        menuItem(
                            text = "Prepare dataset",
                            ## vs dfs add to sidebar
                            devComponents("ui_menu_df"),
                            menu_df
                        ),
                        menuItem(
                            text = "Collection of plots",
                            ## vs plots add to sidebar
                            devComponents("ui_menu_plot"),
                            menu_plot
                        )
                    ),
                    menuItem("Canvas", tabName = "core_canvas", icon = icon("paint-brush")
                    ),
                    menuItem("About", icon = icon("info"), tabName = "core_about")
        )
    )
    # body
    spsinfo("Create UI tab content ...")
    sps_tabs <- tabItems(
        # WF tabs
        tabItem(tabName = "wf_main", wf_mainUI("wf_main")),
        wfPanel(),
        tabItem(tabName = "wf_targets", wf_targetUI("wf_targets")),
        tabItem(tabName = "wf_wf", wf_wfUI("wf_wf")),
        tabItem(tabName = "wf_config", wf_configUI("wf_config")),
        tabItem(tabName = "wf_run", wf_runUI("wf_run")),
        # VS tabs
        tabItem(tabName = "vs_main", vs_mainUI("vs_main")),
        devComponents("ui_tab_df"),
        devComponents("ui_tab_plot"),
        # core tabs
        tabItem(tabName = "core_dashboard", core_dashboardUI("core_dashboard")),
        tabItem(tabName = "core_canvas", core_canvasUI("core_canvas")),
        tabItem(tabName = "core_about", core_aboutUI("core_about"))
    )
    sps_tabs$children <- append(sps_tabs$children, tab_items)
    spsinfo("Add tab content to body ...")
    dashboardBody <- dashboardBody(
        tags$head(
            tags$link(rel="shortcut icon", href="img/systemPipe_small.png"),
            useShinyjs(),
            useSweetAlert(),
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
    mainUI <- dashboardPagePlus(header = dashboardHeader, sidebar = dashboardSidebar,
                                title = "systemPipeShiny",
                                body =  dashboardBody #,rightsidebar = rightsidebar
    )
    # merge everything together
    spsinfo("Add overlay loading screen, admin panel. Merge everything to app container ...")
    spsUIwrapper(mainUI)
}



#' generate gallery by only providing tab names
#'
#' @param tabnames tab names, string vector
#' will be included
#' @param Id div ID
#' @param title gallery title
#' @param title_color title color
#' @param image_frame_size integer, 1-12
#' @param img_height css style like '300px'
#' @param img_width css style like '480px'
#' @param type filter by tab type, then tabnames will be ignored: core, wf, data, vs
#'
#' @return gallery div
#'
#' @example
#' library(shiny)
#' ui <- fluidPage(
#'     genGallery(c("tab_a", "tab_b"))
#' )
#' server <- function(input, output, session) {
#'
#' }
#' shinyApp(ui, server)
genGallery <- function(tabnames=NULL, Id = NULL, title = "Gallery", type = NULL,
                       title_color = "#0275d8", image_frame_size = 3) {
    tabs <- findTabInfo(tabnames, type)
    if (is.null(tabs)) return(div("Nothing to display in gallery"))
    tabs$images[tabs$images == ""] <- "img/noimg.png"
    gallery(Id = Id, title = title, title_color = title_color,
            image_frame_size = image_frame_size,
            texts = tabs[['tab_labels']], hrefs = tabs[['hrefs']],
            images = tabs[['images']])
}

#' generate a table of lists of hyper reference buttons by using tab config file
#'
#' @param rows a list of rows, each item name in the list will be the row name,
#' each item is a vector of tab names
#' @param Id element ID
#' @param title table title
#' @param text_color text color for table
#' @param ... any additional args to the html element, like class, style ...
#' @details For `rows`, there are some specially reserved characters
#' for type and sub types. If indicated, it will return a list of tabs matching
#' the indicated tabs instead of searching individual tab names. These words
#' include: core, wf, vs, data, plot.
#' @example
#' library(shiny)
#' rows <- list(wf1 = c("df_raw", "df_count"), wf2 =  "data")
#' ui <- fluidPage(
#'     genHrefTable(rows)
#' )
#' server <- function(input, output, session) {
#'
#' }
#' shinyApp(ui, server)
genHrefTable <- function(rows, Id = NULL, title = "A Table to list tabs",
                         text_color = "#0275d8", ...) {
    tab_list <- sapply(rows, function(x) {
        if (length(x) == 1 & x[1] %in% c('core', 'wf', 'vs', 'data', 'plot')){
            findTabInfo(type = x)
        } else {findTabInfo(x)}

    })
    hrefTable(Id = Id, title = title,
              text_color = text_color, item_titles = names(rows),
              item_labels = tab_list[1,], item_hrefs = tab_list[2,], ...)
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
#' @export
#'
devComponents <- function(element, shared=NULL){
    element <- match.arg(element, c("ui_menu_df", "ui_menu_plot",
                                    "ui_tab_df", "ui_tab_plot", "server"))

    if(is.null(getOption("sps")$dev)) {
        options(sps = getOption("sps") %>% {.[['dev']] <- FALSE; .})
        spswarn("dev option is not set, set to FALSE")
    }
    if(getOption("sps")$dev){
        switch (element,
                "ui_menu_df" = menuSubItem(text = "Template data", tabName = "df_template"),
                "ui_menu_plot" = menuSubItem(text = "Template Plot", tabName = "plot_template"),
                "ui_tab_df" = tabItem(tabName = "df_template", df_templateUI("df_template")),
                "ui_tab_plot" = tabItem(tabName = "plot_template", plot_templateUI("plot_template")),
                "server" = {
                    df_templateServer("df_template", shared)
                    plot_templateServer("plot_template", shared)
                }
        )
    } else {tabItem("")}
}


#' Workflow progress tracker UI
#' @description call it on top level UI not inside a module. Call this function
#' only once. Do not repeat this function.
#' @export
#' @examples
#' wfPanel()
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


