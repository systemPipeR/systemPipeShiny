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
spsUI <- function(tabs, module_missings){
    spsinfo("Start to generate UI")
    addResourcePath("sps", "www")
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
    # detect if any built-in module is loaded
    any_module <- any(
        spsOption("module_wf"),
        spsOption("module_rnaseq"),
        spsOption("module_ggplot"))
    # start to load tabs
    dashboardSidebar <-  shinydashboard::dashboardSidebar(
        shinydashboard::sidebarSearchForm(textId = "searchText",
                                          buttonId = "searchButton",
                                          label = "Search..."),
        shinydashboard::sidebarMenu(id = "left_sidebar",
                    shinydashboard::menuItem("Welcome",
                                             tabName = "core_dashboard",
                                             icon = icon("sitemap")
                    ),
                    if (any_module) {
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
                        )
                    } else {
                        ""
                    }
                    ,
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
        shinydashboard::tabItem(
            tabName = "wf",
            if(length(module_missings[['wf']]) == 0) wfUI("wf") else modMissingUI(module_missings[['wf']])
        ),
        shinydashboard::tabItem(
            tabName = "vs_rnaseq",
            if(length(module_missings[['rna']]) == 0) vs_rnaseqUI("vs_rnaseq") else modMissingUI(module_missings[['rna']])
        ),
        shinydashboard::tabItem(
            tabName = "vs_esq",
            if(length(module_missings[['ggplot']]) == 0) vs_esqUI("vs_esq") else modMissingUI(module_missings[['ggplot']])
        )
    )
    sps_tabs$children <- append(sps_tabs$children, tab_items)
    spsinfo("Add tab content to body ...")
    dashboardBody <- shinydashboard::dashboardBody(
        class = "sps",
        tags$head(
            tags$link(rel="shortcut icon", href="img/sps_small.png"),
            shinyWidgets::useSweetAlert(),
            shinytoastr::useToastr(),
            shinyjs::useShinyjs(),
            tags$script(src="sps/js/sps.js"),
            tags$link(rel="stylesheet", href = "sps/css/sps.css")
        ),
        spsComps::spsGoTop(),
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


modMissingUI <- function(install_md){
    if (length(install_md)) {
        div(
            h2("Please install following packages and restart R and then restart the app"),
            markdown(install_md)
        )
    } else div()
}

