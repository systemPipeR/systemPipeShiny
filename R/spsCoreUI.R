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
#' @importFrom shinydashboard menuSubItem tabItem
#' @importFrom shinydashboard sidebarSearchForm sidebarMenu menuItem tabItems dashboardBody
#' @importFrom shinydashboardPlus dashboardHeader dashboardPage dashboardSidebar dashboardPage
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom shinyjs useShinyjs
#' @noRd
# @examples
# spsUI()
spsUI <- function(tabs, mod_missings, sps_env, guide, login_message){
    spsinfo("Start to generate UI")
    addResourcePath("sps", file.path(spsOption("app_path"), "www"))

    spsinfo("parse title and logo")
    sps_title <- spsOption("title")
    if(!(is.character(sps_title) && length(sps_title) == 1)) spserror("Value for option 'title' is incorrect")
    sps_logo <- spsOption("title_logo")
    if(!(is.character(sps_logo) && length(sps_logo) == 1)) spserror("Value for option 'title_logo' is incorrect")

    spsinfo("resolve default tabs UI")
    core_welcomeUI <- rlang::env_get(sps_env, 'core_welcomeUI', core_welcomeUI)
    module_mainUI <- rlang::env_get(sps_env, 'module_mainUI', module_mainUI)
    vs_mainUI <- rlang::env_get(sps_env, 'vs_mainUI', vs_mainUI)
    core_canvasUI <- rlang::env_get(sps_env, 'core_canvasUI', core_canvasUI)
    core_aboutUI <- rlang::env_get(sps_env, 'core_aboutUI', core_aboutUI)

    if(spsOption("tab_vs_main")) {
        spsinfo("Loading custom tab UI ...")
        menu_tab <- if(nrow(tabs) > 0){
            lapply(seq_len(nrow(tabs)), function(x){
                shinydashboard::menuItem(text = tabs$tab_labels[x],
                                         tabName = tabs$tab_id[x])
            }) %>% tagList()
        } else tagList()
        tab_items <- c(tabs[['tab_id']]) %>% {.[. != ""]} %>%
            lapply(function(x){
                tab_ui <- glue('{x}UI("{x}")') %>% rlang::parse_expr()
                spsinfo(glue("Loading UI for {x}"))
                shinydashboard::tabItem(tabName = x, rlang::eval_tidy(tab_ui))
            })
    } else {
        spsinfo("You choose not to load custom tabs", TRUE)
    }


    # header
    spsinfo("Loading notifications from developer...")
    notes <- parseNote()

    spsinfo("Loading guide UI")

    spsinfo("Create UI header ...")

    dashboardHeader <- shinydashboardPlus::dashboardHeader(
        title = tagList(
            span(class = "logo-lg", sps_title),
            img(src = sps_logo, height = "25", width = "25")
        ),
        leftUi = tagList(
            if(!is.null(mod_missings[['wf']]) && length(mod_missings[['wf']]) == 0) core_topUI("core_top")  else div(),
            div(notes[['modals']])
        ),
        guide[['guide_ui']],
        shinydashboard::dropdownMenu(
            type = "notifications", badgeStatus = if(emptyIsFalse(notes[['items']])) "warning" else "success" ,
            icon = if(emptyIsFalse(notes[['items']])) icon("exclamation-triangle") else icon("check"),
            headerText = "Notifications",
            .list = notes[['items']]
        )
    )
    # side bar
    spsinfo("Create UI sidebar menu ...")
    # detect if any built-in module is loaded
    any_module <- any(
        spsOption("module_wf"),
        spsOption("module_rnaseq"),
        spsOption("module_ggplot"))
    # start to load tabs
    dashboardSidebar <-  shinydashboardPlus::dashboardSidebar(
        br(),
        # shinydashboard::sidebarSearchForm(textId = "searchText",
        #                                   buttonId = "searchButton",
        #                                   label = "Search..."),
        shinydashboard::sidebarMenu(
            id = "left_sidebar",
            if(spsOption("tab_welcome")) shinydashboard::menuItem("Welcome", tabName = "core_welcome", icon = icon("sitemap")) else "",
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
            },
            if(spsOption("tab_vs_main")) shinydashboard::menuItem("Custom tabs", icon = icon("images"), tabName = "vs_main", menu_tab) else "",
            if(spsOption("tab_canvas")) shinydashboard::menuItem("Canvas", tabName = "core_canvas", icon = icon("paint-brush")) else "",
            if(spsOption("tab_about")) shinydashboard::menuItem("About", icon = icon("info"), tabName = "core_about") else ""
        )
    )
    # body
    spsinfo("Create UI tab content ...")
    sps_tabs <- shinydashboard::tabItems(

        # VS tabs
        if(spsOption("tab_vs_main")) shinydashboard::tabItem(tabName = "vs_main", vs_mainUI("vs_main")) else missingTab(),
        # core tabs
        if(any_module) shinydashboard::tabItem(tabName = "module_main", module_mainUI("module_main")) else missingTab(),
        if(spsOption("tab_welcome")) shinydashboard::tabItem(tabName = "core_welcome", core_welcomeUI("core_welcome")) else missingTab(),
        if(spsOption("tab_canvas")) shinydashboard::tabItem(tabName = "core_canvas", core_canvasUI("core_canvas")) else missingTab(),
        if(spsOption("tab_about")) shinydashboard::tabItem(tabName = "core_about", core_aboutUI("core_about")) else missingTab(),
        #  modules
        shinydashboard::tabItem(
            tabName = "wf",
            if(!is.null(mod_missings[['wf']]) && length(mod_missings[['wf']]) == 0) wfUI("wf") else modMissingUI(mod_missings[['wf']])
        ),
        shinydashboard::tabItem(
            tabName = "vs_rnaseq",
            if(!is.null(mod_missings[['rna']]) && length(mod_missings[['rna']]) == 0) vs_rnaseqUI("vs_rnaseq") else modMissingUI(mod_missings[['rna']])
        ),
        shinydashboard::tabItem(
            tabName = "vs_esq",
            if(!is.null(mod_missings[['ggplot']]) && length(mod_missings[['ggplot']]) == 0) vs_esqUI("vs_esq") else modMissingUI(mod_missings[['ggplot']])
        )
    )
    if(spsOption("tab_vs_main")) sps_tabs$children <- append(sps_tabs$children, tab_items)
    spsinfo("Add tab content to body ...")
    dashboardBody <- shinydashboard::dashboardBody(
        class = "sps",
        tags$head(
            tags$script(src="sps/js/sps.js")
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
    mainUI <- shinydashboardPlus::dashboardPage(
        header = dashboardHeader,
        sidebar = dashboardSidebar,
        title = sps_title,
        body =  dashboardBody #,rightsidebar = rightsidebar
    )
    # HTML dependencies
    core_head <- tags$head(
        tags$link(rel="shortcut icon", href=sps_logo),
        shinyWidgets::useSweetAlert(),
        shinytoastr::useToastr(),
        shinyjs::useShinyjs(),
        if(!emptyIsFalse(checkNameSpace('cicerone', TRUE))) cicerone::use_cicerone() else div(),
        tags$link(rel="stylesheet", href = "sps/css/sps.css")
    )

    # merge everything together
    spsinfo("Add overlay loading screen, admin panel.
            Merge everything to app container ...")
    if(spsOption('login_screen')) {
        list(
            login = fluidPage(class = "sps-page", core_head, spsUIuser(login_message), uiOutput("spsUIadmin")),
            main = mainUI
        )
    } else {
        mainUI <- div(id = "page-user-wrapper", mainUI)
        list(main = fluidPage(class = "sps-page", core_head, mainUI, uiOutput("spsUIadmin")))
    }

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

missingTab <- function(){div(class = "tab-pane")}
