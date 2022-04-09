## UI
#' @importFrom shinydashboardPlus userBox
#' @noRd
core_aboutUI <- function(id){
    ns <- NS(id)
    tags$div(
        tabTitle("About this app"),
        markdown(
            '
            ### [Read the manual{blk}](https://systempipe.org/sps/)
            Read the full manual on our [website{blk}](https://systempipe.org/sps/)
            or click the links below of each SPS feature to navigate to different part
            of the manual.
            '
        ),
        gallery(
            texts = c(
                "App structure",
                "User login",
                "Loading themes",
                "Workflow module",
                "Workflow metadata",
                "Workflow step selection & desgin",
                "Workflow Execution",
                "RNAseq normalization",
                "RNAseq DEG",
                "RNAseq plots",
                "Canvas Module",
                "Admin login",
                "Admin app stats",
                "Admin user control",
                "Customizable notifications",
                "Customizable interactive tutorials",
                "Logging and error handling"
            ),
            hrefs = c(
                "https://systempipe.org/sps/intro/",
                "https://systempipe.org/sps/adv_features/login/#main-app-login",
                "https://systempipe.org/sps/adv_features/login/#main-app-login",
                "https://systempipe.org/sps/modules/workflow/",
                "https://systempipe.org/sps/modules/workflow/#2-prepare-a-target-file",
                "https://systempipe.org/sps/modules/workflow/#3-prepare-a-workflow-file",
                "https://systempipe.org/sps/modules/workflow/#5-run-or-finish-workflow-preparation",
                "https://systempipe.org/sps/modules/rnaseq/",
                "https://systempipe.org/sps/modules/rnaseq/#deg-report",
                "https://systempipe.org/sps/modules/rnaseq/#plot-options",
                "https://systempipe.org/sps/canvas/",
                "https://systempipe.org/sps/adv_features/login/#admin-page",
                "https://systempipe.org/sps/adv_features/login/#app-information",
                "https://systempipe.org/sps/adv_features/login/#account-control",
                "https://systempipe.org/sps/adv_features/notification/",
                "https://systempipe.org/sps/adv_features/guide/",
                "https://systempipe.org/sps/adv_features/debug/"

            ),
            images = c(
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/content/en/sps/img/sps_structure.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/user_login.png?raw=true",
                "https://systempipe.org/sps/adv_features/login_theme.gif",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/wf_main.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/wf_targets.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/wf_wf.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/wf_run.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/rnaseq_normalize.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/rnaseq_deg.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/rnaseq_heatmap.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/canvas.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/admin_login.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/admin_server_info.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/admin_user_control.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/sps_notification.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/sps_guide.png?raw=true",
                "https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/sps/img/logging.png?raw=true"
            ),
            title = "SPS Features",
            enlarge = TRUE,
            enlarge_method = "modal",
            image_frame_size = 2,
            target_blank = TRUE
        ),
        spsHr(),
        h3("Change log"),
        box(
            closable = FALSE,
            collapsible = TRUE,
            width = 12,
            style = "height: 500px; overflow-Y: auto;",
            if(checkUrl("https://raw.githubusercontent.com/systemPipeR/systemPipeShiny/master/NEWS.md")){
                markdown(readLines("https://raw.githubusercontent.com/systemPipeR/systemPipeShiny/master/NEWS.md"))
            } else {
                div(
                    h3("Cannot load SPS updates"),
                    p("Visit here:"),
                    tags$a(href="https://github.com/systemPipeR/systemPipeShiny/blob/master/NEWS.md",
                           target="_blank",
                           "https://github.com/systemPipeR/systemPipeShiny/blob/master/NEWS.md")
                )
            }
        ),
        spsHr(),
        column(
            12, class = "desc-table",
            h3(class="text-center", 'Other packages in systemPipeShiny'),
            markdown(
                '
                | Package | Description | Documents | Function reference | Demo |
                | --- | --- | --- | :---: | --- |
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/sps_small.png?raw=true" align="right" height="30" width="30"/>[systemPipeShiny{blk}](https://github.com/systemPipeR/systemPipeShiny) | SPS main package |[website{blk}](https://systempipe.org/sps/)|[link{blk}](https://systempipe.org/sps/funcs/sps/reference/)  | [demo{blk}](https://tgirke.shinyapps.io/systemPipeShiny/)|
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/spscomps.png?raw=true" align="right" height="30" width="30" />[spsComps{blk}](https://github.com/lz100/spsComps) | SPS UI and server components |[website{blk}](https://systempipe.org/sps/dev/spscomps/)|[link{blk}](https://systempipe.org/sps/funcs/spscomps/reference/)  | [demo{blk}](https://lezhang.shinyapps.io/spsComps)|
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/drawer.png?raw=true" align="right" height="30" width="30" />[drawer{blk}](https://github.com/lz100/drawer) | SPS interactive image editing tool |[website{blk}](https://systempipe.org/sps/dev/drawer/)|[link{blk}](https://systempipe.org/sps/funcs/drawer/reference/)  | [demo{blk}](https://lezhang.shinyapps.io/drawer)|
                |<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/spsutil.png?raw=true" align="right" height="30" width="30" />[spsUtil{blk}](https://github.com/lz100/spsUtil) | SPS utility functions |[website{blk}](https://systempipe.org/sps/dev/spsutil/)|[link{blk}](https://systempipe.org/sps/funcs/spsutil/reference/)  | NA|
                '
            )
        ),
        spsHr(),
        h3("Developers"),
        fluidRow(
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://github.com/lz100">Le Zhang</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars0.githubusercontent.com/u/35240440?s=460&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'Student'
            ),
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://github.com/dcassol">Daniela Cassol</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars2.githubusercontent.com/u/12722576?s=400&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'Postdoc'
            ),
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://github.com/mathrj">Ponmathi Ramasamy</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars2.githubusercontent.com/u/45085174?s=400&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'Student'
            ),
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://github.com/jianhaizhang">Jianhai Zhang</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars0.githubusercontent.com/u/22919387?s=400&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'Student'
            ),
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://github.com/gdmosher">Gordon Mosher</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars0.githubusercontent.com/u/8660309?s=460&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'Student'
            ),
            shinydashboardPlus::userBox(
                title = shinydashboardPlus::userDescription(
                    title = HTML('<a target="_blank" href="https://girke.bioinformatics.ucr.edu">Thomas Girke</a>'),
                    subtitle = br(),
                    type = 2,
                    image = "https://avatars3.githubusercontent.com/u/1336916?s=400&v=4",
                ),
                width = 3,
                collapsible = FALSE,
                'PI'
            ),
        ),
        br()
    )
}

## server
core_aboutServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns

    }
    moduleServer(id, module)
}
