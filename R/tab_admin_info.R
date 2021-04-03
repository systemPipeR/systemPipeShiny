## UI
admin_infoUI <- function(id){
    ns <- NS(id)
    tagList(
        tabTitle("App server information"),
        spsHr(),
        br(),
        fluidRow(
            shinydashboard::infoBoxOutput(ns("uptime")),
            shinydashboard::infoBoxOutput(ns("cpu_now")),
            shinydashboard::infoBoxOutput(ns("cpu_temp")),
            shinydashboard::infoBoxOutput(ns("ram_now")),
            shinydashboard::infoBoxOutput(ns("disk_now"))
        ),
        spsHr(), h3("Details"),
        div(id = ns("detail_panel"), tabsetPanel(
            id = "asas",
            tabPanel("CPU", p("Assd")),
            tabPanel("RAM", p("Assd")),
            tabPanel("Disk", p("Assd"))
        ))
    )
}

# library(shiny)
#
# ui <- fluidPage(
#     plotly::plotlyOutput("p1")
# )
#
# aaa <- getCPU()
#
#
# nrow(aaa)
# p1 <- plot_ly(df, type = 'scatter', mode = 'lines')
# for(i in seq_len(nrow(aaa))) {
#     p1 <- p1 %>% add_trace(x= seq(30), y = 0, name = aaa[['V1']][i])
# }
#
# server <- function(input, output, session) {
#     df <- data.frame(x= 1:10, y1=1, y2 =2)
#     p1_proxy <- plotlyProxy("p1")
#
#     output$p1 <- renderPlotly({
#         p1 %>% layout(
#             xaxis = list(title = "", showticklabels = FALSE),
#             yaxis = list(title = "CPU Usage (%)", range = c(0, 100))) %>%
#             toWebGL()
#     })
#     observe({
#         invalidateLater(2000)
#         lapply(aaa[['V2']], list)
#         aaa <- getCPU()
#         plotlyProxy("p1", session) %>%
#             plotlyProxyInvoke("extendTraces",
#                               list(y = lapply(aaa[['V2']], list)),
#                               Reduce(append, seq_len(nrow(aaa)), list()),
#                               30
#             )
#     })
# }
#
# shinyApp(ui, server)

## server
admin_infoServer <- function(id, shared){

    timeDiffString <- function(){
        if(!exists("time_start")) return("NA")
        if(!inherits(time_start, "POSIXct")) return("NA")
        pass_time <- as.numeric(difftime(Sys.time(), time_start, units = "mins"))
        if(pass_time >= 60) pass_time <- as.numeric(difftime(Sys.time(), time_start, units = "hours"))
        else return(paste(round(pass_time), "mins"))
        if(pass_time >= 60) pass_time <- as.numeric(difftime(Sys.time(), time_start, units = "days"))
        else return(paste(round(pass_time), "hours"))
        return(paste(round(pass_time), "days"))
    }

    getCPU <- function(){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        cpu_file <- system.file("app", "bash", "get_cpu.sh", package = "systemPipeShiny")
        cpu_now <- try(system(paste("bash", cpu_file, "0.5"), intern = TRUE))
        if (inherits(cpu_now, "try-error") || length(cpu_now) == 0) return("Cannot get it")
        cpu_strings <- stringr::str_split(cpu_now, "-")
        cpu_strings %>% unlist() %>%
            matrix(nrow=length(cpu_strings), byrow=TRUE) %>%
            {quiet(tibble::as_tibble(.))} %>%
            dplyr::mutate(V2 = as.numeric(V2))
    }
    cpuString <- function(cpu_df){
        if (is.character(cpu_df)) return(cpu_df)
        return(paste0(round(cpu_df[1, 2], 2), "%"))
    }


    getRAM <- function(){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        ram_now <- try(system("free --mega | egrep \"Mem|Swap\" | awk '{print $3 \"\\n\" $2}'", intern = TRUE))
        if (inherits(ram_now, "try-error") || length(ram_now) == 0) return("Cannot get it")
        return(as.numeric(ram_now))
    }
    ramString <- function(ram_now){
        if (is.character(ram_now)) return(ram_now)
        return(paste0(round(ram_now[1]/1024, 1), "G/", round(ram_now[2]/1024, 1), "G"))
    }

    diskString <- function(){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        app_path <- if(isFALSE(spsOption("app_path")))  getwd() else spsOption("app_path")

        disk_now <- try(system(paste0("du -sh ", app_path ," | awk '{print $1}'"),
                               intern = TRUE, timeout = 1))
        if (inherits(disk_now, "try-error") || length(disk_now) == 0) return("Cannot get it")
        return(disk_now)
    }

    getTemp <- function(){
        if(Sys.info()[['sysname']] != "Linux") return("Only on Linux")
        temp_now <- try(system('cat /sys/class/thermal/thermal_zone*/temp', intern = TRUE))
        if (inherits(temp_now, "try-error") || length(temp_now) == 0) return("Cannot get it")
        temp_type <- try(system('cat /sys/class/thermal/thermal_zone*/type', intern = TRUE))
        if (inherits(temp_type, "try-error") || length(temp_type) == 0) return("Cannot get it")
        tibble::tibble(
            type = c("average", temp_type),
            temp = c(round(mean(as.numeric(temp_now))/1000, 2), as.numeric(temp_now)/1000))
    }

    tempString <- function(temp_now){
        if (is.character(temp_now)) return(temp_now)
        return(HTML(paste0(temp_now[1, 2], "&#176;C")))
    }
    module <- function(input, output, session){
        ns <- session$ns
        observe({
            output$uptime <- shinydashboard::renderInfoBox(
                shinydashboard::infoBox(
                    "App up time", timeDiffString(), icon = icon("clock"),
                    color = "blue"
                )
            )
            invalidateLater(60000)
        })
        observe({
            output$cpu_now <- shinydashboard::renderInfoBox(
                shinydashboard::infoBox(
                    "CPU usage", cpuString(), icon = icon("microchip"),
                    color = "blue"
                )
            )
            output$ram_now <- shinydashboard::renderInfoBox(
                shinydashboard::infoBox(
                    "RAM usage", ramString(), icon = icon("memory"),
                    color = "blue"
                )
            )
            output$disk_now <- shinydashboard::renderInfoBox(
                shinydashboard::infoBox(
                    "Current app size", diskString(), icon = icon("save"),
                    color = "blue"
                )
            )
            invalidateLater(10000)
        })
    }
    moduleServer(id, module)
}
