## UI 
plot1UI <- function(id){
    ns <- NS(id)
    tabPanel(title = "Plot 1", 
             h2("Make a xx plot"),
             fluidRow(
                 radioGroupButtons(
                     inputId = ns("plot_source"), label = "Choose your plot file source:", 
                     selected = "upload",
                     choiceNames = c("Upload", "Example"), 
                     choiceValues = c("upload", "eg"),
                     justified = TRUE, status = "primary",
                     checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon(""))
                 ),
                 fileInput(
                     ns("df_upload"), "If upload, choose your df file here:",
                     multiple = FALSE,
                     accept = c("txt", "csv", "tsv"),
                     placeholder = "Choose your df file path",
                 ),
                 column(width = 12, style = "padding-left: 0;",
                        downloadButton(ns("down_config"), "Save"),
                        actionButton(ns("render"),
                                     label = "Render the plot", 
                                     icon("paper-plane"))
                 )
             ),
    )
}

## server
plot1Server <- function(input, output, session, shared){
    ns <- session$ns
    
}
