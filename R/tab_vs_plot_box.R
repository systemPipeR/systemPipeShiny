## UI
plot_boxUI <- function(id){
    ns <- NS(id)
    tabPanel(title = "Box Plot",
             h2("Make a Box plot"),
             fluidRow(
                 actionButton(
                     ns("op_1"),
                     label = "Raw",
                     icon("cog")
                 ),
                 actionButton(
                     ns("op_2"),
                     label = "R-log",
                     icon("cog")
                 ),
                 actionButton(
                     ns("op_3"),
                     label = "VST",
                     icon("cog")
                 )
             ),
             fluidRow(
                 actionButton(ns("render"),
                              label = "Render the plot",
                              icon("paper-plane")),
             ),
             uiOutput(ns("plot_ui"))
    )
}

## server
plot_boxServer <- function(id, shared){
    module <- function(input, output, session){
        ns <- session$ns
        observeEvent(input$render, {
            countDF <<- data.frame(shared$count$df)
            colnames(countDF) <- countDF[1,]
            countDF <- countDF[-1,]
            rownames(countDF) <- countDF[,1]
            countDF <- countDF[,-1]
            filter <- c(Fold = 2, FDR = 10)
            mytitle <- paste("edgeR DEG Counts (", names(filter)[1], ": ", filter[1], " & " , names(filter)[2], ": ", filter[2], "%)", sep="")
            df_plot <- data.frame(Comparisons=rep(as.character(rownames(countDF)), 2), Counts=c(countDF$Counts_Up, countDF$Counts_Down), Type=rep(c("Up", "Down"), each=length(countDF[,1])))
            p <- ggplot(df_plot, aes(Comparisons, Counts, fill = Type)) + geom_bar(position="stack", stat="identity") + coord_flip() + theme(axis.text.y=element_text(angle=0, hjust=1)) + ggtitle(mytitle)
            output$plot_ui <- renderUI(
                plotlyOutput(ns("box"))
            )
            output$box <- renderPlotly({
                ggplotly(p)
            })
        })
    }
    moduleServer(id, module)
}
