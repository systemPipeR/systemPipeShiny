## UI
degUI <- function(id){
  ns <- NS(id)
  tagList(
    h2("Differentially Expressed Genes Analysis"),
    fluidRow(
      boxPlus(title = "Specifications", width = 12, closable = F,
              #plot specifications
              numericInput(ns("Fold"), "Log Fold Change value for Cut Off:", value = 2), 
              numericInput(ns("FDR"), "False Discovery Rate (FDR) percent Cut Off:", value = 10),
              selectInput(ns("compnum"), "Choose which set of comparisons to use", choices = NULL, selected = NULL),
              # uiOutput(ns("comp_number")),
              uiOutput(ns("comp_names")),
              uiOutput(ns("venn_choices")),
              textOutput(ns("venn_error")),
              br(),
              h4("Analysis type:"),
              checkboxInput(ns("DESeq2"), "DESeq2"),
              checkboxInput(ns("edgeR"), "edgeR")
    )
    ),
    fluidRow(title = "Plot Output",
             uiOutput(ns("edgeR")),
             uiOutput(ns("DESeq2"))
            )
  )
  
}

degServer <- function(input, output, session, shared){
  ns <- session$ns
  select_compnum <- reactiveVal("")
  list_Value <- reactiveVal(1) 
  observe({
    if (!is.null(shared$count_list)){
      ##other plot specifications
      list <- c(seq(along = count_list$cmp))
      list_Value(list)
      comparison <- compare.names(count_list$cmp,as.numeric(input$compnum))
      output$comp_names <- renderUI({                ## which sample for volcano plot
        selectInput(ns("comp"), "Choose two samples to compare (Volcano Plot):", choices = c(comparison))
      })
      ## volcano plot with min 2 comparisons and max 5 comparison
      output$venn_choices <- renderUI({
        selectizeInput(ns("cmp_choices"),label = "Choose sample comparisons (Venn Plot)", choices = comparison, options= list(maxItems = 5), multiple = T)
      })
      ## edgeR analysis
      output$edgeR <- renderUI({
        if (input$edgeR == T) {
          if(!identical(input$compnum,numeric(0))){
          DF <- deg_edgeR(countDF = count_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = count_list$targets, cmp = count_list$cmp, cmpset = as.numeric(input$compnum), plot = F)
          output$edgeR_out <- renderPlotly({
            deg_edgeR(countDF = count_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = count_list$targets, cmp = count_list$cmp, cmpset = as.numeric(input$compnum))
          })
          output$edgeR_v <- renderPlotly({
            run_volcano(DF = DF, Fold = input$Fold, FDR= input$FDR, comparison = input$comp)
          })
          if (length(input$cmp_choices) < 2) {
            output$venn_error <- renderText({
              if (length(input$cmp_choices) < 2) {
              "Please choose at least 2 sample comparisons for the Venn plot" 
              } else {NULL}
            })
          } else {
            output$edgeR_venn <- renderPlot({
              deg_venn(DF = DF, Fold = input$Fold, FDR = input$FDR, cmp_list = input$cmp_choices)
            })
          }                                    
          }
          } else { return(NULL) }
        boxPlus(plotlyOutput(ns("edgeR_out")),
        plotlyOutput(ns("edgeR_v")),
        plotOutput(ns("edgeR_venn")))
      })
      ## DESeq2 analysis
      output$DESeq2 <- renderUI({
        if (input$DESeq2 == T) {
          DF <- deg_deseq2(countDF = count_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = count_list$targets, cmp = count_list$cmp, cmpset = as.numeric(input$compnum), plot = F)
          output$DESeq2_out <- renderPlotly({
            deg_deseq2(countDF = count_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = count_list$targets, cmp = count_list$cmp, cmpset = as.numeric(input$compnum), plot = T)
          })
          output$DESeq2_v <- renderPlotly({
            run_volcano(DF = DF, Fold = input$Fold, FDR= input$FDR, comparison = input$comp)
          })
          output$DESeq2_venn <- renderPlot({
            if (length(input$cmp_choices) < 2) {
             NULL
            } else {
            deg_venn(DF = DF, Fold = input$Fold, FDR = input$FDR, cmp_list = input$cmp_choices)
            }
            })
        } else { return(NULL) }
        boxPlus(plotlyOutput(ns("DESeq2_out")),
                plotlyOutput(ns("DESeq2_v")),
                plotOutput(ns("DESeq2_venn")))
      })
      updateSelectizeInput(session, 'cmp_choices', choices = comparison, server = TRUE)
    } else {return(NULL)}
})
  
observe({
      select_compnum(input$compnum) 
      updateSelectInput(session, "compnum", choices =list_Value(), selected = input$compnum)
})

}


compare.names <- function(cmp, cmpset) {
  if (identical(cmpset,numeric(0))){
    return(NULL)
  } else {
  mycmp <- cmp[[cmpset]]
  comparison <- vector()
  for (i in seq(along = mycmp[,1])) {
    comparison <- c(comparison,paste(mycmp[i, ], collapse = "-"))
  }
  return(comparison)
  }
}



