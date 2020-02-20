## UI
degUI <- function(id){
  ns <- NS(id)
  tagList(
    h2("Differentially Expressed Genes Analysis"),
    fluidRow(
      boxPlus(title = "Specifications", width = 12, closable = F,
              numericInput(ns("Fold"), "Fold:", value = 2), 
              numericInput(ns("FDR"), "FDR:", value = 10),
              numericInput(ns("cmpset"), "Comparison set:", value = 2), ##change this to number of cmp
              uiOutput(ns("comp_names")),
              checkboxInput(ns("DESeq2"), "DESeq2"),
              checkboxInput(ns("edgeR"), "edgeR"))
      
    ),
    fluidRow(title = "Plot Output",
             uiOutput(ns("edgeR")),
             uiOutput(ns("DESeq2"))
             #textOutput(ns("text"))
             
    )
  )
}

## server
degServer <- function(input, output, session, shared){
  ns <- session$ns
  observe({
    #output$text <- renderText(input$comp)
    if (!is.null(shared$count_list)){
      comparison <- compare.names(count_list$cmp, input$cmpset)
      output$comp_names <- renderUI({
        selectInput(ns("comp"), "Choose two samples to compare:", choices = c(comparison))
      })
      ## edgeR
      output$edgeR <- renderUI({
        if (input$edgeR == T) {
          output$edgeR_out <- renderPlotly({
            deg_edgeR(countDF = count_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = count_list$targets, cmp = count_list$cmp, cmpset = input$cmpset)
          })
          output$edgeR_v <- renderPlotly({
            DF <- deg_edgeR(countDF = count_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = count_list$targets, cmp = count_list$cmp, cmpset = input$cmpset, plot = F)
            run_volcano(DF = DF, Fold = input$Fold, FDR= input$FDR, comparison = input$comp) ##hardcode
          })
        } else { return(NULL) } 
        boxPlus(plotlyOutput(ns("edgeR_out")),
                plotlyOutput(ns("edgeR_v")))
        
      })
      # } else if (all(!input$which_ct, !is.null(input$count_targets), !is.null(input$count_input))) {
      #     ct_list <- load_count(targets_file = input$count_targets$datapath, count_table = input$count_input$datapath)
      #     output$edgeR <- renderUI({
      #       if (input$edgeR == T) {
      #         output$edgeR_out <- renderPlotly({
      #         deg_edgeR(countDF = ct_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = ct_list$targets, cmp = ct_list$cmp, cmpset = input$cmpset)
      #         })
      #       } else { return(NULL) } 
      #       boxPlus(plotlyOutput(ns("edgeR_out")))
      #       })
      ## DESeq2
      output$DESeq2 <- renderUI({
        if (input$DESeq2 == T) {
          output$DESeq2_out <- renderPlotly({
            deg_deseq2(countDF = count_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = count_list$targets, cmp = count_list$cmp, cmpset = input$cmpset, plot = T)
          })
          output$DESeq2_v <- renderPlotly({
            DF <- deg_deseq2(countDF = count_list$countDF, Fold = input$Fold, FDR= input$FDR, targets = count_list$targets, cmp = count_list$cmp, cmpset = input$cmpset, plot = F)
            run_volcano(DF = DF, Fold = input$Fold, FDR= input$FDR, comparison = input$comp) ##hardcode
          })
        } else { return(NULL) }
        boxPlus(plotlyOutput(ns("DESeq2_out")),
                plotlyOutput(ns("DESeq2_v")))
        
      })
    } else {return(NULL)}
  })  
}


compare.names <- function(cmp, cmpset) {
  mycmp <- cmp[[cmpset]]
  comparison <- vector()
  for (i in seq(along = mycmp[,1])) {
    comparison <- c(comparison,paste(mycmp[i, ], collapse = "-"))
  }
  return(comparison)
}








