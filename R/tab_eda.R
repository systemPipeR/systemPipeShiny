## UI
edaUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Exploratory Data Analysis"),
    fluidRow(
      boxPlus(title = "Specifications", closable = FALSE,
              radioButtons(ns("norm"), "Normalization method", choices = c( "rlog", "vst"))),
      boxPlus(title = "Plot Types",closable = F,
              checkboxInput(ns("PCA"), "PCA"),
              checkboxInput(ns("GLM"), "GLM-PCA"),
              checkboxInput(ns("TSNE"), "t-SNE"),
              checkboxInput(ns("MDS"), "MDS"),
              checkboxInput(ns("HCLUST"), "Hierarchical Clustering"),
              checkboxInput(ns("HEAT"), "Heat Map of Hierarchical Clustering"))
    ),
    fluidRow(title = "Plot Output",
             uiOutput(ns("PCA")),
             uiOutput(ns("GLM")),
             uiOutput(ns("TSNE")),
             uiOutput(ns("MDS")),
             uiOutput(ns("HCLUST")),
             uiOutput(ns("HMAP"))
    )
  )
}

## server

edaServer <- function(input, output, session, shared){
  ns <- session$ns
  observe({
    
    if (!is.null(shared$count_list)){ 
      ## GLM 
      output$GLM <- renderUI({
        if (input$GLM == T) {
          output$GLM_out <- renderPlotly({
            run_GLM(countDF = count_list$countDF, targets = count_list$targets, colData = count_list$colData)
          })
        } else { return(NULL) }
        boxPlus(plotlyOutput(ns("GLM_out")))
      })
      
      ## PCA
      output$PCA <- renderUI({
        if (input$PCA == T) {
          output$PCA_out <- renderPlotly({
            run_PCA(countDF = count_list$countDF, targets = count_list$targets, colData = count_list$colData, method = input$norm)
          })
        } else { return(NULL) }
        boxPlus(plotlyOutput(ns("PCA_out")))
      })
      
      ## TSNE
      output$TSNE <- renderUI({
        if (input$TSNE == T) {
          output$TSNE_out <- renderPlotly({
            run_TSNE(countDF = count_list$countDF, targets = count_list$targets)
          })
        } else { return(NULL) }
        boxPlus(plotlyOutput(ns("TSNE_out")))
      })
      
      ## MDS
      output$MDS <- renderUI({
        if (input$MDS == T) {
          output$MDS_out <- renderPlotly({
            run_MDS(countDF = count_list$countDF, targets = count_list$targets, colData = count_list$colData, method = input$norm)
          })
        } else { return(NULL) }
        boxPlus(plotlyOutput(ns("MDS_out")))
      })
      
      ## Hierarchial Clustering Dendrogram
      output$HCLUST <- renderUI({
        if (input$HCLUST == T) {
          output$HCLUST_out <- renderPlot({
            run_CLUST(countDF = count_list$countDF, targets = count_list$targets, colData = count_list$colData, method = input$norm)
          })
        } else { return(NULL) }
        boxPlus(plotOutput(ns("HCLUST_out")))
      })
      
      ## Heat Map
      output$HMAP <- renderUI({
        if (input$HEAT == T) {
          output$HEAT_out <- renderPlotly({
            run_HEAT(countDF = count_list$countDF, targets = count_list$targets, colData = count_list$colData, method = input$norm)
          })
        } else { return(NULL) }
        boxPlus(plotlyOutput(ns("HEAT_out")))
      })
      
    }
    
  })
  
} 
