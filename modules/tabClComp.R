#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski, Jacques Marc-Antoine
#
# This module is a tab for comparing the results of several hierarchical clusterings.
#

## Help text ----
#
#

# UI ----
tabClCompUI <- function(id, label = "Comparison"){
  ns <- NS(id)
  
  tagList(
    h4("Comparison of hierarchical clustering outputs."),
    br(),
    br(),
    fluidRow(
      
      column(4,
             selectInput(
               ns("selectDiss"),
               label = ("Dissimilarity measure"),
               choices = list("Euclidean" = "euclidean",
                              "Manhattan" = "manhattan",
                              "Maximum"   = "maximum",
                              "Canberra"  = "canberra",
                              "DTW"       = "DTW"),
               selected = "euclidean",
               multiple = FALSE
             ),
             bsAlert("alertAnchorClValidNAsPresent")
      ),
      column(4,
             selectInput(
               ns("selectLinkage"),
               label = ("Linkage method"),
               choices = list(
                 "Average"  = "average",
                 "Complete" = "complete",
                 "Single"   = "single",
                 "Centroid" = "centroid",
                 "Ward"     = "ward.D",
                 "Ward D2"  = "ward.D2",
                 "McQuitty" = "mcquitty"
               ),
               selected = c("average",
                            "complete",
                            "single",
                            "centroid",
                            "ward.D",
                            "ward.D2",
                            "mcquitty"),
               multiple = TRUE
             )
      ),
      column(4,
             selectInput(
               ns("selectCorr"),
               label = ("Correlation method"),
               choices = list("Cophenetic" = "cophenetic",
                              "Baker" = "baker",
                              "Common nodes" = "common_nodes",
                              "FM index" = "FM_index"
               ),
               selected = "cophenetic"
             )
      )
    ),
    
    br(),
    tabsetPanel(
      tabPanel("Correlation plot",
                br(),
                p("Visualize correlations of different outputs"),
               fluidRow(
                 column(2,
                        actionButton(ns('butPlotComp'), 'Compare!')
                 )
               ),
               br(),
               withSpinner(plotOutput(ns('outPlotHierCorr'))),
      )
    )
  )
}


# SERVER ----
tabClComp <- function(input, output, session, in.dataWide) {

  ns = session$ns
  
  # calculate distance matrix for further clustering
  # time series arranged in rows with columns corresponding to time points
  calcDist <- reactive({
    cat(file = stderr(), 'clustComp:calcDist \n')
    
    loc.dm = in.dataWide()
    
    if (is.null(loc.dm)) {
      return(NULL)
    }
    
    # Throw some warnings if NAs present in the dataset.
    # DTW cannot compute distance when NA's are preset.
    # Other distance measures can be calculated but caution is required with interpretation.
    # NAs in the wide format can result from explicit NAs in the measurment column or
    # from missing rows that cause NAs to appear when convertinf from long to wide (dcast)
    if(sum(is.na(loc.dm)) > 0) {
      if (input$selectDiss == "DTW") {
        createAlert(session, "alertAnchorClValidNAsPresent", "alertClValidNAsPresentDTW", title = "Error",
                    content = helpText.clValid[["alertClValidNAsPresentDTW"]], 
                    append = FALSE,
                    style = "danger")
        closeAlert(session, 'alertClValidNAsPresent')
        
        return(NULL)
        
      } else {
        createAlert(session, "alertAnchorClValidNAsPresent", "alertClValidNAsPresent", title = "Warning",
                    content = helpText.clValid[["alertClValidNAsPresent"]], 
                    append = FALSE, 
                    style = "warning")
        closeAlert(session, 'alertClValidNAsPresentDTW')
      }
    } else {
      closeAlert(session, 'alertClValidNAsPresentDTW')
      closeAlert(session, 'alertClValidNAsPresent')
    }    
    
    
    # calculate distance matrix
    return(proxy::dist(loc.dm, method = input$selectDiss))
  })
  
  
# Calculate dendrograms for a list of linkages and a given distance matrix
calcDendList = reactive({
  cat(file = stderr(), 'clustComp:calcDendList \n')
  
  loc.dist = calcDist()
  
  if (is.null(loc.dist)) {
    return(NULL)
  }
  
  return(LOChcList(x = loc.dist,
                 hc_method = input$selectLinkage
  ))
})
  
  # Plotting ----
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  
  #' Correlation plot between dendrograms
  #' 
  #' @param x list of dendrograms as returned by dendextend::dendlist
  #' @param cor_method the correlation method. See ?dendextend::cor.dendlist()
  #' 
  plothcListCorr <- function(){
    # function(x,
    #          cor_method = "cophenetic")
    # {
    cat(file = stderr(), 'clustComp:LOChcutListCorr \n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlotComp
    
    # Check if required data exists
    # Thanks to isolate all mods in the left panel are delayed 
    # until clicking the Plot button
    loc.dist = isolate(calcDist())
    shiny::validate(shiny::need(!is.null(loc.dist), "Nothing to plot. Load data first!"))

    loc.dendlist <- calcDendList()
    
    dendlist_cor <- dendextend::cor.dendlist(loc.dendlist, method = input$selectCorr)
    loc.p <- corrplot::corrplot.mixed(dendlist_cor, upper = "circle", lower = "number")
    }

  # Plot rendering ----
  output$outPlotHierCorr <- renderPlot({
    loc.p = plothcListCorr()
    if(is.null(loc.p))
      return(NULL)
    
    return(loc.p)
  })

}


