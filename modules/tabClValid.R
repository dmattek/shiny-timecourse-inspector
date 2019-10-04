#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for hierarchical clustering (base R hclust + dist)

helpText.clValid = c(alertNAsPresentDTW = paste0("NAs present. DTW cannot calculate the distance. ",
                                                "NAs and missing data can be interpolated by activating the option in the left panel. ",
                                                "If outlier points were removed, activate \"Interpolate gaps\" or ",
                                                "decrease the threshold for maximum allowed gap length. ",
                                                "The latter will result in entire trajectories with outliers being removed."),
                    alertNAsPresent = paste0("NAs present. The selected distance measure will work with missing data, ",
                                             "however caution is recommended. NAs and missing data can be interpolated by activating the option in the left panel. ",
                                             "If outlier points were removed, activate \"Interpolate gaps\" or ",
                                             "decrease the threshold for maximum allowed gap length. ",
                                             "The latter will result in entire trajectories with outliers being removed."),
                    alLearnMore = paste0("<p><a href=http://www.sthda.com/english/wiki/print.php?id=241>Clustering</a> is an <b>unsupervised</b> machine learning method for partitioning ",
                                         "dataset into a set of groups or clusters. The procedure will return clusters ",
                                         "even if the data <b>does not</b> contain any! ",
                                         "Therefore, it’s necessary to ",
                                         "assess clustering tendency before the analysis, and ",
                                         "validate the quality of the result after clustering.<p>",
                                         "<p><b>Relative validation</b>, evaluates the clustering structure ",
                                         "by varying different parameter values for the same algorithm ",
                                         "(e.g. varying the number of clusters <i>k</i>). Typically used for ",
                                         "determining the optimal number of clusters.</p>",
                                         "<p><b>Internal validation</b>, uses the internal information of the clustering process ",
                                         "to evaluate the goodness of a clustering structure without reference to external information. ",
                                         "It can be also used for estimating the number of clusters and the appropriate clustering algorithm ",
                                         "without any external data.</p>",
                                         "<p><b>External validation</b>, compares the results of a cluster analysis ",
                                         "to an externally known result, such as externally provided class labels. ",
                                         "Since we know the “true” cluster number in advance, ",
                                         "this approach is mainly used for selecting the right clustering algorithm for a specific dataset.</p>",
                                         "<p><b>Stability validation</b>, is a special version of internal validation. ",
                                         "It evaluates the consistency of a clustering result by comparing it with the clusters obtained ",
                                         "after each column is removed, one at a time.</p>"),
                    outPlotWss = "Weighted squared sum...",
                    outPlotSilhAvg = "Average...",
                    outPlotTree = "Dendrogram...",
                    outPlotSilhForCut = "Silhouette plot at dendrogram cut...")


# UI ----
clustValidUI <- function(id, label = "Validation") {
  ns <- NS(id)
  
  tagList(
    h4('Cluster validation'),
    actionLink(ns("alLearnMore"), "Learn more"),
    br(),
    br(),
    fluidRow(
      column(3,
             selectInput(
               ns("selectDiss"),
               label = ("Dissimilarity measure"),
               choices = list("Euclidean" = "euclidean",
                              "Manhattan" = "manhattan",
                              "Maximum"   = "maximum",
                              "Canberra"  = "canberra",
                              "DTW"       = "DTW"),
               selected = 1
             ),
             bsAlert("alertAnchorClHierNAsPresent")
             ),
      column(3,
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
               selected = 2
               )
             )
    ),
    
    br(),
    tabsetPanel(
      tabPanel("Relative",
               br(),
               fluidRow(
                 column(2, 
                        actionButton(ns('butPlotRel'), 'Validate!')
                        ),
                 column(6,
                        sliderInput(
                          ns('slClValidMaxClust'),
                          'Maximum number of clusters to validate',
                          min = 2,
                          max = 20,
                          value = 10,
                          step = 1,
                          ticks = TRUE,
                          round = TRUE
                        )
                        )
               ),
               br(),
               withSpinner(plotOutput(ns('outPlotSilhAvg'))),
               bsTooltip(ns('outPlotSilhAvg'), helpText.clValid[["outPlotSilhAvg"]], 
                         placement = "top", trigger = "hover", options = NULL),
               br(),
               withSpinner(plotOutput(ns('outPlotWss'))),
               bsTooltip(ns('outPlotWss'), helpText.clValid[["outPlotWss"]], 
                         placement = "top", trigger = "hover", options = NULL)
               
      ),
      tabPanel("Internal",
               br(),
               fluidRow(
                 column(2,
                        actionButton(ns('butPlotInt'), 'Validate!')
                        ),
                 column(6,
                        sliderInput(
                          ns('slClValidNclust'),
                          'Number of dendrogram branches to cut',
                          min = 2,
                          max = 20,
                          value = 1,
                          step = 1,
                          ticks = TRUE,
                          round = TRUE
                        )
                        )
               ),
               br(),
               withSpinner(plotOutput(ns('outPlotTree'))),
               bsTooltip(ns('outPlotTree'), helpText.clValid[["outPlotTree"]], 
                         placement = "top", trigger = "hover", options = NULL),
               br(),
               withSpinner(plotOutput(ns('outPlotSilhForCut'))),
               bsTooltip(ns('outPlotSilhForCut'), helpText.clValid[["outPlotSilhForCut"]], 
                         placement = "top", trigger = "hover", options = NULL)
      )
    )
  )
}

# SERVER ----
clustValid <- function(input, output, session, in.data4clust) {

  ns = session$ns
  
  # calculate distance matrix for further clustering
  # time series arranged in rows with columns corresponding to time points
  userFitDistHier <- reactive({
    cat(file = stderr(), 'clustValid:userFitDistHier \n')
    
    loc.dm = in.data4clust()
    
    if (is.null(loc.dm)) {
      return(NULL)
    }
    
    # Throw some warnings if NAs present in the dataset.
    # DTW cannot compute distance when NA's are preset.
    # Other distance measures can be calculated but caution is required with interpretation.
    if(sum(is.na(loc.dm)) > 0) {
      if (input$selectPlotHierDiss == "DTW") {
        createAlert(session, "alertAnchorClHierNAsPresent", "alertNAsPresentDTW", title = "Error",
                    content = helpText.clHier[["alertNAsPresentDTW"]], 
                    append = FALSE,
                    style = "danger")
        return(NULL)
      } else {
        createAlert(session, "alertAnchorClHierNAsPresent", "alertNAsPresent", title = "Warning",
                    content = helpText.clHier[["alertNAsPresent"]], 
                    append = FALSE, 
                    style = "warning")
        closeAlert(session, 'alertNAsPresentDTW')
      }
    } else {
      closeAlert(session, 'alertNAsPresentDTW')
      closeAlert(session, 'alertNAsPresent')
    }
    
    # calculate distance matrix
    
    return(dist(loc.dm, method = input$selectPlotHierDiss))
  })
  
  
  calcDendCut = reactive({
    cat(file = stderr(), 'clustValid:calcDendCut \n')
    
    loc.dmdist = userFitDistHier()
    
    if (is.null(loc.dmdist)) {
      return(NULL)
    }
    
    return(LOChcut(x = loc.dmdist, 
                   k = input$slClValidNclust, 
                   hc_func = "hclust", 
                   hc_method = input$selectLinkage, hc_metric = input$selectDiss))
  })
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  
  # plot average silhouette
  plotSilhAvg <- function() {

    loc.dmdist = userFitDistHier()
    
    if (is.null(loc.dmdist)) {
      return(NULL)
    }
    
    loc.p = LOCnbclust(x = loc.dmdist, 
                       FUNcluster = LOChcut,  
                       method = "silhouette", 
                       verbose = TRUE, 
                       k.max = input$slClValidMaxClust,
                       hc_metric = input$selectDiss,
                       hc_method = input$selectLinkage)
    return(loc.p)
  }

  # plot Ws
  plotWss <- function() {
    
    loc.dmdist = userFitDistHier()
    
    if (is.null(loc.dmdist)) {
      return(NULL)
    }
    
    loc.p = LOCnbclust(x = loc.dmdist, 
                       FUNcluster = LOChcut,  
                       method = "wss", 
                       verbose = TRUE, 
                       k.max = input$slClValidMaxClust,
                       hc_metric = input$selectDiss,
                       hc_method = input$selectLinkage)
    
    return(loc.p)
  }
  

  # plot dendrogram tree
  plotTree <- function() {
    
    loc.dend = calcDendCut()
    
    if (is.null(loc.dend)) {
      return(NULL)
    }
    
    loc.p = factoextra::fviz_dend(x = loc.dend, k = input$slClValidNclust)
    
    return(loc.p)
  }
  
  # plot silhouetts for a particular dendrogram cut
  plotSilhForCut <- function() {
    
    loc.dmdist = userFitDistHier()
    loc.dend = LOChcut(x = loc.dmdist, 
                       k = input$slClValidNclust, 
                       hc_func = "hclust", 
                       hc_method = input$selectLinkage, hc_metric = input$selectDiss)
    
    if (is.null(loc.dend)) {
      return(NULL)
    }
    
    loc.p = factoextra::fviz_silhouette(sil.obj = loc.dend, print.summary = FALSE)
    
    return(loc.p)
  }
  
  # Display silhouette
  output$outPlotSilhAvg <- renderPlot({
    locBut = input$butPlotRel
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotSilhAvg: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotSilhAvg()
  })

  
  # Display wss
  output$outPlotWss <- renderPlot({
    locBut = input$butPlotRel
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotWss: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotWss()
  })
  
  # Display tree
  output$outPlotTree <- renderPlot({
    locBut = input$butPlotInt
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotTree: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotTree()
  })
  
  # Display silhouette for a dendrogram cut
  output$outPlotSilhForCut <- renderPlot({
    locBut = input$butPlotInt
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotSilhForCut: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotSilhForCut()
  })
  
  # Pop-overs ----
  addPopover(session, 
             ns("alLearnMore"),
             title = "Classes of cluster validation",
             content = helpText.clValid[["alLearnMore"]],
             trigger = "click")
}


