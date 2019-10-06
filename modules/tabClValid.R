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
                    alLearnMore = paste0("<p><a href=http://www.sthda.com/english/wiki/print.php?id=241 title=\"External link\">Clustering</a> ",
                                         "is an <b>unsupervised</b> machine learning method for partitioning ",
                                         "dataset into a set of groups or clusters. The procedure will return clusters ",
                                         "even if the data <b>does not</b> contain any! ",
                                         "Therefore, it’s necessary to ",
                                         "assess clustering tendency before the analysis, and ",
                                         "validate the quality of the result after clustering.<p>"
                                         ),
                    alLearnMoreRel = paste0("<p>Determine the optimal number of clusters by inspecting ",
                                            "the average silhouette width and the total within cluster sum of squares (WSS) ",
                                            "for a range of cluster numbers.</p>", 
                                            "<p><b>Silhouette analysis</b> estimates the average distance between clusters. ",
                                            "Larger silhouette widths indicate better.<p>",
                                            "<p><b>WSS</b> evaluates the compactness of clusters. ",
                                            "Compact clusters achieve low WSS values. ",
                                            "Look for the <i>knee</i> in the plot of WSS as function of cluster numbers.</p>"),
                    alLearnMoreInt = paste0("<p>Evaluate the goodness of a clustering structure by inspecting <b>the dendrogram</b> ",
                                            "and <b>the silhouette</b> for a given number of clusters.</p>",
                                            "<p>The height of dendrogram branches indicates how well clusters are separated.</p>",
                                            "<p>The silhouette plot displays how close each time series in one cluster ", 
                                            "is to time series in the neighboring clusters. ",
                                            "A large positive silhouette (Si) indicates time series that are well clustered.",
                                            "A negative Si indicates time series that are closer to ",
                                            "a neighboring cluster, and are placed in the wrong cluster.</p>")
                    )


# UI ----
clustValidUI <- function(id, label = "Validation") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Cluster validation using ",
      a("factoextra", 
        href="https://cran.r-project.org/web/packages/factoextra/",
        title="External link")
    ),
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
               p("Determine and visualise the optimal number of clusters. ",
                 actionLink(ns("alLearnMoreRel"), "Learn more")),
               fluidRow(
                 column(2, 
                        actionButton(ns('butPlotRel'), 'Validate!')
                        ),
                 column(6,
                        sliderInput(
                          ns('slClValidMaxClust'),
                          'Maximum number of clusters to consider',
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
               br(),
               withSpinner(plotOutput(ns('outPlotWss')))
               
      ),
      tabPanel("Internal",
               br(),
               p("Validate a given data partitioning. ",
                 actionLink(ns("alLearnMoreInt"), "Learn more")),
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
               br(),
               #withSpinner(plotOutput(ns('outPlotClPCA'))),
               br(),
               withSpinner(plotOutput(ns('outPlotSilhForCut')))
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
  
  # Plotting ----
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
                       hc_method = input$selectLinkage) +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND)
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
                       hc_method = input$selectLinkage) +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND)
    
    return(loc.p)
  }

  # plot dendrogram tree
  plotTree <- function() {
    
    loc.part = calcDendCut()
    
    if (is.null(loc.part)) {
      return(NULL)
    }
    
    loc.p = factoextra::fviz_dend(x = loc.part, 
                                  show_labels = F,
                                  rect = T,
                                  xlab = "Time series",
                                  k = input$slClValidNclust) +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND)
    
    return(loc.p)
  }
  
  
  # PCA visualization of partitioning methods 
  plotClPCA <- function() {
    
    loc.part = calcDendCut()
    
    if (is.null(loc.part)) {
      return(NULL)
    }
    
    loc.p = factoextra::fviz_cluster(object = loc.part, ellipse.type = "convex")
    
    return(loc.p)
  }
  
  # plot silhouetts for a particular dendrogram cut
  plotSilhForCut <- function() {
    
    loc.part = calcDendCut()
    
    if (is.null(loc.part)) {
      return(NULL)
    }
    
    loc.p = factoextra::fviz_silhouette(sil.obj = loc.part, 
                                        print.summary = FALSE) +
      xlab("Time series") +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND) +
      theme(axis.text.x = element_blank())
    
    return(loc.p)
  }
  
  # Plot rendering ----
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
  
  addPopover(session, 
             ns("alLearnMoreRel"),
             title = "Relative validation",
             content = helpText.clValid[["alLearnMoreRel"]],
             trigger = "click")
  
  addPopover(session, 
             ns("alLearnMoreInt"),
             title = "Internal validation",
             content = helpText.clValid[["alLearnMoreInt"]],
             trigger = "click")
}


