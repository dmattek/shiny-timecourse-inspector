#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for hierarchical clustering (base R hclust + dist)

helpText.clValid = c(alertClValidNAsPresent = paste0("NAs present. The selected distance measure will work, ",
                                              "however caution is recommended. Consider interpolation of NAs and missing data in the left panel."),
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
                    alLearnMoreInt = paste0("<p>Evaluate the goodness of a clustering structure by inspecting ",
                                            "principle components, the dendrogram, ",
                                            "and the silhouette for a given number of clusters.</p>",
                                            "<p>Each point in the scatter plot of 2 principle components corresponds to a single time series. ",
                                            "Points are coloured by cluster numbers. Compact, well separated clusters ",
                                            "indicate good partitioning.</p>",
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

      column(4,
             selectInput(
               ns("selectDiss"),
               label = ("Dissimilarity measure"),
               choices = list("Euclidean" = "euclidean",
                              "Manhattan" = "manhattan",
                              "Maximum"   = "maximum",
                              "Canberra"  = "canberra"),
               selected = "euclidean"
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
               selected = "average"
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
                          'Number of clusters to evaluate',
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
               withSpinner(plotOutput(ns('outPlotClPCA'))),
               br(),
               withSpinner(plotOutput(ns('outPlotTree'))),
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
    print(sum(is.na(loc.dm)))
    if(sum(is.na(loc.dm)) > 0) {
        createAlert(session, "alertAnchorClValidNAsPresent", "alertClValidNAsPresent", title = "Warning",
                    content = helpText.clValid[["alertClValidNAsPresent"]], 
                    append = FALSE, 
                    style = "warning")
    } else {
      closeAlert(session, 'alertClValidNAsPresent')
    }
    
    # calculate distance matrix
    
    return(dist(loc.dm, method = input$selectPlotHierDiss))
  })
  
  
  calcDendCut = reactive({
    cat(file = stderr(), 'clustValid:calcDendCut \n')
    
    loc.dm = returnDMwithChecks()
    
    if (is.null(loc.dm)) {
      return(NULL)
    }
    
    return(factoextra::eclust(x = loc.dm, 
                              FUNcluster = "hclust",
                              k = input$slClValidNclust, 
                              hc_method = input$selectLinkage, 
                              hc_metric = input$selectDiss,
                              graph = FALSE))
  })
  
  # Return a matrix with time series in wide format
  # If data contains NAs (from explicit NAs or due to missing time points, 
  # or due to missing time points after outlier removal),
  # some warnings are thrown. E.g. DTW cannot caluclate distance if NAs are present.
  returnDMwithChecks = reactive({
    cat(file = stderr(), 'clustValid:returnDMwithChecks \n')
    
    loc.dm = in.data4clust()
    
    if (is.null(loc.dm)) {
      return(NULL)
    }
    
    # Throw some warnings if NAs present in the dataset.
    # DTW cannot compute distance when NA's are preset.
    # Other distance measures can be calculated but caution is required with interpretation.
    print(sum(is.na(loc.dm)))
    
    if(sum(is.na(loc.dm)) > 0) {
        createAlert(session, "alertAnchorClValidNAsPresent", "alertClValidNAsPresent", 
                    title = "Warning",
                    content = helpText.clValid[["alertClValidNAsPresent"]], 
                    append = FALSE, 
                    style = "warning")
    } else {
      closeAlert(session, 'alertClValidNAsPresent')
    }
    
    return(loc.dm)
  })
  
  # Plotting ----
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  
  # plot average silhouette
  plotSilhAvg <- function() {

    locBut = input$butPlotRel
    if (locBut == 0) {
      cat(file = stderr(), 'plotSilhAvg: Go button not pressed\n')
      
      return(NULL)
    }
    
    loc.dm = returnDMwithChecks()
    if (is.null(loc.dm)) {
      return(NULL)
    }
    
    loc.p = factoextra::fviz_nbclust(loc.dm,
                                     hcut, 
                                     method = "silhouette",
                                     k.max = input$slClValidMaxClust,
                                     hc_metric = input$selectDiss,
                                     hc_method = input$selectLinkage) +
      xlab("Number of clusters") +
      ylab("Average silhouette width") +
      ggtitle("Optimal number of clusters from silhouette analysis") +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND)
    return(loc.p)
  }

  # plot Ws
  plotWss <- function() {
    
    locBut = input$butPlotRel
    if (locBut == 0) {
      cat(file = stderr(), 'plotWss: Go button not pressed\n')
      
      return(NULL)
    }
    
    loc.dm = returnDMwithChecks()
    if (is.null(loc.dm)) {
      return(NULL)
    }
    
    loc.p = factoextra::fviz_nbclust(loc.dm,
                                     hcut, 
                                     method = "wss",
                                     k.max = input$slClValidMaxClust,
                                     hc_metric = input$selectDiss,
                                     hc_method = input$selectLinkage) +
      xlab("Number of clusters") +
      ylab("Total within cluster sum of squares") +
      ggtitle("Within cluster sum of squares for different cluster numbers") +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND)
    
    return(loc.p)
  }

  # plot dendrogram tree
  plotTree <- function() {
    
    locBut = input$butPlotInt
    if (locBut == 0) {
      cat(file = stderr(), 'plotTree: Go button not pressed\n')
      
      return(NULL)
    }
    
    loc.part = calcDendCut()
    if (is.null(loc.part)) {
      return(NULL)
    }
    
    loc.p = factoextra::fviz_dend(loc.part, 
                                  show_labels = F,
                                  rect = T,
                                  xlab = "Time series", 
                                  main = "Dendrogram") +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND)
    
    return(loc.p)
  }
  
  
  # PCA visualization of partitioning methods 
  plotClPCA <- function() {
    
    locBut = input$butPlotInt
    if (locBut == 0) {
      cat(file = stderr(), 'plotClPCA: Go button not pressed\n')
      
      return(NULL)
    }
    
    loc.part = calcDendCut()
    if (is.null(loc.part)) {
      return(NULL)
    }
    
    loc.p = factoextra::fviz_cluster(loc.part, 
                                     geom = "point",
                                     elipse.type = "norm", 
                                     main = "Principle components"
                                     )
    
    return(loc.p)
  }
  
  # plot silhouetts for a particular dendrogram cut
  plotSilhForCut <- function() {
    
    locBut = input$butPlotInt
    if (locBut == 0) {
      cat(file = stderr(), 'plotSilhForCut: Go button not pressed\n')
      
      return(NULL)
    }
    
    loc.part = calcDendCut()
    if (is.null(loc.part)) {
      return(NULL)
    }
    
    loc.p = factoextra::fviz_silhouette(loc.part, 
                                        print.summary = FALSE, 
                                        main = "Silhouette") +
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
    loc.p = plotSilhAvg()
    if(is.null(loc.p))
      return(NULL)
    
    return(loc.p)
  })

  
  # Display wss
  output$outPlotWss <- renderPlot({
    loc.p = plotWss()
    if(is.null(loc.p))
      return(NULL)
    
    return(loc.p)
  })
  
  # Display PCA of clustering
  output$outPlotClPCA <- renderPlot({
    # This is required to avoid
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    # if (names(dev.cur()) != "null device")
    #   dev.off()
    # pdf(NULL)
    
    loc.p = plotClPCA()
    if(is.null(loc.p))
      return(NULL)
    
    return(loc.p)
  })
  
  # Display tree
  output$outPlotTree <- renderPlot({
    # This is required to avoid
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    # if (names(dev.cur()) != "null device")
    #   dev.off()
    # pdf(NULL)
    
    loc.p = plotTree()
    if(is.null(loc.p))
      return(NULL)
    
    return(loc.p)
  })
  
  # Display silhouette for a dendrogram cut
  output$outPlotSilhForCut <- renderPlot({
    # This is required to avoid
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    # if (names(dev.cur()) != "null device")
    #   dev.off()
    # pdf(NULL)
    
    loc.p = plotSilhForCut()
    if(is.null(loc.p))
      return(NULL)
    
    return(loc.p)
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


