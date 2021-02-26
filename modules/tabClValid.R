#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski & Marc-Antoine Jacques
#
# This module is a tab for hierarchical clustering (base R hclust + dist)

helpText.clValid = c(alertClValidNAsPresent = paste0("NAs present. The selected distance measure will work, ",
                                              "however PCA will not be avaliable."),
                     alertClValidNAsPresentDTW = paste0("NAs present. DTW distance measure will NOT work."),
                    alLearnMore = paste0("<p><a href=http://www.sthda.com/english/wiki/print.php?id=241 title=\"External link\">Clustering</a> ",
                                         "is an <b>unsupervised</b> machine learning method for partitioning ",
                                         "dataset into a set of groups called clusters. The procedure will return clusters ",
                                         "even if the data <b>does not</b> contain any! ",
                                         "Therefore, it’s necessary to ",
                                         "assess clustering tendency before the analysis, and ",
                                         "validate the quality of the result after clustering.<p>"
                                         ),
                    alLearnMoreRel = paste0("<p>Determine the optimal number of clusters by inspecting ",
                                            "the average silhouette width and the total within cluster sum of squares (WSS) ",
                                            "for a range of cluster numbers.</p>", 
                                            "<p><b>Silhouette analysis</b> first computes how close each trajectory is with others in the cluster it is assigned to, ",
                                            "this is then compared to closeness with trajectories in other clusters. ",
                                            "Larger average silhouette widths usually indicate better clustering. To make sure averaging does not hide a locally bad",
                                            "clustering, this should be inspected along with the silhouette plot in the \"Internal\" tab.<p>",
                                            "<p><b>WSS</b> evaluates the compactness of clusters. ",
                                            "Compact clusters achieve low WSS values. ",
                                            "Look for the <i>elbow</i> in the plot of WSS as function of cluster numbers.</p>"),
                    alLearnMoreInt = paste0("<p>Evaluate the goodness of a clustering structure by inspecting ",
                                            "principal components, the dendrogram, ",
                                            "and the silhouette for a given number of clusters.</p>",
                                            "<p><b>Principal components:</b> Each point in the scatter plot corresponds to a single time series in the first 2 PCs space. ",
                                            "Points are coloured by cluster numbers. Compact, well separated clusters ",
                                            "indicate good partitioning. The percentage of total variance carried by each PC is indicated.</p>",
                                            "<p><b>Dendrogram:</b> The height of branches indicates how well clusters are separated.</p>",
                                            "<p><b>Silhouette plot:</b> The plot indicates for each series whether it is on average closer to series within its cluster ",
                                            "or to series in other clusters. Each bar represents the <a href=https://en.wikipedia.org/wiki/Silhouette_(clustering) title=\"External link\">silhouette score</a> ",
                                            "(Si) for one series. The height of the bars varies ",
                                            "between 1 (the series is much closer to series in its cluster) and -1 (the series is much closer to series in an other cluster). ",
                                            "Hence, large positive values of Si are usually associated with better clustering, while negative values are associated with worse clustering.")
                    )


# UI ----
clustValidUI <- function(id, label = "Validation") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Cluster validation using ",
      a("factoextra", 
        href="https://cran.r-project.org/web/packages/factoextra/",
        title="External link",
        target = "_blank")
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
                              "Canberra"  = "canberra",
                              "DTW"       = "DTW"),
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
               withSpinner(plotOutput(ns('outPlotTree'))),
               br(),
               withSpinner(plotOutput(ns('outPlotSilhForCut'))),
               br(),
               withSpinner(plotOutput(ns('outPlotClPCA')))
      )
    )
  )
}

# SERVER ----
clustValid <- function(input, output, session, in.dataWide) {

  ns = session$ns
  
  # Return the number of clusters from the slider 
  # and delay by a constant in milliseconds defined in auxfunc.R
  returnNclust = reactive({
    return(input$slClValidNclust)
  }) %>% debounce(MILLIS)
  
  # Return max number of clusters from the slider 
  # and delay by a constant in milliseconds defined in auxfunc.R
  returnMaxNclust = reactive({
    return(input$slClValidMaxClust)
  }) %>% debounce(MILLIS)
  
  # calculate distance matrix for further clustering
  # time series arranged in rows with columns corresponding to time points
  calcDist <- reactive({
    cat(file = stderr(), 'clustValid:calcDist \n')
    
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
  
  # calculate dendrogram for a chosen number of clusters and the linkage method
  calcDendCut = reactive({
    cat(file = stderr(), 'clustValid:calcDendCut \n')
    
    loc.dist = calcDist()
    
    if (is.null(loc.dist)) {
      return(NULL)
    }
    
    return(LOChcut(x = loc.dist,
                   k = returnNclust(),
                   hc_func = "hclust",
                   hc_method = input$selectLinkage,
                   hc_metric = input$selectDiss
                   ))    
  })
  
  # Plotting ----
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  
  # plot average silhouette
  plotSilhAvg <- function() {
    cat(file = stderr(), 'clustValid:plotSilhAvg: in\n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlotRel

    # Check if required data exists
    # Thanks to isolate all mods in the left panel are delayed 
    # until clicking the Plot button
    loc.dist = shiny::isolate(calcDist())
    
    shiny::validate(
      shiny::need(!is.null(loc.dist), "Nothing to plot. Load data first!"),
      shiny::need(returnMaxNclust() <  nrow(loc.dist), "Maximum number of clusters to conisder should be smaller than the number of time series.")
    )    

    loc.p = LOCnbclust(loc.dist,
                                     method = "silhouette",
                                     k.max = returnMaxNclust(),
                                     hc_metric = input$selectDiss,
                                     hc_method = input$selectLinkage) +
      xlab("Number of clusters") +
      ylab("Average silhouette width") +
      ggtitle("Average silhouette width for different cluster numbers") +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND)
    return(loc.p)
  }

  # plot Ws
  plotWss <- function() {
    cat(file = stderr(), 'clustValid:plotWss: in\n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlotRel
    
    # Check if required data exists
    # Thanks to isolate all mods in the left panel are delayed 
    # until clicking the Plot button
    loc.dist = isolate(calcDist())
    
    shiny::validate(
      shiny::need(!is.null(loc.dist), "Nothing to plot. Load data first!"),
      shiny::need(returnMaxNclust() <  nrow(loc.dist), "Maximum number of clusters to conisder should be smaller than the number of time series.")
    )    
    
    loc.p = LOCnbclust(loc.dist,
                                     method = "wss",
                                     k.max = returnMaxNclust(),
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
  
  # PCA visualization of partitioning methods 
  plotClPCA <- function() {
    cat(file = stderr(), 'clustValid:plotClPCA: in\n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlotInt
    
    # until clicking the Plot button
    loc.part = calcDendCut()
    loc.dm = in.dataWide()
    
    shiny::validate(
      shiny::need(!is.null(loc.part), "Nothing to plot. Load data first!"),
      shiny::need(!is.null(loc.dm),   "Nothing to plot. Load data first!"),
      shiny::need(sum(is.na(loc.dm)), "Cannot calculate PCA in the presence of missing data and/or NAs.")
    )    
    
    if (sum(is.na(loc.dm)) > 0)
      return(NULL)
    
    # The tableau "Color Blind" palette has only 10 colours; 
    # change to "Tableau 20" if more clusters requested
    loc.pal = ifelse(returnNclust() <= 10, "Color Blind", "Tableau 20")
    loc.col = ggthemes::tableau_color_pal(loc.pal)(n = returnNclust())
    
    loc.p = factoextra::fviz_cluster(loc.part, 
                                     data = loc.dm,
                                     geom = "point",
                                     elipse.type = "convex", 
                                     main = "Principal components"
                                     )+
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND) +
      scale_fill_manual(values = loc.col) +
      scale_colour_manual(values = loc.col)

    
    # Retrieve association of cluster and colours and use it for dendrogram for color matching between dend, silhouette and PCA plot
    temp = ggplot_build(loc.p)
    map_individual = as.data.table(temp$data[[1]][, c("colour", "shape")])
    map_cluster = map_individual[, .SD[1], by = shape]
    map_cluster[, cluster := 1:nrow(map_cluster)]

    return(list(plot = loc.p, mapping_individual = map_individual, mapping_cluster = map_cluster))
  }
  
  
  # plot dendrogram tree
  plotTree <- function() {
    cat(file = stderr(), 'clustValid:plotTree: in\n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlotInt
    
    # Check if required data exists
    loc.part = calcDendCut()
    
    # Rerun the PCA plot to obtain clour mapping of clusters in PCA and silhouette plot and match it with dendrogram colors.
    loc.map = plotClPCA()
    
    shiny::validate(
      shiny::need(!is.null(loc.part), "Nothing to plot. Load data first!"),
      shiny::need(!is.null(loc.map),  "Cannot assign colours to clusters. Possible NAs in the dataset!")
    )    
    
    # Determine cluster order of occurence from left to right in the dendrogram
    # This is necessary because fviz_dend colors clusters from left to right,
    # whereas fviz_silhouette and fviz_cluster use the order of cluster first occurence in the list of individuals.
    loc.mapClus = loc.map$mapping_cluster
    ord.clusDend = unique(loc.part$cluster[loc.part$order])
    col.clusDend = loc.mapClus[, colour][ord.clusDend]
        
    loc.p = factoextra::fviz_dend(loc.part,
                                  k = returnNclust(),
                                  k_colors = col.clusDend,
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
  
  
  # plot silhouettes for a particular dendrogram cut
  plotSilhForCut <- function() {
    cat(file = stderr(), 'clustValid:plotSilhForCut: in\n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlotInt
    
    # until clicking the Plot button
    loc.part = calcDendCut()
    shiny::validate(
      shiny::need(!is.null(loc.part), "Nothing to plot. Load data first!")
    )    
    
    loc.pal = ifelse(returnNclust() <= 10, "Color Blind", "Tableau 20")
    loc.col = ggthemes::tableau_color_pal(loc.pal)(n = returnNclust())
    
    loc.p = factoextra::fviz_silhouette(loc.part, 
                                        print.summary = FALSE, 
                                        main = "Silhouette") +
      xlab("Time series") +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = loc.col) +
      scale_colour_manual(values = loc.col)
    
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
    loc.p = loc.p$plot
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


