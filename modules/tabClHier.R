#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for hierarchical clustering (base R hclust + dist)

## Help text ----

helpText.clHier = c(alertNAsPresentClDTW = paste0("NAs (still) present. DTW cannot calculate the distance. ",
                                                  "If interpolation is active in the left panel, missing data can be due to removed outlier time points."),
                    alertNAsPresentCl = paste0("NAs (still) present, caution recommended. If interpolation is active in the left panel, ",
                                               "missing data can be due to removed outlier time points."),
                    alertNAsInDist = "NAs in distance matrix. Possible non-overlapping time series in the dataset.",
                    alertNAsPresentLong2WideConv = "Missing rows. Consider interpolation.",
                    alertDuplPresentCl = "Duplicate track IDs. Heatmap cannot be plotted correctly.",
                    alLearnMore = paste0("<p><a href=\"https://en.wikipedia.org/wiki/Hierarchical_clustering\" target=\"_blank\" title=\"External link\">Agglomerative hierarchical clustering</a> ",
                                         "initially assumes that all time series are forming their own clusters. It then grows a clustering dendrogram using two inputs:<p>",
                                         "A <b>dissimilarity matrix</b> between all pairs ",
                                         "of time series is calculated with one of the metrics, such as ",
                                         "Euclidean (<a href=\"https://en.wikipedia.org/wiki/Euclidean_distance\" target=\"_blank\" title=\"External link\">L2 norm</a>), ",
                                         "Manhattan (<a href=\"https://en.wikipedia.org/wiki/Taxicab_geometry\" target=\"_blank\" title=\"External link\">L1 norm</a>), or ",
                                         "<a href=\"https://en.wikipedia.org/wiki/Dynamic_time_warping\" target=\"_blank\" title=\"External link\">Dynamic Time Warping</a> (DTW). ",
                                         "Instead of comparing time series point by point, DTW tries to align and match their shapes. ",
                                         "This makes DTW a good quantification of similarity when signals are similar but shifted in time.</p>",
                                         "<p>In the second step, clusters are successively built and merged together. The distance between the newly formed clusters is determined by the <b>linkage criterion</b> ",
                                         "using one of <a href=\"https://en.wikipedia.org/wiki/Hierarchical_clustering\" target=\"_blank\" title=\"External link\">linkage methods</a>.</p>"),
                    downCellCl = "Download a CSV with cluster assignments to time series ID",
                    downDend = "Download an RDS file with dendrogram object. Read later with readRDS() function.")


# UI ----
tabClHierUI <- function(id, label = "Hierarchical Clustering") {
  ns <- NS(id)
  
  tagList(
    h4('Hierarchical clustering'),
    p("Standard approach using R's ",
      a("dist", 
        href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html",
        title ="External link",
        target = "_blank"),
      " and ",
      a("hclust", 
        href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html",
        title = "External link", 
        target = "_blank"),
      " functions. ",
      actionLink(ns("alLearnMore"), "Learn more")
    ),
    br(),
    fluidRow(
      column(3,
             selectInput(
               ns("selDiss"),
               label = ("Dissimilarity measure"),
               choices = list("Euclidean" = "euclidean",
                              "Manhattan" = "manhattan",
                              "Maximum"   = "maximum",
                              "Canberra"  = "canberra",
                              "DTW"       = "DTW"),
               selected = 1
             ),
             bsAlert("alertAnchorClHierNAsPresent"),
             selectInput(
               ns("selLink"),
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
               selected = 1
             )
      ),
      column(6,
             sliderInput(
               ns('slNclust'),
               'Number of dendrogram branches to cut',
               min = 1,
               max = 20,
               value = 1,
               step = 1,
               ticks = TRUE,
               round = TRUE
             ),
             
             checkboxInput(ns('chBclDisp'), 'Select clusters to display'),
             uiOutput(ns('selClDispUI')),
      ),
      column(3,
             selectInput(
               ns("selPalDend"),
               label = "Cluster colour palette",
               choices = l.col.pal.dend.2,
               selected = 'Color Blind'
             ),
             
             downloadButton(ns('downCellCl'), 'Cluster assignments'),
             bsTooltip(ns("downCellCl"),
                       helpText.clHier[["downCellCl"]],
                       placement = "top",
                       trigger = "hover",
                       options = NULL),
             br(),
             
             downloadButton(ns('downDend'), 'Dendrogram object'),
             bsTooltip(ns("downDend"),
                       helpText.clHier[["downDend"]],
                       placement = "top",
                       trigger = "hover",
                       options = NULL)
      )
    ),
    
    br(),
    
    tabsetPanel(
      
      tabPanel('Heatmap',
               br(),
               modPlotHMdendUI(ns('modPlotHMdend'))),
      
      tabPanel('Cluster averages',
               br(),
               modTrajRibbonPlotUI(ns('modPlotHierTrajRibbon'))),
      
      tabPanel('Time series in clusters',
               br(),
               modTrajPlotUI(ns('modPlotHierTraj'))),
      
      tabPanel('PSD',
               br(),
               modPSDPlotUI(ns('modPlotHierPsd'))),
      
      tabPanel('Cluster distribution',
               br(),
               modClDistPlotUI(ns('hierClDistPlot'), 'xxx')),
      
      tabPanel('PCA',
               br(),
               plotPCAUI(ns('plotHierPCA'))),
      
      tabPanel('Silhouette',
               br(),
               plotSilhUI(ns('plotHierSilh')))
      
    )
  )
}

# SERVER ----
tabClHier <- function(input, output, session, 
                      inDataLong, 
                      inDataStim) {
  
  ns <- session$ns
  
  ## UI rendering ----
  # Return the number of clusters from the slider 
  # and delay by a constant in milliseconds defined in auxfunc.R
  intNclust = reactive({
    return(input$slNclust)
  }) %>% debounce(MILLIS)
  
  # Manually choose clusters to display
  output$selClDispUI = renderUI({
    
    if(input$chBclDisp) {
      selectInput(ns('selClDisp'), 'Select clusters to display', 
                  choices = seq(1, intNclust(), 1),
                  multiple = TRUE, 
                  selected = 1)
    }
  })
  
  
  ## Processing ----
  
  # Prepare data in wide format, ready for distance calculation in clustering
  # Return a matrix with:
  # - time series as rows
  # - time points as columns
  dataWide <- reactive({
    if (DEB){
      cat(file = stdout(), 'tabClHier:dataWide\n')
    }
    
    loc.dt = inDataLong()
    
    if (is.null(loc.dt))
      return(NULL)
    
    if (nrow(loc.dt) < 1)
      return(NULL)
    
    if (DEB) {
      cat(file = stdout(), 'tabClHier:dataWide: dt not NULL\n')
    }
    
    # Check whether there were more objects with the same track ID in the frame
    # Such track IDs will have TRUE assigned in the 'dup' column
    # Keep only s.track column with dup=TRUE
    loc.duptracks = loc.dt[, 
                           .(dup = (sum(duplicated(get(COLRT))) > 0)), 
                           by = COLID][dup == TRUE, COLID, with = FALSE]
    
    if (nrow(loc.duptracks) > 0) {
      createAlert(session, "alertAnchorClHierNAsPresent", "alertDuplPresentCl", title = "Warning",
                  content = helpText.clHier[["alertDuplPresentCl"]], 
                  append = FALSE, 
                  style = "warning")
    } else {
      closeAlert(session, 'alertDuplPresentClDTW')
    }
      
    
    # convert from long to wide format
    loc.dt.wide = dcast(loc.dt, 
                        reformulate(response = COLID, termlabels = COLRT), 
                        value.var = COLY)
    
    # store row names for later
    loc.rownames = loc.dt.wide[[COLID]]
    
    # omit first column that contains row names
    loc.m.out = as.matrix(loc.dt.wide[, -1])
    
    # assign row names to the matrix
    rownames(loc.m.out) = loc.rownames
    
    # Check for missing time points
    # Missing rows in the long format give rise to NAs during dcast
    # Here, we are not checking for explicit NAs in COLY column
    if ((sum(is.na(loc.dt[[COLY]])) == 0) & (sum(is.na(loc.dt.wide)) > 0))
      cat(helpText.clHier[["alertNAsPresentLong2WideConv"]], "\n")
    
    return(loc.m.out)
  }) 
  
  
  # Calculate distance/dissimilarity matrix using a selected distance metric.
  # The result will be used for further clustering.
  # Requires data in wide format with time series arranged in rows and time points in columns.
  dmDiss <- reactive({
    cat(file = stderr(), 'tabClHier:dmDiss\n')
    
    locDM = dataWide()
    
    if (is.null(locDM)) {
      return(NULL)
    }
    
    # Throw some warnings if NAs present in the dataset.
    # DTW cannot compute distance when NA's are preset.
    # Other distance measures can be calculated but caution is required with interpretation.
    # NAs in the wide format can result from explicit NAs in the measurement column or
    # from missing rows that cause NAs to appear when converting from long to wide (dcast)
    if(sum(is.na(locDM)) > 0) {
      if (input$selDiss == "DTW") {
        createAlert(session, "alertAnchorClHierNAsPresent", "alertNAsPresentClDTW", title = "Error",
                    content = helpText.clHier[["alertNAsPresentClDTW"]], 
                    append = FALSE,
                    style = "danger")
        closeAlert(session, 'alertNAsPresentCl')
        
        return(NULL)
        
      } else {
        createAlert(session, "alertAnchorClHierNAsPresent", "alertNAsPresentCl", title = "Warning",
                    content = helpText.clHier[["alertNAsPresentCl"]], 
                    append = FALSE, 
                    style = "warning")
        closeAlert(session, 'alertNAsPresentClDTW')
      }
    } else {
      closeAlert(session, 'alertNAsPresentClDTW')
      closeAlert(session, 'alertNAsPresentCl')
    }
    
    #pr_DB$set_entry(FUN = fastDTW, names = c("fastDTW"))
    locDist = proxy::dist(locDM, 
                          method = input$selDiss)
    
    # Check if distance matrix has NAs.
    # If NAs present, hclust will throw an error.
    # NAs stem from non-overlapping rows (time-series), for which distance cannot be calculated
    if(sum(is.na(locDist)) > 0) {
      createAlert(session, "alertAnchorClHierNAsPresent", "alertNAsInDist", title = "Error",
                  content = helpText.clHier[["alertNAsInDist"]], 
                  append = TRUE,
                  style = "danger")
      return(NULL)
    } else {
      closeAlert(session, 'alertNAsInDist')
      return(locDist)
    }
  })
  
  # perform hierarchical clustering using a selected linkage method
  # return hclust object
  objHclust <- reactive({
    cat(file = stderr(), 'tabClHier:objHclust\n')
    
    # calculate distance matrix
    locDist = dmDiss()
    
    if (is.null(locDist)) {
      return(NULL)
    }
    
    locHC = hclust(locDist, 
                   method = input$selLink)
    
    return(locHC)
  })
  
  # Return a cut dendrogram with branches coloured according to a chosen palette.
  # The colouring of branches performed using the dendextend package.
  # Used to plot the heatmap.
  dendCutColor <- reactive({
    if (DEB) {
      cat(file = stderr(), 'tabClHier:dendCutColor\n')
    }
    
    # calculate hierarchical clustering
    locHC = objHclust()
    if (is.null(locHC)) {
      return(NULL)
    }
    
    # number of clusters at which dendrogram is cut
    locK = intNclust()
    
    locDend = LOCdendCutColor(inHclust = locHC,
                              inK = locK,
                              inColPal = input$selPalDend)
    
    return(locDend)
  }) 
  
  
  
  # Return a named vector with colours and cluster numbers
  # Input:
  # - a data.table with track IDs, cluster colours, and cluster numbers
  #
  # Output:
  # - vector with colours, named according to cluster numbers
  vecColWithCl = reactive({
    if (DEB) {
      cat(file = stderr(), 'tabClHier:vecColWithCl: in\n')
    }
    
    locDend = dendCutColor()
    
    # Number of clusters at which the dendrogram is cut.
    # The dendrogram has to be re-cut in LOCvecColWithCl to obtain the number of clusters.
    locK = intNclust()
    
    locVecCol = LOCvecColWithCl(locDend,
                                locK)
    
    return(locVecCol)
  })

  # Returns a table with:
  # - time series IDs, 
  # - cluster numbers
  # 
  # Clusters are subset based on manual cluster selection and ordering
  dtIDwithCl <- reactive({
    cat(file = stderr(), 'tabClHier:dtIDwithCl\n')
    
    # calculate hierarchical clustering
    locHc = objHclust()
    
    if (is.null(locHc)) {
      return(NULL)
    }
    
    # number of clusters at which dendrogram is cut
    locK = intNclust()
    
    # Obtain time-series IDs with cluster numbers
    locDT = LOCdtIDwithCl(inHclust = locHc,
                          inK = locK,
                          inDeb = F)
    
    if (is.null(locDT)) {
      return(NULL)
    }
    
    return(locDT)
    
  })
  
  # Return dt with track IDs and their corresponding condition name.
  # The condition is the column defined by the facet grouping.
  dtIDwithCond <- reactive({
    cat(file = stderr(), 'tabClHier:dtIDwithCond\n')
    loc.dt = inDataLong()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[, 
                           c(COLID, COLGR),
                           with = F]))
  })

  
  # Prepare data for plotting trajectories per cluster.
  # Outputs dt as data4trajPlot but with an additional column 'cl' that holds cluster numbers.
  # Additionally, some clusters are omitted according to the manual selection.
  
  data4trajPlotCl <- reactive({
    if (DEB) {
      cat(file = stderr(), 'tabClHier:data4trajPlotCl: in\n')
    }
    
    locDT = inDataLong()
    
    if (is.null(locDT)) {
      if (DEB) {
        cat(file = stderr(), 'tabClHier:data4trajPlotCl: dt is NULL\n')
      }
      
      return(NULL)
    }
    
    # get cellIDs with cluster assignments based on the dendrogram cut
    locDTcl = dtIDwithCl()
    
    if (is.null(locDTcl)) {
      if(DEB) {
        cat(file = stderr(), 'tabClHier:data4trajPlotCl: dt.cl is NULL\n')
      }
      
      return(NULL)
    }
    
    
    # Keep only clusters manually specified in input$selClDisp
    # The order of clusters in the input field doesn't matter!
    if(input$chBclDisp) {
      if (length(input$selClDisp) > 0) {
        locDTcl = locDTcl[get(COLCL) %in% input$selClDisp]
      } else {
        return(NULL)
      }
    }
    
    # add the column with cluster assignment to the main dataset
    locDT = merge(locDT, 
                  locDTcl, 
                  by = COLID)
    
    return(locDT)    
  })
  
  
  data4stimPlotCl <- reactive({
    cat(file = stderr(), 'tabClHier:data4stimPlotCl: in\n')
    
    loc.dt = inDataStim()
    
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'tabClHier:data4stimPlotCl: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'tabClHier:data4stimPlotCl: dt not NULL\n')
    return(loc.dt)
  })
  

  # Prepare data for barplot with distribution of items per condition
  # Return a data.table with 3 columns:
  # - grouping
  # - cluster numbers
  # - aggregated number of trajectories per group&cluster
  data4clDistPlot <- reactive({
    cat(file = stderr(), 'tabClHier:data4clDistPlot: in\n')
    
    # get cell id's with associated cluster numbers
    locDTcl = dtIDwithCl()
    if (is.null(locDTcl)) {
      cat(file = stderr(), 'tabClHier:data4clDistPlot: dt.cl is NULL\n')
      return(NULL)
    }
    
    # get cellIDs with condition name
    locDTgr = dtIDwithCond()
    if (is.null(locDTgr)) {
      cat(file = stderr(), 'tabClHier:data4clDistPlot: dt.gr is NULL\n')
      return(NULL)
    }
    
    # add grouping to clusters+ids
    locDT = merge(locDTcl, 
                  locDTgr, 
                  by = COLID)
    
    # Keep only clusters manually specified in input$selClDisp
    # The order of clusters in the input field doesn't matter!
    if(input$chBclDisp) {
      if (length(input$selClDisp) > 0) {
        locDT = locDT[get(COLCL) %in% input$selClDisp]
      } else {
        return(NULL)
      }
    }
    
    # Count the number of time series per group, per cluster
    locDTaggr = locDT[, 
                      .(xxx = .N), 
                      by = c(COLGR, 
                             COLCL)]
    
    setnames(locDTaggr, "xxx", COLNTRAJ)
    
    return(locDTaggr)
    
  })
  
  
  # Create a list with names of the distance metric and the linkage method.
  # Used for passing to plotting functions.
  createClustMethList = reactive({
    return(list(diss = input$selDiss,
                link = input$selLink))
  })
  
  createFnameTrajPlot = reactive({
    
    paste0('clust_hier_tCourses_',
           input$selDiss,
           '_',
           input$selLink, 
           '.pdf')
  })
  
  createFnameRibbonPlot = reactive({
    
    paste0('clust_hier_tCoursesMeans_',
           input$selDiss,
           '_',
           input$selLink, 
           '.pdf')
  })
  
  createFnamePsdPlot = reactive({
    
    paste0('clust_hier_tCoursesPsd_',
           input$selDiss,
           '_',
           input$selLink, 
           '.pdf')
  })
  
  createFnameDistPlot = reactive({
    
    paste0('clust_hier_clDist_',
           input$selDiss,
           '_',
           input$selLink, '.pdf')  
  })
  
  
  createFnamePCAplot = reactive({
    
    paste0('clust_hier_tCoursesPCA_',
           input$selDiss,
           '_',
           input$selLink, 
           '.pdf')
  })
  
  createFnameSilhPlot = reactive({
    
    paste0('clust_hier_tCoursesSilh_',
           input$selDiss,
           '_',
           input$selLink, 
           '.pdf')
  })
  
  
  ## Download ----
  
  
  # download a CSV with a list of cellIDs with cluster assignments
  output$downCellCl <- downloadHandler(
    filename = function() {
      paste0('clust_hier_data_',
             input$selDiss,
             '_',
             input$selLink, '.csv')
    },
    
    content = function(file) {
      write.csv(x = dtIDwithCl(), file = file, row.names = FALSE)
    }
  )
  
  # download an RDS file with dendrogram objet
  output$downDend <- downloadHandler(
    filename = function() {
      paste0('clust_hier_dend_',
             input$selDiss,
             '_',
             input$selLink, '.rds')
    },
    
    content = function(file) {
      saveRDS(object = dendCutColor(), file = file)
    }
  )
  
  
  ## Modules ----
  
  # heatmap plot module 
  callModule(modPlotHMdend, 'modPlotHMdend', 
             inDataWide = dataWide, 
             inDend = dendCutColor,
             inMeth = createClustMethList)
  
  
  # plot individual trajectories within clusters  
  callModule(modTrajPlot, 'modPlotHierTraj', 
             in.data = data4trajPlotCl, 
             in.data.stim = data4stimPlotCl,
             in.facet = COLCL,  
             in.facet.color = vecColWithCl,
             in.fname = createFnameTrajPlot)
  
  # plot cluster means
  callModule(modTrajRibbonPlot, 'modPlotHierTrajRibbon', 
             in.data = data4trajPlotCl, 
             in.data.stim = data4stimPlotCl,
             in.group = COLCL,  
             in.group.color = vecColWithCl,
             in.fname = createFnameRibbonPlot)
  
  # plot cluster PSD
  callModule(modPSDPlot, 'modPlotHierPsd',
             in.data = data4trajPlotCl,
             in.facet = COLCL,
             in.facet.color = vecColWithCl,
             in.fname = createFnamePsdPlot)
  
  # plot distribution barplot
  callModule(modClDistPlot, 'hierClDistPlot', 
             in.data = data4clDistPlot,
             in.colors = vecColWithCl,
             in.fname = createFnameDistPlot)
  
  # plot cluster PCA
  callModule(plotPCA, 'plotHierPCA',
             inDataWide = dataWide,
             inIdWithCl = dtIDwithCl,
             inColWithCl = vecColWithCl,
             inMeth = createClustMethList)
  
  # plot cluster Silhouette
  callModule(plotSilh, 'plotHierSilh',
             inDist = dmDiss,
             inColWithCl = vecColWithCl,
             inMeth = createClustMethList)
  
  
  # Pop-overs ----
  addPopover(session, 
             ns("alLearnMore"),
             title = "Hierarchical clustering",
             content = helpText.clHier[["alLearnMore"]],
             trigger = "click")
  
  
}