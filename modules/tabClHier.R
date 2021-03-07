#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for hierarchical clustering (base R hclust + dist)

helpText.clHier = c(alertNAsPresentClDTW = paste0("NAs (still) present. DTW cannot calculate the distance. ",
                                                  "If interpolation is active in the left panel, missing data can be due to removed outlier time points."),
                    alertNAsPresentCl = paste0("NAs (still) present, caution recommended. If interpolation is active in the left panel, ",
                                               "missing data can be due to removed outlier time points."),
                    alertNAsInDist = "NAs in distance matrix. Possible non-overlapping time series in the dataset.",
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
clustHierUI <- function(id, label = "Hierarchical Clustering") {
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
               ns("selectPlotHierDiss"),
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
               ns("selectPlotHierLinkage"),
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
               ns('slPlotHierNclust'),
               'Number of dendrogram branches to cut',
               min = 1,
               max = 20,
               value = 1,
               step = 1,
               ticks = TRUE,
               round = TRUE
             ),
             
             checkboxInput(ns('chBPlotHierClSel'), 'Select clusters to display'),
             uiOutput(ns('uiPlotHierClSel')),
      ),
      column(3,
             selectInput(
               ns("selectPlotHierPaletteDend"),
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
               modPlotHeatmapUI(ns('modPlotHeatmap'))),
      
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
               modPCAplotUI(ns('modPlotHierPCA')))
    )
  )
}

# SERVER ----
clustHier <- function(input, output, session, in.dataWide, in.dataLong, in.dataStim) {
  
  ns <- session$ns
  
  ## UI ----
  # Return the number of clusters from the slider 
  # and delay by a constant in milliseconds defined in auxfunc.R
  returnNclust = reactive({
    return(input$slPlotHierNclust)
  }) %>% debounce(MILLIS)
  
  
  output$uiPlotHierClSel = renderUI({
    
    if(input$chBPlotHierClSel) {
      selectInput(ns('inPlotHierClSel'), 'Select clusters to display', 
                  choices = seq(1, returnNclust(), 1),
                  multiple = TRUE, 
                  selected = 1)
    }
  })
  

  ## Processing ----
  
  # calculate distance matrix for further clustering
  # time series arranged in rows with columns corresponding to time points
  calcDist <- reactive({
    cat(file = stderr(), 'calcDist \n')
    
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
      if (input$selectPlotHierDiss == "DTW") {
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
    cl.dist = proxy::dist(loc.dm, method = input$selectPlotHierDiss)
    
    # check if distance matrix has NAs
    # if NAs present, hclust will throw an error
    # NAs stem from non-overlapping rows (time-series), for which distance cannot be calculated
    if(sum(is.na(cl.dist)) > 0) {
      createAlert(session, "alertAnchorClHierNAsPresent", "alertNAsInDist", title = "Error",
                  content = helpText.clHier[["alertNAsInDist"]], 
                  append = TRUE,
                  style = "danger")
      return(NULL)
    } else {
      closeAlert(session, 'alertNAsInDist')
      return(cl.dist)
    }
    
    
  })
  
  # perform hierarchical clustering 
  calcHclust <- reactive({
    cat(file = stderr(), 'calcHclust \n')
    
    # calculate distance matrix
    loc.dm.dist = calcDist()
    
    if (is.null(loc.dm.dist)) {
      return(NULL)
    }
    
    loc.cl.hc = hclust(loc.dm.dist, method = input$selectPlotHierLinkage)
    
    return(loc.cl.hc)
  })
  
  # return dendrogram coloured according to cutree
  # branch coloring performed using dendextend package
  calcDendCutColor <- reactive({
    cat(file = stderr(), 'calcDendCutColor \n')
    
    # calculate distance matrix
    loc.cl.hc = calcHclust()
    
    if (is.null(loc.cl.hc)) {
      return(NULL)
    }
    
    # number of clusters at which dendrogram is cut
    loc.k = returnNclust()
    
    # make a palette with the amount of colours equal to the number of clusters
    #loc.col = get(input$selectPlotHierPaletteDend)(n = loc.k)
    loc.col = ggthemes::tableau_color_pal(input$selectPlotHierPaletteDend)(n = loc.k)
    
    loc.dend <- as.dendrogram(loc.cl.hc)
    loc.dend <- dendextend::color_branches(loc.dend, 
                                           col = loc.col, 
                                           k = loc.k)
    
    return(loc.dend)
  }) 
  
  
  # Returns a table prepared with f-n getClCol
  # for hierarchical clustering.
  # The table contains colours assigned to clusters.
  # Colours are obtained from the dendrogram using dendextend::get_leaves_branches_col
  getClColHier <- reactive({
    cat(file = stderr(), 'getClColHier \n')
    
    loc.dend = calcDendCutColor()
    if (is.null(loc.dend))
      return(NULL)
    
    # obtain relations between cluster and colors from the dendrogram
    loc.dt = LOCgetClCol(loc.dend, returnNclust())
    
    # Display clusters specified in the inPlotHierClSel field
    # Clusters are ordered according to the order of clusters specified in this field
    if(input$chBPlotHierClSel) {
      # keep only clusters specified in input$inPlotHierClSel
      loc.dt = loc.dt[gr.no %in% input$inPlotHierClSel]
      loc.dt[, gr.no := factor(gr.no, levels = input$inPlotHierClSel)]
    }
    
    # set the key to allow sub-setting
    setkey(loc.dt, gr.no)
    
    return(loc.dt)
  })
  
  
  
  
  # UNUSED ATM
  # Return all unique track object labels (created in dataMod)
  # This will be used to display in UI for trajectory highlighting
  getDataTrackObjLabUni_afterTrim <- reactive({
    cat(file = stderr(), 'getDataTrackObjLabUni_afterTrim\n')
    loc.dt = in.dataLong()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt$id))
  })
  
  # return dt with cell IDs and their corresponding condition name
  # The condition is the column defined by facet groupings
  getDataCond <- reactive({
    cat(file = stderr(), 'getDataCond\n')
    loc.dt = in.dataLong()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[, .(id, group)]))
    
  })
  
  
  
  # Prepare data with track IDs and cluster numbers
  # Used for colouring points by cluster numbers in PCA
  # If manual cluster selection switched on, 
  # return only those clusters specified in the inPlotHierClSel field
  dataIDwithCl <- reactive({
    cat(file = stderr(), 'dataIDwithCl: in\n')
    
    # get cellIDs with cluster assignments based on dendrogram cut
    loc.dt.cl = getDataCl(calcDendCutColor(), returnNclust())
    
    
    if (is.null(loc.dt.cl)) {
      cat(file = stderr(), 'dataClWithCol: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'dataClWithCol: dt not NULL\n')
    
    # Display clusters specified in the inPlotHierClSel field
    # Data is ordered according to the order of clusters specified in this field
    if(input$chBPlotHierClSel) {
      loc.dt.cl = loc.dt.cl[get(COLCL) %in% input$inPlotHierClSel]
      loc.dt.cl[, (COLCL) := factor(get(COLCL), levels = input$inPlotHierClSel)]
      setkeyv(loc.dt.cl, COLCL)
    } else {
      loc.dt.cl[, (COLCL) := as.factor(get(COLCL))]
    }
    
    return(loc.dt.cl)    
  })
  
  # prepare data for plotting trajectories per cluster
  # outputs dt as data4trajPlot but with an additional column 'cl' that holds cluster numbers
  # additionally some clusters are omitted according to manual selection
  data4trajPlotCl <- reactive({
    cat(file = stderr(), 'data4trajPlotCl: in\n')
    
    loc.dt = in.dataLong()
    
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'data4trajPlotCl: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'data4trajPlotCl: dt not NULL\n')
    
    # get cellIDs with cluster assignments based on dendrogram cut
    loc.dt.cl = getDataCl(calcDendCutColor(), returnNclust())
    
    if (is.null(loc.dt.cl)) {
      cat(file = stderr(), 'data4trajPlotCl: dt.cl is NULL\n')
      
      return(NULL)
    }

    cat(file = stderr(), 'data4trajPlotCl: dt.cl not NULL\n')
    
    # add the column with cluster assignemnt to the main dataset
    loc.dt = merge(loc.dt, loc.dt.cl, by = COLID)
    
    # Display clusters specified in the inPlotHierClSel field
    # Data is ordered according to the order of clusters specified in this field
    if(input$chBPlotHierClSel) {
      loc.dt = loc.dt[get(COLCL) %in% input$inPlotHierClSel]
      loc.dt[, (COLCL) := factor(get(COLCL), levels = input$inPlotHierClSel)]
      setkeyv(loc.dt, COLCL)
    }
    
    return(loc.dt)    
  })
  
  data4stimPlotCl <- reactive({
    cat(file = stderr(), 'data4stimPlotCl: in\n')
    
    loc.dt = in.dataStim()
    
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'data4stimPlotCl: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'data4stimPlotCl: dt not NULL\n')
    return(loc.dt)
  })
  
  # download a CSV with a list of cellIDs with cluster assignments
  output$downCellCl <- downloadHandler(
    filename = function() {
      paste0('clust_hier_data_',
             input$selectPlotHierDiss,
             '_',
             input$selectPlotHierLinkage, '.csv')
    },
    
    content = function(file) {
      write.csv(x = getDataCl(calcDendCutColor(), returnNclust()), file = file, row.names = FALSE)
    }
  )
  
  # download an RDS file with dendrogram objet
  output$downDend <- downloadHandler(
    filename = function() {
      paste0('clust_hier_dend_',
             input$selectPlotHierDiss,
             '_',
             input$selectPlotHierLinkage, '.rds')
    },
    
    content = function(file) {
      saveRDS(object = calcDendCutColor(), file = file)
    }
  )
  
  # prepare data for barplot with distribution of items per condition  
  data4clDistPlot <- reactive({
    cat(file = stderr(), 'data4clDistPlot: in\n')
    
    # get cell IDs with cluster assignments depending on dendrogram cut
    loc.dend <- calcDendCutColor()
    if (is.null(loc.dend)) {
      cat(file = stderr(), 'plotClDist: loc.dend is NULL\n')
      return(NULL)
    }
    
    # get cell id's with associated cluster numbers
    loc.dt.cl = getDataCl(loc.dend, returnNclust())
    if (is.null(loc.dt.cl)) {
      cat(file = stderr(), 'plotClDist: loc.dt.cl is NULL\n')
      return(NULL)
    }
    
    # get cellIDs with condition name
    loc.dt.gr = getDataCond()
    if (is.null(loc.dt.gr)) {
      cat(file = stderr(), 'plotClDist: loc.dt.gr is NULL\n')
      return(NULL)
    }
    
    # add grouping to clusters+ids
    loc.dt = merge(loc.dt.cl, loc.dt.gr, by = COLID)
    
    # count number of time series per group, per cluster
    loc.dt.aggr = loc.dt[, .(xxx = .N), by = c(COLGR, COLCL)]
    setnames(loc.dt.aggr, "xxx", COLNTRAJ)
    
    # Display clusters specified in the inPlotHierClSel field
    # Data is ordered according to the order of clusters specified in this field
    if(input$chBPlotHierClSel) {
      loc.dt.aggr = loc.dt.aggr[cl %in% input$inPlotHierClSel]
      loc.dt.aggr[, (COLCL) := factor(get(COLCL), levels = input$inPlotHierClSel)]
      setkeyv(loc.dt.aggr, COLCL)
    }
    return(loc.dt.aggr)
    
  })
  
  createClustMethList = reactive({
    return(list(diss = input$selectPlotHierDiss,
                link = input$selectPlotHierLinkage))
  })
  
  createFnameTrajPlot = reactive({
    
    paste0('clust_hier_tCourses_',
           input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage, 
           '.pdf')
  })
  
  createFnameRibbonPlot = reactive({
    
    paste0('clust_hier_tCoursesMeans_',
           input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage, 
           '.pdf')
  })
  
  createFnamePsdPlot = reactive({
    
    paste0('clust_hier_tCoursesPsd_',
           input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage, 
           '.pdf')
  })
  
  createFnameDistPlot = reactive({
    
    paste0('clust_hier_clDist_',
           input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage, '.pdf')  
  })
  
  
  createFnamePCAplot = reactive({
    
    paste0('clust_hier_tCoursesPCA_',
           input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage, 
           '.pdf')
  })
  
  
  
  ## Modules ----

  # heatmap plot module 
  callModule(modPlotHeatmap, 'modPlotHeatmap', 
             inDataWide = in.dataWide, 
             inDend = calcDendCutColor,
             inMeth = createClustMethList)
  
    
  # plot individual trajectories within clusters  
  callModule(modTrajPlot, 'modPlotHierTraj', 
             in.data = data4trajPlotCl, 
             in.data.stim = data4stimPlotCl,
             in.facet = COLCL,  
             in.facet.color = getClColHier,
             in.fname = createFnameTrajPlot)
  
  # plot cluster means
  callModule(modTrajRibbonPlot, 'modPlotHierTrajRibbon', 
             in.data = data4trajPlotCl, 
             in.data.stim = data4stimPlotCl,
             in.group = COLCL,  
             in.group.color = getClColHier,
             in.fname = createFnameRibbonPlot)
  
  # plot cluster PSD
  callModule(modPSDPlot, 'modPlotHierPsd',
             in.data = data4trajPlotCl,
             in.facet = COLCL,
             in.facet.color = getClColHier,
             in.fname = createFnamePsdPlot)
  
  # plot distribution barplot
  callModule(modClDistPlot, 'hierClDistPlot', 
             in.data = data4clDistPlot,
             in.colors = getClColHier,
             in.fname = createFnameDistPlot)
  
  # plot cluster PCA
  callModule(modPCAplot, 'modPlotHierPCA',
             in.dataWide = in.dataWide,
             in.idWithCl = dataIDwithCl,
             in.clWithCol = getClColHier,
             in.fname = createFnamePCAplot)
  
  # Pop-overs ----
  addPopover(session, 
             ns("alLearnMore"),
             title = "Hierarchical clustering",
             content = helpText.clHier[["alLearnMore"]],
             trigger = "click")
  
  
}