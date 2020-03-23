#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for hierarchical clustering (base R hclust + dist)

helpText.clHier = c(alertNAsPresentClDTW = paste0("NAs (still) present. DTW cannot calculate the distance. ",
                                                  "If interpolation is active in the left panel, missing data can be due to removed outlier time points."),
                    alertNAsPresentCl = paste0("NAs (still) present, caution recommended. If interpolation is active in the left panel, ",
                                               "missing data can be due to removed outlier time points."),
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
                                         "using one of <a href=\"https://en.wikipedia.org/wiki/Hierarchical_clustering\" target=\"_blank\" title=\"External link\">linkage methods</a>.</p>"))


# UI ----
clustHierWdistUI <- function(id, label = "Hierarchical Clustering") {
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
             fileInput(
               ns('inFileLoadDist'),
               "Select data file and click Load Data"
             ),

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
             
             # These two lines are to manually assign colours to clusters; it doesn't really work well, so skip
             # NOT USED AT THE MOMENT!
             #checkboxInput(ns('chBPlotHierClAss'), 'Manually assign cluster colours'),
             #uiOutput(ns('uiPlotHierClAss')),
             
             checkboxInput(ns('chBPlotHierClSel'), 'Manually select clusters to display'),
             uiOutput(ns('uiPlotHierClSel')),
             downloadButton(ns('downCellCl'), 'Download CSV with cluster assignments')
      )
    ),
    
    br(),
    
    tabsetPanel(
      tabPanel('Heatmap',
               br(),
               fluidRow(
                 column(3,
                        selectInput(
                          ns("selectPlotHierPalette"),
                          label = "Heatmap\'s colour palette",
                          choices = l.col.pal,
                          selected = 'Spectral'
                        ),
                        checkboxInput(ns('inPlotHierRevPalette'), 'Reverse heatmap\'s colour palette', TRUE),
                        checkboxInput(ns('selectPlotHierKey'), 'Plot colour key', TRUE),
                        checkboxInput(ns('chBsetColBounds'), 'Set bounds for colour scale', FALSE),
                        
                        fluidRow(
                          column(5,
                                 uiOutput(ns('uiSetColBoundsLow'))
                          ),
                          column(5,
                                 uiOutput(ns('uiSetColBoundsHigh'))
                          )
                        )
                 ),
                 column(3,
                        selectInput(
                          ns("selectPlotHierPaletteDend"),
                          label = "Dendrogram\'s colour palette",
                          choices = l.col.pal.dend.2,
                          selected = 'Color Blind'
                        ),
                        checkboxInput(ns('selectPlotHierDend'), 'Plot dendrogram and re-order samples', TRUE),
                        sliderInput(
                          ns('inPlotHierNAcolor'),
                          'Shade of grey for NA values',
                          min = 0,
                          max = 1,
                          value = 0.8,
                          step = .1,
                          ticks = TRUE
                        )
                        
                 ),
                 column(3,
                        numericInput(
                          ns('inPlotHierMarginX'),
                          'Bottom margin',
                          5,
                          min = 1,
                          width = "120px"
                        ),
                        numericInput(
                          ns('inPlotHierFontY'),
                          'Font size column labels',
                          1,
                          min = 0,
                          width = "180px",
                          step = 0.1
                        ),
                        numericInput(ns('inPlotHierHeatMapHeight'), 
                                     'Display plot height [px]', 
                                     value = 600, 
                                     min = 100,
                                     step = 100,
                                     width = "180px")
                        
                 ),
                 column(3,
                        numericInput(
                          ns('inPlotHierMarginY'),
                          'Right margin',
                          20,
                          min = 1,
                          width = "120px"
                        ),
                        numericInput(
                          ns('inPlotHierFontX'),
                          'Font size row labels',
                          1,
                          min = 0,
                          width = "180px",
                          step = 0.1
                        )
                 )
               ),
               
               actionButton(ns('butPlotHierHeatMap'), 'Plot!'),
               downPlotUI(ns('downPlotHier'), "Download Plot"),
               withSpinner(plotOutput(ns('outPlotHier')))
      ),
      
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
               modClDistPlotUI(ns('hierClDistPlot'), 'xxx'))
      
    )
  )
}

# SERVER ----
clustHierWdist <- function(input, output, session, in.dataWide, in.dataLong, in.dataStim) {
  
  ns <- session$ns
  
  
  # load distance matrix
  dataLoadDist <- reactive({
    if (DEB)
      cat(file = stdout(), "clustHierWdist:dataLoadDist\n")
    
    locFilePath = input$inFileLoadDist$datapath
    
    if (is.null(locFilePath) || locFilePath == '')
      return(NULL)
    else {
      return(readRDS(locFilePath))
    }
  })
  
  # Return the number of clusters from the slider 
  # and delay by a constant in milliseconds defined in auxfunc.R
  returnNclust = reactive({
    return(input$slPlotHierNclust)
  }) %>% debounce(MILLIS)
  
  # not functional; see th note in UI
  output$uiPlotHierClAss = renderUI({
    
    if(input$chBPlotHierClAss) {
      selectInput(ns('inPlotHierClAss'), 'Assign cluster order', 
                  choices = seq(1, returnNclust(), 1),
                  multiple = TRUE, 
                  selected = seq(1, returnNclust(), 1))
    }
  })
  
  output$uiPlotHierClSel = renderUI({
    
    if(input$chBPlotHierClSel) {
      selectInput(ns('inPlotHierClSel'), 'Select clusters to display', 
                  choices = seq(1, returnNclust(), 1),
                  multiple = TRUE, 
                  selected = 1)
    }
  })
  
  
  
  # UI for setting lower and upper bounds for heat map colour scale  
  output$uiSetColBoundsLow = renderUI({
    
    if(input$chBsetColBounds) {
      
      loc.dt = in.dataLong()
      if (is.null(loc.dt))
        return(NULL)
      
      numericInput(
        ns('inSetColBoundsLow'),
        label = 'Lower',
        step = 0.1, 
        value = signif(min(loc.dt[['y']], na.rm = T), digits = 3)
      )
    }
  })
  
  
  output$uiSetColBoundsHigh = renderUI({
    
    if(input$chBsetColBounds) {
      
      loc.dt = in.dataLong()
      if (is.null(loc.dt))
        return(NULL)
      
      numericInput(
        ns('inSetColBoundsHigh'),
        label = 'Upper',
        step = 0.1, 
        value = signif(max(loc.dt[['y']], na.rm = T), digits = 3)
      )
    }
  })
  
  
  # perform hierarchical clustering and return dendrogram coloured according to cutree
  # branch coloring performed using dendextend package
  userFitDendHier <- reactive({
    cat(file = stderr(), 'userFitDendHier \n')
    
    # calculate distance matrix
    loc.dm.dist = dataLoadDist()

    if (is.null(loc.dm.dist)) {
      return(NULL)
    }
    
    loc.cl.hc = hclust(loc.dm.dist, method = input$selectPlotHierLinkage)
    
    # number of clusters at which dendrogram is cut
    loc.k = returnNclust()
    
    # make a palette with the amount of colours equal to the number of clusters
    #loc.col = get(input$selectPlotHierPaletteDend)(n = loc.k)
    loc.col = ggthemes::tableau_color_pal(input$selectPlotHierPaletteDend)(n = loc.k)
    
    # take into account manual assignment of cluster numbers
    # NOT USED AT THE MOMENT
    #if (input$chBPlotHierClAss) {
    #  loc.col = loc.col[as.numeric(input$inPlotHierClAss)]
    #}
    
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
    
    loc.dend = userFitDendHier()
    if (is.null(loc.dend))
      return(NULL)
    
    # obtain relations between cluster and colors from the dendrogram
    loc.dt = LOCgetClCol(loc.dend, returnNclust())
    
    # Display clusters specified in the inPlotHierClSel field
    # Data is ordered according to the order of clusters specified in this field
    if(input$chBPlotHierClSel) {
      # kepp only clusters specified in input$inPlotHierClSel
      loc.dt = loc.dt[gr.no %in% input$inPlotHierClSel]
      loc.dt[, gr.no := factor(gr.no, levels = input$inPlotHierClSel)]
    }
    
    # set the key to allow subsetting
    setkey(loc.dt, gr.no)
    
    return(loc.dt)
  })
  
  
  
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
    loc.dt.cl = getDataCl(userFitDendHier(), returnNclust())

    # add the column with cluster assignemnt to the main dataset
    loc.dt = merge(loc.dt, loc.dt.cl, by = COLID)

    # Display clusters specified in the inPlotHierClSel field
    # Data is ordered according to the order of clusters specified in this field
    if(input$chBPlotHierClSel) {
      loc.dt = loc.dt[cl %in% input$inPlotHierClSel]
      loc.dt[, cl := factor(cl, levels = input$inPlotHierClSel)]
      setkey(loc.dt, cl)
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
  
  # download a list of cellIDs with cluster assignments
  output$downCellCl <- downloadHandler(
    filename = function() {
      paste0('clust_hierch_data_',
             input$selectPlotHierDiss,
             '_',
             input$selectPlotHierLinkage, '.csv')
    },
    
    content = function(file) {
      write.csv(x = getDataCl(userFitDendHier(), returnNclust()), file = file, row.names = FALSE)
    }
  )
  
  # prepare data for barplot with distribution of items per condition  
  data4clDistPlot <- reactive({
    cat(file = stderr(), 'data4clDistPlot: in\n')
    
    # get cell IDs with cluster assignments depending on dendrogram cut
    loc.dend <- userFitDendHier()
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
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  plotHier <- function() {
    cat(file = stderr(), 'plotHier: in\n')
    
    # make the f-n dependent on the button click
    locBut = input$butPlotHierHeatMap
    
    # Check if main data exists
    # Thanks to solate all mods in the left panel are delayed 
    # until clicking the Plot button
    loc.dm = shiny::isolate(in.dataWide())
    loc.dend = shiny::isolate(userFitDendHier())
    
    shiny::validate(
      shiny::need(!is.null(loc.dm), "Nothing to plot. Load data first!"),
      shiny::need(!is.null(loc.dend), "Could not create dendrogram")
    )
    
    # Dummy dependency to redraw the heatmap without clicking Plot
    # when changing the number of clusters to highlight
    loc.k = returnNclust()
    
    loc.col.bounds = NULL
    if (input$chBsetColBounds)
      loc.col.bounds = c(input$inSetColBoundsLow, 
                         input$inSetColBoundsHigh)
    else 
      loc.col.bounds = NULL
    
    
    loc.p = LOCplotHeatmap(loc.dm,
                           loc.dend, 
                           palette.arg = input$selectPlotHierPalette, 
                           palette.rev.arg = input$inPlotHierRevPalette, 
                           dend.show.arg = input$selectPlotHierDend, 
                           key.show.arg = input$selectPlotHierKey, 
                           margin.x.arg = input$inPlotHierMarginX, 
                           margin.y.arg = input$inPlotHierMarginY, 
                           nacol.arg = input$inPlotHierNAcolor, 
                           font.row.arg = input$inPlotHierFontX, 
                           font.col.arg = input$inPlotHierFontY, 
                           breaks.arg = loc.col.bounds,
                           title.arg = paste0(
                             "Distance measure: ",
                             input$selectPlotHierDiss,
                             "\nLinkage method: ",
                             input$selectPlotHierLinkage
                           ))
    
    return(loc.p)
  }
  
  
  
  #  Hierarchical - display heatmap
  getPlotHierHeatMapHeight <- function() {
    return (input$inPlotHierHeatMapHeight)
  }
  
  output$outPlotHier <- renderPlot({
    
    plotHier()
  }, height = getPlotHierHeatMapHeight)
  
  createFnameHeatMap = reactive({
    
    paste0('clust_hierch_heatMap_',
           input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage,
           '.png')
  })
  
  createFnameTrajPlot = reactive({
    
    paste0('clust_hierch_tCourses_',
           input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage, 
           '.pdf')
  })
  
  createFnameRibbonPlot = reactive({
    
    paste0('clust_hierch_tCoursesMeans_',
           input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage, 
           '.pdf')
  })
  
  createFnamePsdPlot = reactive({
    
    paste0('clust_hierch_tCoursesPsd_',
           input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage, 
           '.pdf')
  })
  
  createFnameDistPlot = reactive({
    
    paste0('clust_hierch_clDist_',
           input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage, '.pdf')  
  })
  
  
  #  Hierarchical - Heat Map - download pdf
  callModule(downPlot, "downPlotHier", createFnameHeatMap, plotHier)
  
  # plot individual trajectories withina cluster  
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
  
  # Pop-overs ----
  addPopover(session, 
             ns("alLearnMore"),
             title = "Hierarchical clustering",
             content = helpText.clHier[["alLearnMore"]],
             trigger = "click")
  
  
}