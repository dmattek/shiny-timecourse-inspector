#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for sparse hierarchical clustering using sparcl package
#

# UI ----
clustHierSparUI <- function(id, label = "Sparse Hierarchical CLustering") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Sparse hierarchical clustering using ",
      a("sparcl", href = "https://cran.r-project.org/web/packages/sparcl/")
    ),
    br(),
    fluidRow(
      column(
        4,
        selectInput(
          ns("selectPlotHierSparDiss"),
          label = ("Select type of dissimilarity measure:"),
          choices = list("Euclidean" = 1,
                         "Manhattan" = 2),
          selected = 1
        ),
        selectInput(
          ns("selectPlotHierSparLinkage"),
          label = ("Select linkage method:"),
          choices = list(
            "Average" = 1,
            "Complete" = 2,
            "Single" = 3,
            "Centroid" = 4
          ),
          selected = 1
        )
      ),
      
      column(
        4,
        sliderInput(
          ns('inPlotHierSparNclust'),
          '#dendrogram branches to colour',
          min = 1,
          max = 20,
          value = 1,
          step = 1,
          ticks = TRUE,
          round = TRUE
        ),
        checkboxInput(ns('chBPlotHierSparClSel'), 'Manually select clusters to display'),
        uiOutput(ns('uiPlotHierSparClSel')),
        downloadButton(ns('downCellClSpar'), 'Download CSV with cluster assignments')
      ),
      
      column(
        4,
        checkboxInput(ns('inHierSparAdv'),
                      'Advanced options',
                      FALSE),
        uiOutput(ns('uiPlotHierSparNperms')),
        uiOutput(ns('uiPlotHierSparNiter'))
      )
    ),
    
    
    br(),
    
    tabsetPanel(
      tabPanel('Heatmap',
               br(),
               fluidRow(
                 column(3,
                        selectInput(
                          ns("selectPlotHierSparPalette"),
                          label = "Heatmap\'s colour palette",
                          choices = l.col.pal,
                          selected = 'Spectral'
                        ),
                        checkboxInput(ns('inPlotHierSparRevPalette'), 'Reverse heatmap\'s colour palette', TRUE),
                        checkboxInput(ns('selectPlotHierSparKey'), 'Plot colour key', TRUE),
                        
                        checkboxInput(ns('chBsetColBounds'), 'Set bounds for colour scale', FALSE),
                        
                        fluidRow(
                          column(3,
                                 uiOutput(ns('uiSetColBoundsLow'))
                          ),
                          column(3,
                                 uiOutput(ns('uiSetColBoundsHigh'))
                          )
                        )
                 ),
                 column(3,
                        selectInput(
                          ns("selectPlotHierSparPaletteDend"),
                          label = "Dendrogram\'s colour palette",
                          choices = l.col.pal.dend.2,
                          selected = 'Color Blind'
                        ),
                        checkboxInput(ns('selectPlotHierSparDend'), 'Plot dendrogram and re-order samples', TRUE),
                        sliderInput(
                          ns('inPlotHierSparNAcolor'),
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
                          ns('inPlotHierSparMarginX'),
                          'Bottom margin',
                          5,
                          min = 1,
                          width = "120px"
                        ),
                        numericInput(
                          ns('inPlotHierSparFontY'),
                          'Font size column labels',
                          1,
                          min = 0,
                          width = "180px",
                          step = 0.1
                        ),
                        numericInput(ns('inPlotHierSparHeatMapHeight'), 
                                     'Display plot height [px]', 
                                     value = 600, 
                                     min = 100,
                                     step = 100, 
                                     width = "180px")
                        
                 ),
                 column(3,
                        numericInput(
                          ns('inPlotHierSparMarginY'),
                          'Right margin',
                          20,
                          min = 1,
                          width = "120px"
                        ),
                        numericInput(
                          ns('inPlotHierSparFontX'),
                          'Font size row labels',
                          1,
                          min = 0,
                          width = "180px",
                          step = 0.1
                        )
                 )
               ),
               
               br(),
               downPlotUI(ns('downPlotHierSparHM'), "Download PNG"),
               actionButton(ns('butPlotHierSparHeatMap'), 'Plot!'),
               br(),
               "Columns in the heatmap labeled according to their ",
               actionLink(ns("alImportance"), "importance"),
               withSpinner(plotOutput(ns('outPlotHierSpar')))

      ),
      
      tabPanel('Averages',
               br(),
               modTrajRibbonPlotUI(ns('modPlotHierSparTrajRibbon'))),
      
      tabPanel('Time series',
               br(),
               modTrajPlotUI(ns('modPlotHierSparTraj'))),
      
      tabPanel('PSD',
               br(),
               modPSDPlotUI(ns('modPlotHierSparPsd'))),
      
      tabPanel('Cluster distribution',
               br(),
               modClDistPlotUI(ns('hierClSparDistPlot')))
    )
  )
}

# SERVER ----
clustHierSpar <- function(input, output, session, 
                          in.data4clust, 
                          in.data4trajPlot, 
                          in.data4stimPlot) {
  
  ns = session$ns
  
  # UI for advanced options
  output$uiPlotHierSparNperms = renderUI({
    ns <- session$ns
    if (input$inHierSparAdv)
      sliderInput(
        ns('inPlotHierSparNperms'),
        'Number of permutations',
        min = 1,
        max = 20,
        value = 1,
        step = 1,
        ticks = TRUE
      )
  })
  
  output$uiSetColBoundsLow = renderUI({
    ns <- session$ns
    
    if(input$chBsetColBounds) {
      
      loc.dt = in.data4trajPlot()
      
      numericInput(
        ns('inSetColBoundsLow'),
        label = 'Lower',
        step = 0.1, 
        value = floor(min(loc.dt[['y']], na.rm = T))
      )
    }
  })
  
  
  output$uiSetColBoundsHigh = renderUI({
    ns <- session$ns
    
    if(input$chBsetColBounds) {
      
      loc.dt = in.data4trajPlot()
      
      numericInput(
        ns('inSetColBoundsHigh'),
        label = 'Upper',
        step = 0.1, 
        value = ceil(max(loc.dt[['y']], na.rm = T))
      )
    }
  })
  
  # UI for advanced options
  output$uiPlotHierSparNiter = renderUI({
    ns <- session$ns
    
    if (input$inHierSparAdv)
      sliderInput(
        ns('inPlotHierSparNiter'),
        'Number of iterations',
        min = 1,
        max = 50,
        value = 1,
        step = 1,
        ticks = TRUE
      )
  })
  
  
  output$uiPlotHierSparClSel = renderUI({
    ns <- session$ns
    
    if(input$chBPlotHierSparClSel) {
      selectInput('inPlotHierSparClSel', 'Select clusters to display', 
                  choices = seq(1, input$inPlotHierSparNclust, 1),
                  multiple = TRUE, 
                  selected = 1)
    }
  })
  
  
  userFitHierSpar <- reactive({
    cat(file = stderr(), 'userFitHierSpar \n')
    
    dm.t = in.data4clust()
    if (is.null(dm.t)) {
      return()
    }
    
    #cat('rownames: ', rownames(dm.t), '\n')
    #cat('=============\ndimensions:', dim(dm.t), '\n')
    
    perm.out <- HierarchicalSparseCluster.permute(
      dm.t,
      wbounds = NULL,
      nperms = ifelse(input$inHierSparAdv, input$inPlotHierSparNperms, 1),
      dissimilarity = s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)]
    )
    
    sparsehc <- HierarchicalSparseCluster(
      dists = perm.out$dists,
      wbound = perm.out$bestw,
      niter = ifelse(input$inHierSparAdv, input$inPlotHierSparNiter, 1),
      method = s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)],
      dissimilarity = s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)]
    )
    
    #cat('=============\nsparsehc:\n')
    #print(sparsehc$hc)
    
    return(sparsehc)
  })
  
  
  
  # return dendrogram colour coded according to the cut level of the dendrogram
  userFitDendHierSpar <- reactive({
    sparsehc = userFitHierSpar()
    if (is.null(sparsehc)) {
      return()
    }

    # number of clusters at which dendrigram is cut
    loc.k = input$inPlotHierSparNclust
    
    # make a palette with the amount of colours equal to the number of clusters
    #loc.col = get(input$selectPlotHierSparPaletteDend)(n = loc.k)
    loc.col = ggthemes::tableau_color_pal(input$selectPlotHierSparPaletteDend)(n = loc.k)
    
    
    dend <- as.dendrogram(sparsehc$hc)
    dend <- color_branches(dend, 
                           col = loc.col,
                           k = loc.k)
    
    return(dend)
  })
  
  # returns table prepared with f-n getClCol
  # for sparse hierarchical clustering
  getClColHierSpar <- reactive({
    cat(file = stderr(), 'getClColHierSpar \n')
    
    loc.dend = userFitDendHierSpar()
    if (is.null(loc.dend))
      return(NULL)
    
    loc.cut = getClCol(loc.dend, input$inPlotHierSparNclust)
    
    
    return(loc.cut)
  })
  
  
  # return all unique track object labels (created in dataMod)
  # This will be used to display in UI for trajectory highlighting
  getDataTrackObjLabUni_afterTrim <- reactive({
    cat(file = stderr(), 'getDataTrackObjLabUni_afterTrim\n')
    loc.dt = in.data4clust()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(rownames(loc.dt))
  })
  
  # return dt with cell IDs and their corresponding condition name
  # The condition is the column defined by facet groupings
  getDataCond <- reactive({
    cat(file = stderr(), 'getDataCond\n')
    loc.dt = in.data4trajPlot()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[, .(id, group)]))
    
  })
  
  # prepare data for plotting trajectories per cluster
  # outputs dt as data4trajPlot but with an additional column 'cl' that holds cluster numbers
  # additionally some clusters are omitted according to manual selection
  data4trajPlotClSpar <- reactive({
    cat(file = stderr(), 'data4trajPlotClSpar: in\n')
    
    loc.dt = in.data4trajPlot()
    
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'data4trajPlotClSpar: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'data4trajPlotClSpar: dt not NULL\n')
    
    #cat('rownames: ', rownames(in.data4clust()), '\n')
    
    # get cellIDs with cluster assignments based on dendrogram cut
    loc.dt.cl = getDataClSpar(userFitDendHierSpar(), 
                              input$inPlotHierSparNclust, 
                              getDataTrackObjLabUni_afterTrim())
    
    ####
    ## PROBLEM!!!
    ## the dendrogram from sparse hier clust doesn't contain cellID's
    ## the following merge won't work...
    ## No idea how to solve it
    
    loc.dt = merge(loc.dt, loc.dt.cl, by = 'id')
    
    # display only selected clusters
    if(input$chBPlotHierSparClSel)
      loc.dt = loc.dt[cl %in% input$inPlotHierSparClSel]
    
    return(loc.dt)    
  })
  
  data4stimPlotClSpar <- reactive({
    cat(file = stderr(), 'data4stimPlotClSpar: in\n')
    
    loc.dt = in.data4stimPlot()
    
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'data4stimPlotClSpar: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'data4stimPlotClSpar: dt not NULL\n')
    return(loc.dt)
  })
  
  
  # download a list of cellIDs with cluster assignments
  output$downCellClSpar <- downloadHandler(
    filename = function() {
      paste0('clust_hierchSpar_data_',
             s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
             '_',
             s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.csv')
    },
    
    content = function(file) {
      write.csv(x = getDataClSpar(userFitDendHierSpar(), 
                                  input$inPlotHierSparNclust, 
                                  getDataTrackObjLabUni_afterTrim()), 
                file = file, row.names = FALSE)
    }
  )
  
  # prepare data for barplot with distribution of items per condition  
  data4clSparDistPlot <- reactive({
    cat(file = stderr(), 'data4clSparDistPlot: in\n')
    
    # get cell IDs with cluster assignments depending on dendrogram cut
    loc.dend <- userFitDendHierSpar()
    if (is.null(loc.dend)) {
      cat(file = stderr(), 'plotClSparDist: loc.dend is NULL\n')
      return(NULL)
    }
    
    # get cell id's with associated cluster numbers
    loc.dt.cl = getDataClSpar(loc.dend, input$inPlotHierSparNclust, getDataTrackObjLabUni_afterTrim())
    
    # get cellIDs with condition name
    loc.dt.gr = getDataCond()
    if (is.null(loc.dt.gr)) {
      cat(file = stderr(), 'plotClSparDist: loc.dt.gr is NULL\n')
      return(NULL)
    }
    
    loc.dt = merge(loc.dt.cl, loc.dt.gr, by = 'id')
    
    # display only selected clusters
    if(input$chBPlotHierSparClSel)
      loc.dt = loc.dt[cl %in% input$inPlotHierSparClSel]
    
    loc.dt.aggr = loc.dt[, .(nCells = .N), by = .(group, cl)]
    
    return(loc.dt.aggr)
    
  })
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  plotHierSpar <- function() {
    
    loc.dm = in.data4clust()
    if (is.null(loc.dm)) {
      return()
    }
    
    sparsehc <- userFitHierSpar()
    loc.dend <- userFitDendHierSpar()
    
    loc.colnames = paste0(ifelse(sparsehc$ws == 0, "",
                                 ifelse(
                                   sparsehc$ws <= 0.1,
                                   "* ",
                                   ifelse(sparsehc$ws <= 0.5, "** ", "*** ")
                                 )),  colnames(loc.dm))
    
    loc.colcol   = ifelse(sparsehc$ws == 0,
                          "black",
                          ifelse(
                            sparsehc$ws <= 0.1,
                            "blue",
                            ifelse(sparsehc$ws <= 0.5, "green", "red")
                          ))
    
    loc.col.bounds = NULL
    if (input$chBsetColBounds)
      loc.col.bounds = c(input$inSetColBoundsLow, input$inSetColBoundsHigh)
    else 
      loc.col.bounds = NULL
    
    
    loc.p = LOCplotHeatmap(loc.dm,
                           loc.dend, 
                           palette.arg = input$selectPlotHierSparPalette, 
                           palette.rev.arg = input$inPlotHierSparRevPalette, 
                           dend.show.arg = input$selectPlotHierSparDend, 
                           key.show.arg = input$selectPlotHierSparKey, 
                           margin.x.arg = input$inPlotHierSparMarginX, 
                           margin.y.arg = input$inPlotHierSparMarginY, 
                           nacol.arg = input$inPlotHierSparNAcolor, 
                           colCol.arg = loc.colcol,
                           labCol.arg = loc.colnames,
                           font.row.arg = input$inPlotHierSparFontX, 
                           font.col.arg = input$inPlotHierSparFontY, 
                           breaks.arg = loc.col.bounds,
                           title.arg = paste(
                             "Distance measure: ",
                             s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
                             "\nLinkage method: ",
                             s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)]
                           ))
    
    return(loc.p)
  }
  
  getPlotHierSparHeatMapHeight <- function() {
    return (input$inPlotHierSparHeatMapHeight)
  }
  
  createFnameHeatMap = reactive({
    
    paste0('clust_hierchSparse_heatMap_',
           s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
           '_',
           s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)],
           '.png')
  })
  
  createFnameTrajPlot = reactive({
    
    paste0('clust_hierchSparse_tCourses_',
           s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
           '_',
           s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], 
           '.pdf')
  })
  
  createFnameRibbonPlot = reactive({
    
    paste0('clust_hierchSparse_tCoursesMeans_',
           s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
           '_',
           s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], 
           '.pdf')
  })
  
  createFnamePsdPlot = reactive({
    
    paste0('clust_hierchSparse_tCoursesPsd_',
           s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
           '_',
           s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], 
           '.pdf')
  })
  
  createFnameDistPlot = reactive({
    
    paste0('clust_hierchSparse_clDist_',
           s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
           '_',
           s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.pdf')  })
  
  
  
  # Sparse Hierarchical - Heat Map - download pdf
  callModule(downPlot, "downPlotHierSparHM", createFnameHeatMap, plotHierSpar)
  
  # plot individual trajectories withina cluster  
  callModule(modTrajPlot, 'modPlotHierSparTraj', 
             in.data = data4trajPlotClSpar, 
             in.data.stim = data4stimPlotClSpar,
             in.facet = 'cl', 
             in.facet.color = getClColHierSpar,
             in.fname = createFnameTrajPlot)
  
  # plot cluster means
  callModule(modTrajRibbonPlot, 'modPlotHierSparTrajRibbon', 
             in.data = data4trajPlotClSpar, 
             in.data.stim = data4stimPlotClSpar,
             in.facet = 'cl',  
             in.facet.color = getClColHierSpar,
             in.fname = createFnameRibbonPlot)
  
  # plot cluster PSD
  callModule(modPSDPlot, 'modPlotHierSparPsd',
             in.data = data4trajPlotClSpar,
             in.facet = 'cl',
             in.facet.color = getClColHierSpar,
             in.fname = createFnamePsdPlot)
  
  # plot distribution barplot
  callModule(modClDistPlot, 'hierClSparDistPlot', 
             in.data = data4clSparDistPlot,
             in.cols = getClColHierSpar,
             in.fname = createFnameDistPlot)
  
  
  
  # Sparse Hierarchical - display heatmap
  output$outPlotHierSpar <- renderPlot({
    locBut = input$butPlotHierSparHeatMap
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotHierSpar: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotHierSpar()
  }, height = getPlotHierSparHeatMapHeight)

  addPopover(session, 
             ns("alImportance"),
             title = "Variable importance",
             content = paste0("<p>Weight factors (WF) calculated during clustering ",
                              "reflect the importance of time points in the clustering. ",
                              "The following labels are used to indicate the importance:",
                              "<li>Black - time point not taken into account</li>",
                              "<li><p, style=\"color:DodgerBlue;\">* - low, WF∈(0, 0.1]</p></li>",
                              "<li><p, style=\"color:MediumSeaGreen;\">** - medium, WF∈(0.1, 0.5]</p></li>",
                              "<li><p, style=\"color:Tomato;\">*** - high, WF∈(0.5, 1.0]</p></li>",
                              "</p><p>Witten and Tibshirani (2010): ",
                              "<i>A framework for feature selection in clustering</i>; ",
                              "Journal of the American Statistical Association 105(490): 713-726.</p>"),
             trigger = "click")

}


