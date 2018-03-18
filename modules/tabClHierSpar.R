
# UI
clustHierSparUI <- function(id, label = "Sparse Hierarchical CLustering") {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(
        4,
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
        ),
        selectInput(
          ns("selectPlotHierSparDiss"),
          label = ("Select type of dissimilarity measure:"),
          choices = list("Squared Distance" = 1,
                         "Absolute Value" = 2),
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
        downloadButton(ns('downCellClSpar'), 'Download CSV with cell IDs and cluster no.')
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
      tabPanel('Heat-map',
               fluidRow(
                 column(3,
                        checkboxInput(ns('selectPlotHierSparDend'), 'Plot dendrogram and re-order samples', TRUE),
                        selectInput(
                          ns("selectPlotHierSparPalette"),
                          label = "Select colour palette:",
                          choices = l.col.pal,
                          selected = 'Spectral'
                        ),
                        checkboxInput(ns('inPlotHierSparRevPalette'), 'Reverse colour palette', TRUE),
                        checkboxInput(ns('selectPlotHierSparKey'), 'Plot colour key', TRUE)
                 ),
                 column(3,
                        sliderInput(
                          ns('inPlotHierSparNAcolor'),
                          'Shade of grey for NA values (0 - black, 1 - white)',
                          min = 0,
                          max = 1,
                          value = 0.8,
                          step = .1,
                          ticks = TRUE
                        ),
                        numericInput(ns('inPlotHierSparHeatMapHeight'), 
                                     'Display plot height [px]', 
                                     value = 600, 
                                     min = 100,
                                     step = 100)
                 ),
                 column(6,
                        br(),
                        h4(
                          "Sparse hierarchical clustering using ",
                          a("sparcl", href = "https://cran.r-project.org/web/packages/sparcl/")
                        ),
                        p(
                          'Column labels in the heat-map are additionally labeld according to their \"importance\":'
                        ),
                        tags$ol(
                          tags$li("Black - not taken into account"),
                          tags$li("Blue with \"*\" - low importance (weight factor in (0, 0.1]"),
                          tags$li("Green with \"**\" - medium importance (weight factor in (0.1, 0.5]"),
                          tags$li("Red with \"***\" - high importance (weight factor in (0.5, 1.0]")
                        )
                 )
               ),
               
               fluidRow(
                 column(
                   3,
                   numericInput(
                     ns('inPlotHierSparMarginX'),
                     'Margin below x-axis',
                     5,
                     min = 1,
                     width = 100
                   )
                 ),
                 column(
                   3,
                   numericInput(
                     ns('inPlotHierSparMarginY'),
                     'Margin right of y-axis',
                     20,
                     min = 1,
                     width = 100
                   )
                 ),
                 column(
                   3,
                   numericInput(
                     ns('inPlotHierSparFontX'),
                     'Font size row labels',
                     1,
                     min = 0,
                     width = 100,
                     step = 0.1
                   )
                 ),
                 column(
                   3,
                   numericInput(
                     ns('inPlotHierSparFontY'),
                     'Font size column labels',
                     1,
                     min = 0,
                     width = 100,
                     step = 0.1
                   )
                 )
               ),
               br(),
               
               
               downPlotUI(ns('downPlotHierSparHM'), "Download PDF"),
               
               actionButton(ns('butPlotHierSparHeatMap'), 'Plot!'),
               plotOutput(ns('outPlotHierSpar'))
      ),

      tabPanel('Averages',
               modTrajRibbonPlotUI(ns('modPlotHierSparTrajRibbon'))),
      
      tabPanel('Time-courses',
               modTrajPlotUI(ns('modPlotHierSparTraj'))),
      
      tabPanel('Cluster dist.',
               modClDistPlotUI(ns('hierClSparDistPlot')))
    )
  )
}

# SERVER
clustHierSpar <- function(input, output, session, in.data4clust, in.data4trajPlot) {

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
    
    dend <- as.dendrogram(sparsehc$hc)
    
    #cat('=============\ncutree:\n', dendextend::cutree(dend, input$inPlotHierSparNclust, order_clusters_as_data = TRUE), '\n')
    
    dend <- color_branches(dend, 
                           col = rainbow_hcl,
                           k = input$inPlotHierSparNclust)
    
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
    loc.dt.cl = getDataClSpar(userFitDendHierSpar(), input$inPlotHierSparNclust, getDataTrackObjLabUni_afterTrim())

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
  
  
  # download a list of cellIDs with cluster assignments
  output$downCellClSpar <- downloadHandler(
    filename = function() {
      paste0('clust_hierchSpar_data_',
             s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
             '_',
             s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.csv')
    },
    
    content = function(file) {
      write.csv(x = getDataClSpar(userFitDendHierSpar(), input$inPlotHierSparNclust, getDataTrackObjLabUni_afterTrim()), file = file, row.names = FALSE)
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
    
    loc.p = myPlotHeatmap(loc.dm,
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
  
  
  callModule(modTrajPlot, 'modPlotHierSparTraj', 
             in.data = data4trajPlotClSpar, 
             in.facet = 'cl', 
             in.facet.color = getClColHierSpar,
             paste0('clust_hierchSparse_tCourses_',
                    s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
                    '_',
                    s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.pdf'))
  
  callModule(modTrajRibbonPlot, 'modPlotHierSparTrajRibbon', 
             in.data = data4trajPlotClSpar, 
             in.facet = 'cl',  
             in.facet.color = getClColHierSpar,
             in.fname = paste0('clust_hierchSparse_tCoursesMeans_',
                               s.cl.diss[as.numeric(input$selectPlotHierSparDiss)],
                               '_',
                               s.cl.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.pdf'))
  
  
  callModule(modClDistPlot, 'hierClSparDistPlot', 
             in.data = data4clSparDistPlot,
             in.cols = getClColHierSpar,
             in.fname = paste0('clust_hierchSparse_clDist_',
                               s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
                               '_',
                               s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.pdf'))
  
  
  
  # Sparse Hierarchical - display heatmap
  output$outPlotHierSpar <- renderPlot({
    locBut = input$butPlotHierSparHeatMap
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotHierSpar: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotHierSpar()
  }, height = getPlotHierSparHeatMapHeight)
  
  # Sparse Hierarchical - Heat Map - download pdf
  callModule(downPlot, "downPlotHierSparHM",       paste0('clust_hierchSparse_heatMap_',
                                                          s.cl.spar.diss[as.numeric(input$selectPlotHierSparDiss)],
                                                          '_',
                                                          s.cl.spar.linkage[as.numeric(input$selectPlotHierSparLinkage)], '.png'), plotHierSpar)
  
  
}