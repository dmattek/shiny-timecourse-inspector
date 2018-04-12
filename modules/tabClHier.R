
# UI
clustHierUI <- function(id, label = "Hierarchical CLustering") {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(4,
             selectInput(
               ns("selectPlotHierLinkage"),
               label = ("Select linkage method:"),
               choices = list(
                 "Ward" = 1,
                 "Ward D2" = 2,
                 "Single" = 3,
                 "Complete" = 4,
                 "Average" = 5,
                 "McQuitty" = 6,
                 "Centroid" = 7
               ),
               selected = 2
             ),
             selectInput(
               ns("selectPlotHierDiss"),
               label = ("Select type of dissimilarity measure:"),
               choices = list("Euclidean" = 1,
                              "Maximum" = 2,
                              "Manhattan" = 3,
                              "Canberra" = 4,
                              "Binary" = 5,
                              "Minkowski" = 6,
                              "DTW" = 7),
               selected = 1
             )
      ),
      column(4,
             sliderInput(
               ns('inPlotHierNclust'),
               '#dendrogram branches to colour',
               min = 1,
               max = 20,
               value = 1,
               step = 1,
               ticks = TRUE,
               round = TRUE
             ),
             checkboxInput(ns('chBPlotHierClSel'), 'Manually select clusters to display'),
             uiOutput(ns('uiPlotHierClSel')),
             downloadButton(ns('downCellCl'), 'Download CSV with cell IDs and cluster no.')
      )
    ),
    
    br(),

    tabsetPanel(
      tabPanel('Heat-map',
               fluidRow(
                 column(3,
                        checkboxInput(ns('selectPlotHierDend'), 'Plot dendrogram and re-order samples', TRUE),
                        selectInput(
                          ns("selectPlotHierPaletteDend"),
                          label = "Dendrogram\'s colour palette:",
                          choices = l.col.pal.dend,
                          selected = 'Rainbow'
                        ),

                        checkboxInput(ns('selectPlotHierKey'), 'Plot colour key', TRUE)
                 ),
                 column(3,
                        selectInput(
                          ns("selectPlotHierPalette"),
                          label = "Heatmap\'s colour palette:",
                          choices = l.col.pal,
                          selected = 'Spectral'
                        ),
                        checkboxInput(ns('inPlotHierRevPalette'), 'Reverse heatmap\'s colour palette', TRUE),
                        sliderInput(
                          ns('inPlotHierNAcolor'),
                          'Shade of grey for NA values (0 - black, 1 - white)',
                          min = 0,
                          max = 1,
                          value = 0.8,
                          step = .1,
                          ticks = TRUE
                        )
                 ),
                 column(6,
                        h4('Classic hierarchical clustering'),
                        br(),
                        numericInput(ns('inPlotHierHeatMapHeight'), 
                                     'Display plot height [px]', 
                                     value = 600, 
                                     min = 100,
                                     step = 100)
                        
                 )
               ),
               
               fluidRow(
                 column(
                   3,
                   numericInput(
                     ns('inPlotHierMarginX'),
                     'Margin below x-axis',
                     5,
                     min = 1,
                     width = 100
                   )
                 ),
                 column(
                   3,
                   numericInput(
                     ns('inPlotHierMarginY'),
                     'Margin right of y-axis',
                     20,
                     min = 1,
                     width = 100
                   )
                 ),
                 column(
                   3,
                   numericInput(
                     ns('inPlotHierFontX'),
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
                     ns('inPlotHierFontY'),
                     'Font size column labels',
                     1,
                     min = 0,
                     width = 100,
                     step = 0.1
                   )
                 )
               ),
               br(),
               
               downPlotUI(ns('downPlotHier'), "Download PDF"),
               actionButton(ns('butPlotHierHeatMap'), 'Plot!'),
               plotOutput(ns('outPlotHier'))
      ),

      tabPanel('Averages',
               modTrajRibbonPlotUI(ns('modPlotHierTrajRibbon'))),
      
      tabPanel('Time-courses',
               modTrajPlotUI(ns('modPlotHierTraj'))),
      
      tabPanel('Cluster dist.',
               modClDistPlotUI(ns('hierClDistPlot'), 'xxx'))
      
    )
  )
}

# SERVER
clustHier <- function(input, output, session, in.data4clust, in.data4trajPlot) {
  
  output$uiPlotHierClSel = renderUI({
    ns <- session$ns
    
    if(input$chBPlotHierClSel) {
      selectInput(ns('inPlotHierClSel'), 'Select clusters to display', 
                  choices = seq(1, input$inPlotHierNclust, 1),
                  multiple = TRUE, 
                  selected = 1)
    }
  })
  
  # calculate distance matrix for further clustering
  # time series arranged in rows with columns corresponding to time points
  userFitDistHier <- reactive({
    cat(file = stderr(), 'userFitDistHier \n')
    
    dm.t = in.data4clust()

    if (is.null(dm.t)) {
      return(NULL)
    }
    
    #pr_DB$set_entry(FUN = fastDTW, names = c("fastDTW"))
    cl.dist = dist(dm.t, method = s.cl.diss[as.numeric(input$selectPlotHierDiss)])
    
    return(cl.dist)
  })
  
  # perform hierarchical clustering and return dendrogram coloured according to cutree
  # branch coloring performed using dendextend package
  userFitDendHier <- reactive({
    cat(file = stderr(), 'userFitDendHier \n')
    
    dm.dist = userFitDistHier()
    
    if (is.null(dm.dist)) {
      return(NULL)
    }
    
    cl.hc = hclust(dm.dist, method = s.cl.linkage[as.numeric(input$selectPlotHierLinkage)])

    dend <- as.dendrogram(cl.hc)
    dend <- color_branches(dend, 
                           col = get(input$selectPlotHierPaletteDend), # make sure that n here equals max in the input$inPlotHierNclust slider
                           k = input$inPlotHierNclust)
    
    return(dend)
  })
  

  # returns table prepared with f-n getClCol
  # for hierarchical clustering
  getClColHier <- reactive({
    cat(file = stderr(), 'getClColHier \n')
    
    loc.dend = userFitDendHier()
    if (is.null(loc.dend))
      return(NULL)
    
    loc.dt = getClCol(loc.dend, input$inPlotHierNclust)
    
    # Display clusters specified in the inPlotHierClSel field
    # Data is ordered according to the order of clusters specified in this field
    if(input$chBPlotHierClSel) {
      loc.dt = loc.dt[cl.no %in% input$inPlotHierClSel]
      loc.dt[, cl.no := factor(cl.no, levels = input$inPlotHierClSel)]
      setkey(loc.dt, cl.no)
    }
    
    return(loc.dt)
  })
  
  
  
  # return all unique track object labels (created in dataMod)
  # This will be used to display in UI for trajectory highlighting
  getDataTrackObjLabUni_afterTrim <- reactive({
    cat(file = stderr(), 'getDataTrackObjLabUni_afterTrim\n')
    loc.dt = in.data4trajPlot()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt$id))
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
  data4trajPlotCl <- reactive({
    cat(file = stderr(), 'data4trajPlotCl: in\n')
    
    loc.dt = in.data4trajPlot()
    
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'data4trajPlotCl: dt is NULL\n')
      return(NULL)
    }
    
    cat(file = stderr(), 'data4trajPlotCl: dt not NULL\n')
    
    # get cellIDs with cluster assignments based on dendrogram cut
    loc.dt.cl = getDataCl(userFitDendHier(), input$inPlotHierNclust)
    loc.dt = merge(loc.dt, loc.dt.cl, by = 'id')
    
    # Display clusters specified in the inPlotHierClSel field
    # Data is ordered according to the order of clusters specified in this field
    if(input$chBPlotHierClSel) {
      loc.dt = loc.dt[cl %in% input$inPlotHierClSel]
      loc.dt[, cl := factor(cl, levels = input$inPlotHierClSel)]
      setkey(loc.dt, cl)
    }
    
    return(loc.dt)    
  })
  
  # download a list of cellIDs with cluster assignments
  output$downCellCl <- downloadHandler(
    filename = function() {
      paste0('clust_hierch_data_',
             s.cl.diss[as.numeric(input$selectPlotHierDiss)],
             '_',
             s.cl.linkage[as.numeric(input$selectPlotHierLinkage)], '.csv')
    },
    
    content = function(file) {
      write.csv(x = getDataCl(userFitDendHier(), input$inPlotHierNclust), file = file, row.names = FALSE)
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
    loc.dt.cl = getDataCl(loc.dend, input$inPlotHierNclust)
    
    # get cellIDs with condition name
    loc.dt.gr = getDataCond()
    if (is.null(loc.dt.gr)) {
      cat(file = stderr(), 'plotClDist: loc.dt.gr is NULL\n')
      return(NULL)
    }
    
    loc.dt = merge(loc.dt.cl, loc.dt.gr, by = 'id')
    
      
    loc.dt.aggr = loc.dt[, .(nCells = .N), by = .(group, cl)]
    
    # Display clusters specified in the inPlotHierClSel field
    # Data is ordered according to the order of clusters specified in this field
    if(input$chBPlotHierClSel) {
      loc.dt.aggr = loc.dt.aggr[cl %in% input$inPlotHierClSel]
      loc.dt.aggr[, cl := factor(cl, levels = input$inPlotHierClSel)]
      setkey(loc.dt.aggr, cl)
    }
    return(loc.dt.aggr)
    
  })
  
  createMethodStr = reactive({

    paste0(s.cl.diss[as.numeric(input$selectPlotHierDiss)],
    '_',
    s.cl.linkage[as.numeric(input$selectPlotHierLinkage)])
    
  })
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  plotHier <- function() {
    
    loc.dm = in.data4clust()
    if (is.null(loc.dm))
      return(NULL)
    
    loc.dend <- userFitDendHier()
    if (is.null(loc.dend))
      return(NULL)
    
    loc.p = myPlotHeatmap(loc.dm,
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
                          title.arg = paste0(
                            "Distance measure: ",
                            s.cl.diss[as.numeric(input$selectPlotHierDiss)],
                            "\nLinkage method: ",
                            s.cl.linkage[as.numeric(input$selectPlotHierLinkage)]
                          ))
    
    return(loc.p)
  }
  
  
   
  #  Hierarchical - display heatmap
  getPlotHierHeatMapHeight <- function() {
    return (input$inPlotHierHeatMapHeight)
  }
 
  output$outPlotHier <- renderPlot({
    locBut = input$butPlotHierHeatMap
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotHier: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotHier()
  }, height = getPlotHierHeatMapHeight)
  
  createFnameHeatMap = reactive({
    
    paste0('clust_hierch_heatMap_',
           s.cl.diss[as.numeric(input$selectPlotHierDiss)],
           '_',
           s.cl.linkage[as.numeric(input$selectPlotHierLinkage)],
           '.png')
  })
  
  createFnameTrajPlot = reactive({
    
    paste0('clust_hierch_tCourses_',
           s.cl.diss[as.numeric(input$selectPlotHierDiss)],
           '_',
           s.cl.linkage[as.numeric(input$selectPlotHierLinkage)], 
           '.pdf')
  })
  
  createFnameRibbonPlot = reactive({
    
    paste0('clust_hierch_tCoursesMeans_',
           s.cl.diss[as.numeric(input$selectPlotHierDiss)],
           '_',
           s.cl.linkage[as.numeric(input$selectPlotHierLinkage)], 
           '.pdf')
  })
  
  createFnameDistPlot = reactive({
    
    paste0('clust_hierch_clDist_',
           s.cl.diss[as.numeric(input$selectPlotHierDiss)],
           '_',
           s.cl.linkage[as.numeric(input$selectPlotHierLinkage)], '.pdf')  })
  
  
  
  #  Hierarchical - Heat Map - download pdf
  callModule(downPlot, "downPlotHier", createFnameHeatMap, plotHier)
  
  callModule(modTrajPlot, 'modPlotHierTraj', 
             in.data = data4trajPlotCl, 
             in.facet = 'cl',  
             in.facet.color = getClColHier,
             in.fname = createFnameTrajPlot)
  
  callModule(modTrajRibbonPlot, 'modPlotHierTrajRibbon', 
             in.data = data4trajPlotCl, 
             in.facet = 'cl',  
             in.facet.color = getClColHier,
             in.fname = createFnameRibbonPlot)
  
  callModule(modClDistPlot, 'hierClDistPlot', 
             in.data = data4clDistPlot,
             in.cols = getClColHier,
             in.fname = createFnameDistPlot)

  
}