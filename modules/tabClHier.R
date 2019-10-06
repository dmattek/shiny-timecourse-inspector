#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for hierarchical clustering (base R hclust + dist)

helpText.clHier = c(alertNAsPresentDTW = paste0("NAs present. DTW cannot calculate the distance. ",
                                                "Consider interpolation of NAs and missing data in the left panel."),
                    alertNAsPresent = paste0("NAs present. The selected distance measure will work, ",
                                             "however caution is recommended. Consider interpolation of NAs and missing data the left panel."))


# UI ----
clustHierUI <- function(id, label = "Hierarchical Clustering") {
  ns <- NS(id)
  
  tagList(
    h4('Hierarchical clustering'),
    p("Standard approach using R's ",
      a("dist", 
        href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html",
        title="External link"),
      " and ",
      a("hclust", 
        href = "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html",
        title="External link"),
      " functions."),
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
               ns('inPlotHierNclust'),
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
      
      tabPanel('Averages',
               br(),
               modTrajRibbonPlotUI(ns('modPlotHierTrajRibbon'))),
      
      tabPanel('Time series',
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
clustHier <- function(input, output, session, in.data4clust, in.data4trajPlot, in.data4stimPlot) {
  
  output$uiPlotHierClAss = renderUI({
    ns <- session$ns
    
    if(input$chBPlotHierClAss) {
      selectInput(ns('inPlotHierClAss'), 'Assign cluster order', 
                  choices = seq(1, input$inPlotHierNclust, 1),
                  multiple = TRUE, 
                  selected = seq(1, input$inPlotHierNclust, 1))
    }
  })
  
  output$uiPlotHierClSel = renderUI({
    ns <- session$ns
    
    if(input$chBPlotHierClSel) {
      selectInput(ns('inPlotHierClSel'), 'Select clusters to display', 
                  choices = seq(1, input$inPlotHierNclust, 1),
                  multiple = TRUE, 
                  selected = 1)
    }
  })
  
  # UI for setting lower and upper bounds for heat map colour scale  
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
  
  # calculate distance matrix for further clustering
  # time series arranged in rows with columns corresponding to time points
  userFitDistHier <- reactive({
    cat(file = stderr(), 'userFitDistHier \n')
    
    dm.t = in.data4clust()
    
    if (is.null(dm.t)) {
      return(NULL)
    }
    
    #pr_DB$set_entry(FUN = fastDTW, names = c("fastDTW"))
    cl.dist = dist(dm.t, method = input$selectPlotHierDiss)
    
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
    
    cl.hc = hclust(dm.dist, method = input$selectPlotHierLinkage)
    
    # number of clusters at which dendrigram is cut
    loc.k = input$inPlotHierNclust
    
    # make a palette with the amount of colours equal to the number of clusters
    #loc.col = get(input$selectPlotHierPaletteDend)(n = loc.k)
    loc.col = ggthemes::tableau_color_pal(input$selectPlotHierPaletteDend)(n = loc.k)
    
    # take into account manual assignment of cluster numbers
    # NOT USED AT THE MOMENT
    #if (input$chBPlotHierClAss) {
    #  loc.col = loc.col[as.numeric(input$inPlotHierClAss)]
    #}
    
    dend <- as.dendrogram(cl.hc)
    dend <- color_branches(dend, 
                           col = loc.col, 
                           k = loc.k)
    
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
    
    loc.dt = in.data4stimPlot()
    
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
    
    loc.dt = merge(loc.dt.cl, loc.dt.gr, by = COLID)
    
    
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
    
    paste0(input$selectPlotHierDiss,
           '_',
           input$selectPlotHierLinkage)
    
  })
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  plotHier <- function() {
    
    loc.dm = in.data4clust()
    if (is.null(loc.dm))
      return(NULL)
    
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
    
    loc.dend <- userFitDendHier()
    if (is.null(loc.dend))
      return(NULL)
    
    loc.col.bounds = NULL
    if (input$chBsetColBounds)
      loc.col.bounds = c(input$inSetColBoundsLow, input$inSetColBoundsHigh)
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
    locBut = input$butPlotHierHeatMap
    
    if (locBut == 0) {
      cat(file = stderr(), 'outPlotHier: Go button not pressed\n')
      
      return(NULL)
    }
    
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
             in.facet = 'cl',  
             in.facet.color = getClColHier,
             in.fname = createFnameTrajPlot)
  
  # plot cluster means
  callModule(modTrajRibbonPlot, 'modPlotHierTrajRibbon', 
             in.data = data4trajPlotCl, 
             in.data.stim = data4stimPlotCl,
             in.facet = 'cl',  
             in.facet.color = getClColHier,
             in.fname = createFnameRibbonPlot)
  
  # plot cluster PSD
  callModule(modPSDPlot, 'modPlotHierPsd',
             in.data = data4trajPlotCl,
             in.facet = 'cl',
             in.facet.color = getClColHier,
             in.fname = createFnamePsdPlot)
  
  # plot distribution barplot
  callModule(modClDistPlot, 'hierClDistPlot', 
             in.data = data4clDistPlot,
             in.cols = getClColHier,
             in.fname = createFnameDistPlot)
  
  
}