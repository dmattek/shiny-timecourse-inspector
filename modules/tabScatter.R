#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is a tab for plotting scatter plots between two time points
#
# Use:
# in ui.R
# tabPanel(
#  'Hierarchical',
#  clustHierUI('TabClustHier'))
#
# in server.R
# callModule(clustHier, 'TabClustHier', dataMod)
# where dataMod is the output from a reactive function that returns dataset ready for clustering

# UI ----
tabScatterPlotUI <- function(id, label = "Comparing t-points") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Scatter plot between two time points"
    ),
    br(),
    
    fluidRow(
      column(
        4,
        uiOutput(ns('uiSelTptX')),
        uiOutput(ns('uiSelTptY')),
        checkboxInput(ns('chBfoldChange'), 'Difference between two time points on Y-axis'),
        bsTooltip(ns('chBfoldChange'), help.text.short[15], placement = "right", trigger = "hover", options = NULL),
        checkboxInput(ns('chBregression'), 'Linear regression with 95% CI'),
        bsTooltip(ns('chBregression'), help.text.short[16], placement = "right", trigger = "hover", options = NULL)
      ),
      column(
        4, 
        numericInput(ns('inNeighTpts'), 'Time points left & right:', value = 0, step = 1, min = 0),
        bsTooltip(ns('inNeighTpts'), help.text.short[17], placement = "right", trigger = "hover", options = NULL),
        radioButtons(ns('rBstats'), 'Operation:', list('Mean' = 1, 'Min' = 2, 'Max' = 3)),
        bsTooltip(ns('inNeighTpts'), help.text.short[18], placement = "right", trigger = "hover", options = NULL)
      ),
      column(
        4,
        numericInput(
          ns('inPlotHeight'),
          'Display plot height [px]',
          value = PLOTSCATTERHEIGHT,
          min = 100,
          step = 100
        ),
        numericInput(
          ns('inPlotNcolFacet'),
          '#Columns',
          value = PLOTNFACETDEFAULT,
          min = 1,
          step = 1
        )
      )
    ),
    
    br(),
    checkboxInput(ns('plotInt'), 
                  'Interactive Plot?',
                  value = FALSE),
    actionButton(ns('butGoScatter'), 'Plot!'),
    uiOutput(ns("plotInt_ui")),
    downPlotUI(ns('downPlotScatter'), "Download PDF")
  )
}

# SERVER ----
tabScatterPlot <- function(input, output, session, in.data, in.fname) {
  
# return all unique time points (real time)
# This will be used to display in UI for box-plot
# These timepoints are from the original dt and aren't affected by trimming of x-axis
getDataTpts <- reactive({
  cat(file = stderr(), 'getDataTpts\n')
  loc.dt = in.data()
  
  if (is.null(loc.dt))
    return(NULL)
  else
    return(unique(loc.dt[[COLRT]]))
})

output$uiSelTptX = renderUI({
  cat(file = stderr(), 'UI uiSelTptX\n')
  
  ns <- session$ns
  
  loc.v = getDataTpts()
  if (!is.null(loc.v)) {
    selectInput(
      ns('inSelTptX'),
      'Time point for X-axis:',
      loc.v,
      width = '100%',
      selected = 0,
      multiple = FALSE
    )
  }
})

output$uiSelTptY = renderUI({
  cat(file = stderr(), 'UI uiSelTptY\n')
  
  ns <- session$ns
  
  loc.v = getDataTpts()
  if (!is.null(loc.v)) {
    selectInput(
      ns('inSelTptY'),
      'Time point for Y-axis:',
      loc.v,
      width = '100%',
      selected = 0,
      multiple = FALSE
    )
  }
})

data4scatterPlot <- reactive({
  cat(file = stderr(), 'data4scatterPlot\n')
  
  loc.dt.in = in.data()
  if(is.null(loc.dt.in))
    return(NULL)
  
  loc.tpts.x = input$inSelTptX
  loc.tpts.y = input$inSelTptY
  
  # if neigbbouring points selected
  if (input$inNeighTpts > 0) {
    loc.dt.in.tpts = unique(loc.dt.in[[COLRT]])
    
    loc.tpts.x.id = seq(which(loc.dt.in.tpts == loc.tpts.x) - input$inNeighTpts, which(loc.dt.in.tpts == loc.tpts.x) + input$inNeighTpts, 1)
    loc.tpts.y.id = seq(which(loc.dt.in.tpts == loc.tpts.y) - input$inNeighTpts, which(loc.dt.in.tpts == loc.tpts.y) + input$inNeighTpts, 1)
    
    loc.tpts.x.id = loc.tpts.x.id[loc.tpts.x.id > 0]
    loc.tpts.y.id = loc.tpts.y.id[loc.tpts.y.id > 0]
    
    loc.tpts.x = loc.dt.in.tpts[loc.tpts.x.id]
    loc.tpts.y = loc.dt.in.tpts[loc.tpts.y.id]
    
    #cat(loc.tpts.x.id, '\n')
    #cat(loc.tpts.y.id, '\n')

  } 

  #cat(loc.tpts.x, '\n')
  #cat(loc.tpts.y, '\n')
  
  if (input$rBstats == 1) {
    loc.dt.x = loc.dt.in[get(COLRT) %in% loc.tpts.x, .(y.aggr = mean(y)), by = c(COLGR, COLID)]
    loc.dt.y = loc.dt.in[get(COLRT) %in% loc.tpts.y, .(y.aggr = mean(y)), by = c(COLGR, COLID)]
  } else if (input$rBstats == 2) {
    loc.dt.x = loc.dt.in[get(COLRT) %in% loc.tpts.x, .(y.aggr = min(y)), by = c(COLGR, COLID)]
    loc.dt.y = loc.dt.in[get(COLRT) %in% loc.tpts.y, .(y.aggr = min(y)), by = c(COLGR, COLID)]
  } else {
    loc.dt.x = loc.dt.in[get(COLRT) %in% loc.tpts.x, .(y.aggr = max(y)), by = c(COLGR, COLID)]
    loc.dt.y = loc.dt.in[get(COLRT) %in% loc.tpts.y, .(y.aggr = max(y)), by = c(COLGR, COLID)]
  }

  loc.dt = merge(loc.dt.x, loc.dt.y, by = COLID)
  
  loc.dt[, group.y := NULL]
  setnames(loc.dt, c('group.x', 'y.aggr.x', 'y.aggr.y'), c(COLGR, 'x', 'y'))

  if (input$chBfoldChange) {
    loc.dt[ , y := y - x]
  }
  return(loc.dt)
  
})

  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  # This function is used to plot and to downoad a pdf
  
plotScatter <- function() {
  cat(file=stderr(), "plotScatter\n")
  
  # isolate because calculations & plotting take a while
  # re-plotting done upon button press
  loc.dt = isolate(data4scatterPlot())

  cat("plotScatter on to plot\n\n")
  if (is.null(loc.dt)) {
    cat(file=stderr(), 'plotScatter: dt is NULL\n')
    return(NULL)
  }
  
  cat(file=stderr(), 'plotScatter:dt not NULL\n')
  
  
  ## FIX: r.squared is unavailable for lm  
  
  #     loc.fit.rsq = ifelse(input$inRobustFit, loc.fit$r.squared, )

  p.out = LOCggplotScat(
    dt.arg = loc.dt,
    plotlab.arg = NULL,
    facet.arg = COLGR,
    facet.ncol.arg = input$inPlotNcolFacet,
    alpha.arg = 0.5,
    trend.arg = input$chBregression,
    ci.arg = 0.95
  )
  return(p.out)
}

# display plot
output$outPlotScatter <- renderPlot({
  locBut = input$butGoScatter
  
  if (locBut == 0) {
    cat(file=stderr(), 'plotScatter: Go button not pressed\n')
    
    return(NULL)
  }
  
  plotScatter()
})



output$outPlotScatterInt <- renderPlotly({
  # This is required to avoid 
  # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
  # When running on a server. Based on:
  # https://github.com/ropensci/plotly/issues/494
  
  locBut = input$butGoScatter
  if (locBut == 0) {
    cat(file=stderr(), 'plotScatterInt Go button not pressed\n')
    return(NULL)
  }
  
  if (names(dev.cur()) != "null device") dev.off()
  pdf(NULL)

  return(plotly_build(plotScatter()))
  
})

  # download pdf
  callModule(downPlot, "downPlotScatter", in.fname, plotScatter, TRUE)
  
  # Hierarchical - choose to display regular heatmap.2 or d3heatmap (interactive)
  output$plotInt_ui <- renderUI({
    ns <- session$ns
    if (input$plotInt)
      tagList( withSpinner(plotlyOutput(ns("outPlotScatterInt"), height = paste0(input$inPlotHeight, "px"))))
    else
      tagList( withSpinner(plotOutput(ns('outPlotScatter'), height = paste0(input$inPlotHeight, "px"))))
  })
  
}