# RShiny module for performing hierarchical clustering
# Use:
# in ui.R
# tabPanel(
#  'Hierarchical',
#  clustHierUI('TabClustHier'))
#
# in server.R
# callModule(clustHier, 'TabClustHier', dataMod)
# where dataMod is the output from a reactive function that returns dataset ready for clustering


require(plotly) # interactive plot
require(robust)

# UI
tabScatterPlotUI <- function(id, label = "Comparing t-points") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Scatter plot between two time points"
    ),
    br(),
    
    fluidRow(
      column(
        6,
        uiOutput(ns('varSelTptX')),
        uiOutput(ns('varSelTptY'))
      ),
      column(
        6,
        numericInput(
          ns('inPlotHeight'),
          'Display plot height',
          value = 1000,
          min = 100,
          step = 100
        ),
        numericInput(
          ns('inPlotNcolFacet'),
          '#columns',
          value = 2,
          min = 1,
          step = 1
        )
      )
    ),
    
    br(),
    actionButton(ns('butGoScatter'), 'Plot!'),
    checkboxInput(ns('plotInt'), 
                  'Interactive Plot?',
                  value = FALSE),
    uiOutput(ns("plotInt_ui")),
    downPlotUI(ns('downPlotScatter'), "Download PDF")
  )
}

# SERVER
tabScatterPlot <- function(input, output, session, in.data) {
  
# return all unique time points (real time)
# This will be used to display in UI for box-plot
# These timepoints are from the original dt and aren't affected by trimming of x-axis
getDataTpts <- reactive({
  cat(file = stderr(), 'getDataTpts\n')
  loc.dt = in.data()
  
  if (is.null(loc.dt))
    return(NULL)
  else
    return(unique(loc.dt$realtime))
})

output$varSelTptX = renderUI({
  cat(file = stderr(), 'UI varSelTptX\n')
  
  ns <- session$ns
  
  loc.v = getDataTpts()
  if (!is.null(loc.v)) {
    selectInput(
      ns('inSelTptX'),
      'Select timepoint for X-axis:',
      loc.v,
      width = '100%',
      selected = 0,
      multiple = FALSE
    )
  }
})

output$varSelTptY = renderUI({
  cat(file = stderr(), 'UI varSelTptY\n')
  
  ns <- session$ns
  
  loc.v = getDataTpts()
  if (!is.null(loc.v)) {
    selectInput(
      ns('inSelTptY'),
      'Select timepoint for Y-axis:',
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
  
  loc.dt = data.table(x = loc.dt.in[realtime == input$inSelTptX, y],  
                      y = loc.dt.in[realtime == input$inSelTptY, y],
                      group = loc.dt.in[realtime == input$inSelTptX, group])
  
  
  loc.dt.x = loc.dt.in[realtime == input$inSelTptX]
  loc.dt.y = loc.dt.in[realtime == input$inSelTptY]
  loc.dt = merge(loc.dt.x, loc.dt.y, by = 'id')
  
  setnames(loc.dt, c('group.x', 'y.x', 'y.y'), c('group', 'x', 'y'))

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
  #loc.fit = isolate(dataFit())
  
  cat("plotScatter on to plot\n\n")
  if (is.null(loc.dt)) {
    cat(file=stderr(), 'plotScatter: dt is NULL\n')
    return(NULL)
  }
  
  cat(file=stderr(), 'plotScatter:dt not NULL\n')
  
  
  ## FIX: r.squared is unavailable for lm  
  
  #     loc.fit.rsq = ifelse(input$inRobustFit, loc.fit$r.squared, )

  p.out = myGgplotScat(
    dt.arg = loc.dt,
    band.arg = NULL, #list(a = loc.fit$coeff.a, b = loc.fit$coeff.b, width = input$inBandWidth),
    group.col.arg = NULL,
    plotlab.arg = NULL,
    # plotlab.arg = sprintf(
    #   "%s%.2f\n%s%.2f x %.2f",
    #   ifelse(input$inRobustFit, "lmRob, entire dataset R2=", "lm, entire dataset R2="),
    #   loc.fit$r.squared,
    #   'bandwidth=',
    #   input$inBandWidth,
    #   loc.fit$coeff.b
    # ),
    facet.arg = 'group',
    facet.ncol.arg = input$inPlotNcolFacet,
    alpha.arg = 0.5
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
  if (names(dev.cur()) != "null device") dev.off()
  pdf(NULL)

  return( plotly_build(plotScatter()))
  
})

  # download pdf
  callModule(downPlot, "downPlotScatter", "scatter.pdf", plotScatter, TRUE)
  
  # Hierarchical - choose to display regular heatmap.2 or d3heatmap (interactive)
  output$plotInt_ui <- renderUI({
    ns <- session$ns
    if (input$plotInt)
      tagList(plotlyOutput(ns("outPlotScatterInt"), height = paste0(input$inPlotHeight, "px")))
    else
      tagList(plotOutput(ns('outPlotScatter'), height = paste0(input$inPlotHeight, "px")))
  })
  
}