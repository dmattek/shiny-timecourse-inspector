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
    actionLink(ns("alScatter"), "Learn more"),
    br(),
    
    fluidRow(
      column(
        4,
        uiOutput(ns('uiSelTptX')),
        uiOutput(ns('uiSelTptY')),
        bsAlert("alert2differentTpts"),
        radioButtons(ns('rBfoldChange'), 'Y-axis', 
                     choices = c("Y" = "y", "Y-X" = "diff"), 
                     width = "100px", inline = T),
        bsTooltip(ns('rBfoldChange'), help.text.short[15], placement = "right", trigger = "hover", options = NULL),
        checkboxInput(ns('chBregression'), 'Linear regression with 95% CI'),
        bsTooltip(ns('chBregression'), help.text.short[16], placement = "right", trigger = "hover", options = NULL)
      ),
      column(
        4, 
        numericInput(ns('inNeighTpts'), 'Smoothing', value = 0, step = 1, min = 0, width = "150px"),
        bsTooltip(ns('inNeighTpts'), help.text.short[17], placement = "right", trigger = "hover", options = NULL)
      ),
      column(
        4,
        numericInput(
          ns('inPlotHeight'),
          'Height [px]',
          value = PLOTSCATTERHEIGHT,
          min = 100,
          step = 100,
          width = "100px"
        ),
        numericInput(
          ns('inPlotNcolFacet'),
          '#columns',
          value = PLOTNFACETDEFAULT,
          min = 1,
          step = 1,
          width = "100px"
          
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
  
  ns <- session$ns
  
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
      'Time point for X-axis',
      loc.v,
      width = '200px',
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
      'Time point for Y-axis',
      loc.v,
      width = '200px',
      selected = 1,
      multiple = FALSE
    )
  }
})

data4scatterPlot <- reactive({
  cat(file = stderr(), 'data4scatterPlot\n')
  
  loc.dt.in = in.data()
  if(is.null(loc.dt.in))
    return(NULL)
  
  # obtain selected time points from UI
  loc.tpts.x = as.integer(input$inSelTptX)
  loc.tpts.y = as.integer(input$inSelTptY)
  
  if (loc.tpts.x == loc.tpts.y) {
    createAlert(session, "alert2differentTpts", "exampleAlert", title = "",
                content = "Select two different time points.", append = FALSE)
    return(NULL)
    
  } else {
    closeAlert(session, "exampleAlert")    
  }
  
  # if neigbbouring points selected, obtain time points for which the aggregation will be calculated
  if (input$inNeighTpts > 0) {
    # get all time points in the dataset
    loc.dt.in.tpts = unique(loc.dt.in[[COLRT]])
    
    # get indices of time points around selected time points
    loc.tpts.x.id = seq(which(loc.dt.in.tpts == loc.tpts.x) - input$inNeighTpts, which(loc.dt.in.tpts == loc.tpts.x) + input$inNeighTpts, 1)
    loc.tpts.y.id = seq(which(loc.dt.in.tpts == loc.tpts.y) - input$inNeighTpts, which(loc.dt.in.tpts == loc.tpts.y) + input$inNeighTpts, 1)
    
    # get only indices of time points that are greater than 0
    loc.tpts.x.id = loc.tpts.x.id[loc.tpts.x.id > 0]
    loc.tpts.y.id = loc.tpts.y.id[loc.tpts.y.id > 0]
    
    # update time points used for aggregation
    loc.tpts.x = loc.dt.in.tpts[loc.tpts.x.id]
    loc.tpts.y = loc.dt.in.tpts[loc.tpts.y.id]

    # aggregate separately each time point sets
    loc.dt.x = loc.dt.in[get(COLRT) %in% loc.tpts.x, .(y.aggr = mean(get(COLY))), by = c(COLGR, COLID)]
    loc.dt.y = loc.dt.in[get(COLRT) %in% loc.tpts.y, .(y.aggr = mean(get(COLY))), by = c(COLGR, COLID)]
    
    loc.dt = merge(loc.dt.x, loc.dt.y, by = COLID)
    loc.dt[, group.y := NULL]
    
    setnames(loc.dt, c('group.x', 'y.aggr.x', 'y.aggr.y'), c(COLGR, 'x', 'y'))

    #cat(loc.tpts.x.id, '\n')
    #cat(loc.tpts.y.id, '\n')
  } else {
    # get data from selected time points
    loc.dt = loc.dt.in[get(COLRT) %in% c(loc.tpts.x, loc.tpts.y)]

    # convert to wide, such that two selected time points are in two columns
    loc.dt = dcast(loc.dt[, c(COLGR, COLID, COLY, COLRT), with = F], 
                   as.formula(paste0(COLGR, "+", COLID, "~", COLRT)),
                   value.var = COLY)
   
    setnames(loc.dt, c(COLGR, COLID, "x", "y"))
  }

  

  if (input$rBfoldChange == "diff") {
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
  
  # Scatter plot - choose to display regular or interactive plot
  output$plotInt_ui <- renderUI({
    ns <- session$ns
    if (input$plotInt)
      tagList( withSpinner(plotlyOutput(ns("outPlotScatterInt"), height = paste0(input$inPlotHeight, "px"))))
    else
      tagList( withSpinner(plotOutput(ns('outPlotScatter'), height = paste0(input$inPlotHeight, "px"))))
  })
  
  addPopover(session, 
             id = ns("alScatter"), 
             title = "Scatter plot",
             content = "Display measurement values from two different time points as a scatter plot.",
             trigger = "click")
  
  
  
}