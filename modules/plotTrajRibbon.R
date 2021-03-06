#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This module is for plotting group averages as ribbon plots (mean + 95%CI)
#

## UI ----

modTrajRibbonPlotUI =  function(id, label = "Plot Group Averages") {
  ns <- NS(id)
  
  tagList(
    checkboxInput(ns('chBplotStyle'),
                  'Appearance',
                  FALSE),
    conditionalPanel(
      condition = "input.chBplotStyle",
      ns = ns,
      fluidRow(
        column(
          3,
          radioButtons(ns('rBlegendPos'), 'Legend', list('top' = 'top', 'right' = 'right')),
          checkboxInput(ns("chBfacet"), "Groups in facets", FALSE),
          conditionalPanel(
            condition = "input.chBfacet",
            ns = ns,
            numericInput(
              ns('inFacetNcol'),
              '#columns',
              value = PLOTNFACETDEFAULT,
              min = 1,
              width = '100px',
              step = 1
            )
          )
        ),
        column(
          2,
          radioButtons(ns('rBPlotTrajStat'), 'Display', list('Mean only' = 'Mean',
                                                             'Add 95% CI' = 'CI', 
                                                             'Add SE' = 'SE'))
        ),
        column(
          3,
          sliderInput(ns('sliPlotTrajSkip'), 'Plot every n-th point', min = 1, max = 10, value = 1, step = 1),
          
          checkboxInput(ns('chBsetXbounds'), 'Bounds for X', FALSE),
          fluidRow(
            column(6,
                   uiOutput(ns('uiSetXboundsLow'))
            ),
            column(6,
                   uiOutput(ns('uiSetXboundsHigh'))
            )),
          
          checkboxInput(ns('chBsetYbounds'), 'Bounds for Y', FALSE),
          fluidRow(
            column(6,
                   uiOutput(ns('uiSetYboundsLow'))
            ),
            column(6,
                   uiOutput(ns('uiSetYboundsHigh'))
            ))
        ),
        column(
          2,
          numericInput(
            ns('inPlotTrajWidth'),
            'Width [%]',
            value = PLOTWIDTH,
            min = 10,
            width = '100px',
            step = 5
          ),
          numericInput(
            ns('inPlotTrajHeight'),
            'Height [px]',
            value = PLOTRIBBONHEIGHT,
            min = 100,
            width = '100px',
            step = 50
          )
        )
      )
    ),
    
    fluidRow(
      column(2,
             actionButton(ns('butPlotTraj'), 'Plot!')),
      column(2,
             checkboxInput(ns('chBplotTrajInt'), 'Interactive'))),
    uiOutput(ns('uiPlotTraj')),
    
    checkboxInput(ns('chBdownload'),
                  'Download',
                  FALSE),
    conditionalPanel(
      condition = "input.chBdownload",
      ns = ns,
      
      downPlotUI(ns('downPlotTraj'), "")
    ),
    
    modTrackStatsUI(ns('dispTrackStats'))
  )
}


## SERVER ----

#' Module for plotting an aggregated ribbon plot of time series
#'
#' @param input 
#' @param output 
#' @param session 
#' @param in.data Data.table with individual (non-aggregated) times series in long format
#' @param in.data.stim Optional long-format data.table for plotting stimulation segments
#' @param in.group String with the name of a grouping column
#' @param in.group.color Data.table with assignments of colours to groups in in.data. 
#'                       Contains two columns: 
#'                       gr.no - group id, 
#'                       gr.col - colour assignments
#' @param in.fname File name for saving the plot
#'
#' @return
#' @export
#'
#' @examples
modTrajRibbonPlot = function(input, output, session, 
                             in.data, 
                             in.data.stim = NULL,
                             in.group = 'group', 
                             in.group.color = NULL, 
                             in.fname = "trajAverages.pdf") {
  
  ns <- session$ns
  
  ## UI rendering ----
  
  # UI for bounding the x-axis
  output$uiSetXboundsLow = renderUI({
    ns <- session$ns
    
    if(input$chBsetXbounds) {
      
      loc.dt = in.data()
      
      if (is.null(loc.dt)) {
        cat(file = stderr(), 'uiSetXboundsLow: dt is NULL\n')
        return(NULL)
      }
      
      if (nrow(loc.dt) < 1)
        return(NULL)
      
      numericInput(
        ns('inSetXboundsLow'),
        label = 'Lower',
        step = 0.1, 
        value = floor(min(loc.dt[[COLRT]], na.rm = T))
      )
    }
  })
  
  
  output$uiSetXboundsHigh = renderUI({
    ns <- session$ns
    
    if(input$chBsetXbounds) {
      
      loc.dt = in.data()
      
      if (is.null(loc.dt)) {
        cat(file = stderr(), 'uiSetXboundsHigh: dt is NULL\n')
        return(NULL)
      }
      
      if (nrow(loc.dt) < 1)
        return(NULL)
      
      numericInput(
        ns('inSetXboundsHigh'),
        label = 'Upper',
        step = 0.1, 
        value = ceil(max(loc.dt[[COLRT]], na.rm = T))
      )
    }
  })
  
  
  # UI for bounding the y-axis
  output$uiSetYboundsLow = renderUI({
    ns <- session$ns
    
    if(input$chBsetYbounds) {
      
      loc.dt = in.data()
      
      if (is.null(loc.dt)) {
        cat(file = stderr(), 'uiSetYboundsLow: dt is NULL\n')
        return(NULL)
      }
      
      if (nrow(loc.dt) < 1)
        return(NULL)
      
      numericInput(
        ns('inSetYboundsLow'),
        label = 'Lower',
        step = 0.1, 
        value = min(loc.dt[[COLY]], na.rm = T)
      )
    }
  })
  
  
  output$uiSetYboundsHigh = renderUI({
    ns <- session$ns
    
    if(input$chBsetYbounds) {
      
      loc.dt = in.data()
      
      if (is.null(loc.dt)) {
        cat(file = stderr(), 'uiSetYboundsHigh: dt is NULL\n')
        return(NULL)
      }
      
      if (nrow(loc.dt) < 1)
        return(NULL)
      
      numericInput(
        ns('inSetYboundsHigh'),
        label = 'Upper',
        step = 0.1, 
        value = max(loc.dt[[COLY]], na.rm = T)
      )
    }
  })
  
  ## Plotting ====
  
  output$uiPlotTraj = renderUI({
    if (input$chBplotTrajInt)
      plotlyOutput(
        ns("outPlotTrajInt"),
        width = paste0(input$inPlotTrajWidth, '%'),
        height = paste0(input$inPlotTrajHeight, 'px')
      ) else
        plotOutput(
          ns("outPlotTraj"),
          width = paste0(input$inPlotTrajWidth, '%'),
          height = paste0(input$inPlotTrajHeight, 'px')
        )
  })
  
  
  callModule(modTrackStats, 'dispTrackStats',
             in.data = in.data,
             in.bycols = in.group)
  
  
  output$outPlotTraj <- renderPlot({
    
    loc.p = plotTraj()
    if(is.null(loc.p))
      return(NULL)
    
    return(loc.p)
  })
  
  
  output$outPlotTrajInt <- renderPlotly({
    # This is required to avoid
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    if (names(dev.cur()) != "null device")
      dev.off()
    pdf(NULL)
    
    loc.p = plotTraj()
    if(is.null(loc.p))
      return(NULL)
    
    return(plotly_build(loc.p))
  })
  
  
  plotTraj <- function() {
    cat(file = stderr(), 'plotTrajRibbon: in\n')
    locBut = input$butPlotTraj
    
    # Check if data exists
    # Thanks to isolate all mods in the left panel are delayed 
    # until clicking the Plot button
    loc.dt = shiny::isolate(in.data())
    
    shiny::validate(
      shiny::need(!is.null(loc.dt), message = "Nothing to plot. Load data first!")
    )
    
    if (is.null(loc.dt)) {
      cat(file = stderr(), 'plotTrajRibbon: dt is NULL\n')
      
      return(NULL)
    }
    
    if (nrow(loc.dt) < 1) {
      cat(file = stderr(), 'plotTrajRibbon: dt has 0 rows\n')
      
      return(NULL)
    }
    
    # check if stim data exists
    loc.dt.stim = shiny::isolate(in.data.stim())
    
    if (is.null(loc.dt.stim)) {
      cat(file = stderr(), 'plotTrajRibbon: stim is NULL\n')
    } else {
      cat(file = stderr(), 'plotTrajRibbon: stim not NULL\n')
      
      if (!input$chBfacet) {
        # choose only 1st group of stimulation pattern for ribbon plot
        # if everything plotted in the same facet
        
        loc.groups = unique(loc.dt.stim[['group']])
        if(length(loc.groups) > 1) {
          cat(file = stderr(), 'plotTrajRibbon: more than 1 group in stim; choosing 1st\n')
          loc.dt.stim = loc.dt.stim[group == loc.groups[1]]
        }
      }
    }
    
    # Future: change such that a column with colouring status is chosen by the user
    # colour trajectories, if dataset contains mid.in column
    # with filtering status of trajectory
    if (sum(names(loc.dt) %in% 'mid.in') > 0)
      loc.line.col.arg = 'mid.in'
    else
      loc.line.col.arg = NULL
    
    # select every other point for plotting
    loc.dt = loc.dt[, .SD[seq(1, .N, input$sliPlotTrajSkip)], by = id]
    
    # check if columns with XY positions are present
    if (sum(names(loc.dt) %like% 'pos') == 2)
      locPos = TRUE
    else
      locPos = FALSE
    
    # check if column with ObjectNumber is present
    if (sum(names(loc.dt) %like% 'obj.num') == 1)
      locObjNum = TRUE
    else
      locObjNum = FALSE
    
    # in.group.color is typically used when plotting time series within clusters.
    # The number of colours in the palette has to be equal to the number of groups.
    # This might differ if the user selects manually groups (e.g. clusters) to display.
    
    if (is.null(in.group.color)) {
      # If no colour scale provided, create a vector with colours 
      # from a default Tableau palette.
      
      # Get existing groups in dt for sub-setting the externally provided group-colour table
      loc.groups = unique(loc.dt[, ..in.group])
      loc.group.color = LOCreturnTableauPalette("Tableau 20", nrow(loc.groups))
    } else {
      loc.group.color = in.group.color()
    }
    
    # aggregate data; calculate Mean, CI or SE
    loc.ribbon.lohi = NULL
    
    if(input$rBPlotTrajStat == "Mean") {
      # calculate the mean
      loc.dt.aggr = loc.dt[, 
                           .(Mean = mean(get(COLY), 
                                         na.rm = T)), 
                           by = c(in.group, COLRT)]
      
    } else if(input$rBPlotTrajStat == "CI") {
      # calculate the mean and the confidence intervals
      loc.dt.aggr = LOCcalcTrajCI(in.dt = loc.dt, 
                                  in.col.meas = COLY, 
                                  in.col.by = c(in.group, COLRT), 
                                  in.type = 'normal')
      
      loc.ribbon.lohi = c('Lower', 'Upper')
      
    } else if(input$rBPlotTrajStat == "SE") {
      # calculate the mean and the standard error of the mean
      loc.dt.aggr = loc.dt[, 
                           .(Mean = mean(get(COLY), na.rm = T),
                             Lower = mean(get(COLY), na.rm = T) - LOCstderr(get(COLY), na.rm = T),
                             Upper = mean(get(COLY), na.rm = T) + LOCstderr(get(COLY), na.rm = T)), 
                           by = c(in.group, COLRT)]
      
      loc.ribbon.lohi = c('Lower', 'Upper')
    }
    
    
    
    # set the grouing column to a factor (for plotting)
    loc.dt.aggr[, 
                (in.group) := as.factor(get(in.group))]
    
    # setting bounds for displaying of x and y axes
    loc.xlim.arg = NULL
    if(input$chBsetXbounds) {
      loc.xlim.arg = c(input$inSetXboundsLow, input$inSetXboundsHigh)
    } 
    
    loc.ylim.arg = NULL
    if(input$chBsetYbounds) {
      loc.ylim.arg = c(input$inSetYboundsLow, input$inSetYboundsHigh)
    } 
    
    p.out = LOCplotTrajRibbon(dt.arg = loc.dt.aggr, 
                              x.arg = COLRT, 
                              y.arg = 'Mean',
                              group.arg = in.group,
                              facet.arg = input$chBfacet,
                              facet.ncol.arg = input$inFacetNcol,
                              col.arg = loc.group.color,
                              dt.stim.arg = loc.dt.stim,
                              x.stim.arg = c('tstart', 'tend'),
                              y.stim.arg = c('ystart', 'yend'), 
                              ribbon.lohi.arg = loc.ribbon.lohi,
                              xlim.arg = loc.xlim.arg,
                              ylim.arg = loc.ylim.arg,
                              xlab.arg = 'Time',
                              ylab.arg = '') +
      LOCggplotTheme(in.font.base = PLOTFONTBASE, 
                     in.font.axis.text = PLOTFONTAXISTEXT, 
                     in.font.axis.title = PLOTFONTAXISTITLE, 
                     in.font.strip = PLOTFONTFACETSTRIP, 
                     in.font.legend = PLOTFONTLEGEND) + 
      theme(legend.position = input$rBlegendPos) 
    
    return(p.out)
  }
  
  ## Download ----
  # Trajectory plot - download pdf
  callModule(downPlot, "downPlotTraj", 
             in.fname = in.fname, 
             plotTraj, TRUE)
  
}