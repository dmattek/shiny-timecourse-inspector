
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(data.table)
library(ggplot2)
library(plotly)

options(shiny.maxRequestSize=30*1024^2)
source('auxfunc.R')

shinyServer(function(input, output) {
  
  butCounter <- reactiveValues(
    dataLoadNuc  = isolate(ifelse(is.null(input$inFileNucLoad), 0, 1)), 
    dataLoadStim = isolate(ifelse(is.null(input$inFileStimLoad), 0, 1)), 
    dataGen  = isolate(input$butDataGen)
  )
  
  getDataNucCols <- reactive({
    cat(file=stderr(), 'getDataNucCols: in\n')
    
    return(colnames(dataInBoth()))
  })
  
  output$varSelSite = renderUI({
    cat(file=stderr(), 'UI varSelSite\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'ite'][1] # index 1 at the end in case more matches; select 1st

    cat(locColSel, '\n')
    selectInput('inSelSite', 'Select Grouping (e.g. Metadata_Site or Well):', locCols, width = '100%', selected = locColSel)
  })
  
  output$varSelTrackLabel = renderUI({
    cat(file=stderr(), 'UI varSelTrackLabel\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'rack'][1] # index 1 at the end in case more matches; select 1st
    
    cat(locColSel, '\n')
    selectInput('inSelTrackLabel', 'Select Track Label (e.g. objNuc_Track_ObjectsLabel):', locCols, width = '100%', selected = locColSel)
  })
  
  output$varSelTime = renderUI({
    cat(file=stderr(), 'UI varSelTime\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'ime'][1] # index 1 at the end in case more matches; select 1st
    
    cat(locColSel, '\n')
    selectInput('inSelTime', 'Select Time (e.g. RealTime):', locCols, width = '100%', selected = locColSel)
  })

    output$varSelMeas1 = renderUI({
    cat(file=stderr(), 'UI varSelMeas1\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'MeanIntensity'][1] # index 1 at the end in case more matches; select 1st
    
    cat(locColSel, '\n')
    selectInput('inSelMeas1', 'Select 1st Measurement:', locCols, width = '100%', selected = locColSel)
  })

  output$varSelRatio = renderUI({
    cat(file=stderr(), 'UI varSelRatio\n')
    checkboxInput('inSelRatio', 'Divide by:', 0)
  })

  output$varSelMeas2 = renderUI({
    cat(file=stderr(), 'UI varSelMeas2\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'Intensity'][1] # index 1 at the end in case more matches; select 1st
    
    cat(locColSel, '\n')
    selectInput('inSelMeas2', 'Select 2nd Measurement:', locCols, width = '100%', selected = locColSel)
  })
  
  output$outPlot = renderUI({
    
    plotlyOutput("trajPlot", width = paste0(input$inPlotWidth, '%'), height = paste0(input$inPlotHeight, 'px'))
  })
  
  userDataNuc <- eventReactive(input$inFileNucLoad, {
    cat(file=stderr(), 'userDataNuc: in\n')
    
    infile = input$inFileNucLoad
    
    dt = fread(infile$datapath)
    
    cat(file=stderr(), 'userDataNuc: out\n')
    return(dt)
  })
  
  userDataNucMod = reactive({
    # make unique cell identifier based on metadata.site
    cat(file=stderr(), 'userDataNucMod: in\n')
    
#    dt = userDataNuc()
    dt = dataInBoth()
    colNameSite  = input$inSelSite
    colNameTrackLabel = input$inSelTrackLabel
    
    if (colNameSite == '' && colNameTrackLabel == '') {
      cat(file=stderr(), 'userDataNucMod: no colName\n')
      return(NULL)
    }
    
    dt[, trackObjectsLabelUni := paste(sprintf("%04d", get(colNameSite)),
                                       sprintf("%04d", get(colNameTrackLabel)),
                                       sep = "_")]

    loc.colnames = colnames(dt)
    if (sum(loc.colnames %like% 'Stimulation') == 0) {
      dt[, metadata.site.stim := get(colNameSite)]
    } else {
      dt[, metadata.site.stim := paste(
        sprintf('%02d', get(colNameSite)),
        ': ',
        Stimulation_duration,
        ' ',
        Stimulation_intensity,
        ' ',
        Stimulation_treatment,
        sep = ''
      )]
    }
    
    cat(file=stderr(), 'userDataNucMod: out\n')
    return(dt)
  })
  
  userDataStim <- eventReactive(input$inFileStimLoad, {
    cat(file=stderr(), 'userDataStim: in\n')
    
    infile = input$inFileStimLoad
    
    dt = fread(infile$datapath)
    cat(file=stderr(), 'userDataStim: out\n')
    return(dt)
  })
  
  # This button will reset the inFileLoad
  observeEvent(input$butReset, {
    reset("inFileNucLoad")  # reset is a shinyjs function
    reset("inFileStimLoad")  # reset is a shinyjs function
  })
  
  dataInBoth <- reactive({
    cat(file=stderr(), 'dataInBoth: in\n')
    
    locInGen = input$butDataGen
    locInLoadNuc  = ifelse(is.null(input$inFileNucLoad),  0, isolate(butCounter$dataLoadNuc) + 1)
    locInLoadStim = ifelse(is.null(input$inFileStimLoad), 0, isolate(butCounter$dataLoadStim) + 1)
    
    cat(file=stderr(), "dataInBoth\n1: ", locInGen, "\n2: ", locInLoadNuc, "\n3: ", locInLoadStim, "\n")
    
    # isolate the checks of counter reactiveValues
    # as we set the values in this same reactive
    if (locInLoadNuc != isolate(butCounter$dataLoadNuc)) {
      cat(file=stderr(), "dataInBoth if inFileNucLoad\n")
      dm = userDataNuc()

      # no need to isolate updating the counter reactive values!
      butCounter$dataLoad <- locInLoadNuc
    } else if (locInGen != isolate(butCounter$dataGen)) {
      cat(file=stderr(), "dataInBoth if inDataGen\n")
      dm = userDataGen()
      cat(colnames(dm))
      # no need to isolate updating the counter reactive values!
      butCounter$dataGen <- locInGen
    } else dm = NULL
    
    cat(file=stderr(), 'dataInBoth: out\n')
    return(dm)
  })
  
  output$trajPlot <- renderPlotly({
    
    cat(file=stderr(), 'trajPlot: in\n')
    locBut = input$butGo
    
    if (locBut == 0) {
      cat(file=stderr(), 'trajPlot: Go button not pressed\n')
      
      return(NULL)
    }
    
    
    dt.nuc = (userDataNucMod())
    locInLoadStim = isolate(input$inFileStimLoad)
    
    if (is.null(dt.nuc) && is.null(locInLoadStim)) {
      cat(file=stderr(), 'trajPlot: Data not yet loaded\n')
      
      return(NULL)
    } else if (is.null(locInLoadStim)) {
      cat(file=stderr(), 'trajPlot: only timecourses loaded\n')
      dt.stim = NULL
      
    } else {
      cat(file=stderr(), 'trajPlot: timecourses and stimulation pattern loaded\n')
      
      dt.stim = userDataStim()
    }
    
    loc.facet.ncol.arg = isolate(input$inFacetNcol)
    loc.time = isolate(input$inSelTime)
    loc.meas.1 = isolate(input$inSelMeas1)
    
    if (isolate(input$inSelRatio)) {
      loc.meas.2 = isolate(input$inSelMeas2)
      loc.y.arg = paste0(loc.meas.1, ' / ', loc.meas.2)
    }  else
      loc.y.arg = loc.meas.1
    
    cat(loc.y.arg)
    
    p.out = myGgplotTraj(
      dt.arg = dt.nuc,
      x.arg = loc.time,
      y.arg = loc.y.arg,
      group.arg = "trackObjectsLabelUni",
      facet.arg = 'metadata.site.stim',
#      xlab.arg = "Time (min)",
#      ylab.arg = loc.y.arg,
#      plotlab.arg = "Raw data from illumination-corrected images",
      dt.stim.arg = dt.stim,
      tfreq.arg = 1,
      maxrt.arg = 120,
      xaxisbreaks.arg = 10,
      facet.ncol.arg = loc.facet.ncol.arg,
      ylim.arg = c(0, 1.2),
      stim.bar.height.arg = 0.05,
      stim.bar.width.arg = 1
    )
    
    #ggplotly(p.out)
    cat(file=stderr(), 'trajPlot: out\n')
    return(ggplotly(p.out))
  })
  
})

