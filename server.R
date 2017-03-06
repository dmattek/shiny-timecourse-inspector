
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

shinyServer(function(input, output, session) {
  
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
    selectInput('inSelSite', 'Select FOV (e.g. Metadata_Site or Metadata_Series):', locCols, width = '100%', selected = locColSel)
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
    selectInput('inSelTime', 'Select X (e.g. RealTime):', locCols, width = '100%', selected = locColSel)
  })

  # This is main field to select plot facet grouping
  # It's typically a column with the entire experimental description,
  # e.g. in Yannick's case it's Stim_All_Ch or Stim_All_S.
  # In Coralie's case it's a combination of 3 columns called Stimulation_...
  output$varSelGroup = renderUI({
    cat(file=stderr(), 'UI varSelGroup\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'timulation']
    if (length(locColSel) == 0)
      locColSel = locCols[locCols %like% 'ite'][1] # index 1 at the end in case more matches; select 1st
    else if (length(locColSel) > 1) {
      locColSel = locColSel[1]      
    }
    
#    cat('UI varSelGroup::locColSel ', locColSel, '\n')
    selectInput('inSelGroup', 'Select Grouping for Plotting (e.g. Site, Well, Channel):', locCols, width = '100%', selected = locColSel)
  })

  output$varSelGroup2 = renderUI({
    cat(file=stderr(), 'UI varSelGroup2\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'timulation']
    if (length(locColSel) == 0)
      locColSel = locCols[locCols %like% 'ite'][1] # index 1 at the end in case more matches; select 1st
    else if (length(locColSel) > 1) {
#      updateCheckboxInput(session, 'inGroupMore1', value = 1)
      locColSel = locColSel[2]      
    }
    
#    cat('UI varSelGroup2::locColSel ', locColSel, '\n')
    

    if(input$inGroupMore1) {
      selectInput('inSelGroup2', 'Select Additional Grouping:', locCols, width = '100%', selected = locColSel)
    } else {
      disabled(selectInput('inSelGroup2', 'Select Additional Grouping:', locCols, width = '100%', selected = locColSel))
    }
  })
  
  output$varSelGroup3 = renderUI({
    cat(file=stderr(), 'UI varSelGroup2\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'timulation']
    if (length(locColSel) == 0)
      locColSel = locCols[locCols %like% 'ite'][1] # index 1 at the end in case more matches; select 1st
    else if (length(locColSel) > 1) {
 #     updateCheckboxInput(session, 'inGroupMore2', value = 1)
      locColSel = locColSel[3]      
    }
    
#    cat('UI varSelGroup3::locColSel ', locColSel, '\n')
    
    
    if(input$inGroupMore2) {
      selectInput('inSelGroup3', 'Select Additional Grouping:', locCols, width = '100%', selected = locColSel)
    } else {
      disabled(selectInput('inSelGroup3', 'Select Additional Grouping:', locCols, width = '100%', selected = locColSel))
    }
  })
  
  output$varSelMeas1 = renderUI({
    cat(file=stderr(), 'UI varSelMeas1\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'Intensity'][1] # index 1 at the end in case more matches; select 1st
    
#    cat(locColSel, '\n')
    selectInput('inSelMeas1', 'Select Y:', locCols, width = '100%', selected = locColSel)
  })


  output$varSelMeas2 = renderUI({
    cat(file=stderr(), 'UI varSelMeas2\n')
    locCols = getDataNucCols()
    locColSel = locCols[locCols %like% 'Intensity'][1] # index 1 at the end in case more matches; select 1st
    
#    cat(locColSel, '\n')
    selectInput('inSelMeas2', 'Select 2nd operand:', locCols, width = '100%', selected = locColSel)
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

    reset("inGroupMore1")  # reset is a shinyjs function
    reset("inGroupMore2")  # reset is a shinyjs function
  })
  
  dataInBoth <- reactive({
    cat(file=stderr(), 'dataInBoth: in\n')
    
    locInGen = input$butDataGen
    locButLoadNuc = isolate(butCounter$dataLoadNuc)
    locButLoadStim = isolate(butCounter$dataLoadStim)
    locButGen = isolate(butCounter$dataGen)
    cat("butCounter$dataGen: ", locButGen, "\nbutCounter$dataLoadNuc: ", locButLoadNuc, "\nbutCounter$locButLoadStim: ", locButLoadStim, "\n")
    
    locInLoadNuc  = ifelse(is.null(input$inFileNucLoad),  0, locButLoadNuc + 1)
    locInLoadStim = ifelse(is.null(input$inFileStimLoad), 0, locButLoadStim + 1)
    cat(file=stderr(), "dataInBoth\ninGen: ", locInGen, "\ninLoadNuc: ", locInLoadNuc, "\ninLoadStim: ", locInLoadStim, "\n")
    
    
    # isolate the checks of counter reactiveValues
    # as we set the values in this same reactive
    if (locInLoadNuc != locButLoadNuc) {
      cat(file=stderr(), "dataInBoth if inFileNucLoad\n")
      dm = userDataNuc()

      # no need to isolate updating the counter reactive values!
      
      butCounter$dataLoad <- locInLoadNuc
    } else if (locInGen != locButGen) {
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
    
    
    dt.nuc = userDataNucMod()
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
    
    # create an expression for faceting (max 3 fields)
    loc.facet.group = isolate(input$inSelGroup)
    if (isolate(input$inGroupMore1)) {
      loc.facet.group = paste0(loc.facet.group, ' + ', isolate(input$inSelGroup2))
    } 
    if (isolate(input$inGroupMore2)) {
      loc.facet.group = paste0(loc.facet.group, ' + ', isolate(input$inSelGroup3))
    }
#    cat("loc.facet.group: ", loc.facet.group, "\n")
    
    
    # create expression for plotting Y-axis
    loc.math = isolate(input$inSelMath)
    if (loc.math != '') {
      loc.meas.2 = isolate(input$inSelMeas2)
      loc.y.arg = paste0(loc.meas.1, loc.math, loc.meas.2)
    }  else
      loc.y.arg = loc.meas.1
#    cat("loc.y.arg", loc.y.arg, "\n")
    
    p.out = myGgplotTraj(
      dt.arg = dt.nuc,
      x.arg = loc.time,
      y.arg = loc.y.arg,
      group.arg = "trackObjectsLabelUni",
      facet.arg = loc.facet.group,
      dt.stim.arg = dt.stim,
      tfreq.arg = 1,
      facet.ncol.arg = loc.facet.ncol.arg,
      stim.bar.height.arg = 0.05,
      stim.bar.width.arg = 1
    )
    
    #ggplotly(p.out)
    cat(file=stderr(), 'trajPlot: out\n')
    
    # This is required to avoid 
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    if (names(dev.cur()) != "null device") dev.off()
    pdf(NULL)
    
    p.out.ly = plotly_build(p.out)
    
    # Custom tooltip
    # p.out.ly$x$data[[1]]$text <- sprintf("t: %d <br>y: %.2f <br>id: %s <br>s: %s", 
    #                                      dt.nuc[[loc.time]], 
    #                                      dt.nuc[[loc.y.arg]], dt.nuc[['trackObjectsLabelUni']],
    #                                      dt.nuc[[loc.facet.group]])
    
    return(p.out.ly)
  })
  
})

