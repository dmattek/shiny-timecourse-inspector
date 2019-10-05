#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This is the server logic for a Shiny web application.
#

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(shinyBS) # for tooltips
library(shinycssloaders) # for loader animations
library(data.table)
library(ggplot2)
library(gplots) # for heatmap.2
library(plotly) # interactive plot
library(DT) # interactive tables

library(dendextend) # for color_branches
library(colorspace) # for palettes (used to colour dendrogram)
library(RColorBrewer)
library(scales) # for percentages on y scale
library(ggthemes) # nice colour palettes

library(sparcl) # sparse hierarchical and k-means
library(dtw) # for dynamic time warping
library(factoextra) # extract and visualize the output of multivariate data analyses 
library(imputeTS) # for interpolating NAs
library(robust) # for robust linear regression
library(MASS)
library(pracma) # for trapz used in AUC calculation



# Global parameters ----
# change to increase the limit of the upload file size
options(shiny.maxRequestSize = 200 * 1024 ^ 2)

# Important when joining, grouping or ordering numeric (i.e. double, POSIXct) columns.
# https://stackoverflow.com/questions/58230619/xy-join-of-keyed-data-table-fails-when-key-on-numeric-column-and-data-fread-fr
setNumericRounding(2)


# colour of loader spinner (shinycssloaders)
options(spinner.color="#00A8AA")

# Server logic ----
shinyServer(function(input, output, session) {
  useShinyjs()
  
  # This is only set at session start
  # We use this as a way to determine which input was
  # clicked in the dataInBoth reactive
  counter <- reactiveValues(
    # The value of actionButton is the number of times the button is pressed
    dataGen1        = isolate(input$inDataGen1),
    dataLoadNuc     = isolate(input$inButLoadNuc),
    dataLoadTrajRem = isolate(input$inButLoadTrajRem),
    dataLoadStim    = isolate(input$inButLoadStim)
  )

  nCellsCounter <- reactiveValues(
    nCellsOrig = 0,
    nCellsAfterOutlierTrim = 0
  )
    
  myReactVals = reactiveValues(
    outlierIDs = NULL
  )
  
  # UI-side-panel-data-load ----
  
  # Generate random dataset
  dataGen1 <- eventReactive(input$inDataGen1, {
    if (DEB)
      cat("server:dataGen1\n")
    
    return(LOCgenTraj2(n_perGroup = 20, sd_noise = 0.01, sampleFreq = 0.4, endTime = 40))
  })
  
  # Load main data file
  dataLoadNuc <- eventReactive(input$inButLoadNuc, {
    if (DEB)
      cat("server:dataLoadNuc\n")

    locFilePath = input$inFileLoadNuc$datapath
    
    counter$dataLoadNuc <- input$inButLoadNuc - 1
    
    if (is.null(locFilePath) || locFilePath == '')
      return(NULL)
    else {
      return(fread(locFilePath, strip.white = T))
    }
  })
  
  # This button will reset the inFileLoad
  observeEvent(input$butReset, {
    reset("inFileLoadNuc")  # reset is a shinyjs function
  })

  # Load data with trajectories to remove
  dataLoadTrajRem <- eventReactive(input$inButLoadTrajRem, {
    if (DEB)
      cat(file = stdout(), "server:dataLoadTrajRem\n")
    
    locFilePath = input$inFileLoadTrajRem$datapath
    
    counter$dataLoadTrajRem <- input$inButLoadTrajRem - 1
    
    if (is.null(locFilePath) || locFilePath == '')
      return(NULL)
    else {
      return(fread(locFilePath))
    }
  })
  
  # Load data with stimulation pattern
  dataLoadStim <- eventReactive(input$inButLoadStim, {
    if (DEB)
      cat(file = stdout(), "server:dataLoadStim\n")
    
    locFilePath = input$inFileLoadStim$datapath
    
    counter$dataLoadStim <- input$inButLoadStim - 1
    
    if (is.null(locFilePath) || locFilePath == '')
      return(NULL)
    else {
      return(fread(locFilePath))
    }
  })
  
    
  # UI for loading csv with cell IDs for trajectory removal
  output$uiFileLoadTrajRem = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:uiFileLoadTrajRem\n')
    
    if(input$chBtrajRem) 
      fileInput(
        'inFileLoadTrajRem',
        "Select file and click Load Data",
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      )
  })
  
  output$uiButLoadTrajRem = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:uiButLoadTrajRem\n')
    
    if(input$chBtrajRem)
      actionButton("inButLoadTrajRem", "Load Data")
  })

  # UI for loading csv with stimulation pattern
  output$uiFileLoadStim = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:uiFileLoadStim\n')
    
    if(input$chBstim) 
      fileInput(
        'inFileLoadStim',
        "Select file and click Load Data",
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      )
  })
  
  output$uiButLoadStim = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:uiButLoadStim\n')
    
    if(input$chBstim)
      actionButton("inButLoadStim", "Load Data")
  })
  

  
  # UI-side-panel-column-selection ----
  output$varSelTrackLabel = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:varSelTrackLabel\n')
    
    locCols = getDataNucCols()
    locColSel = locCols[grep('(T|t)rack|ID|id', locCols)[1]] # index 1 at the end in case more matches; select 1st; matches TrackLabel, tracklabel, Track Label etc
    
    selectInput(
      'inSelTrackLabel',
      'Track ID column',
      locCols,
      width = '100%',
      selected = locColSel
    )
  })
  
  output$varSelTime = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:varSelTime\n')
    
    locCols = getDataNucCols()
    locColSel = locCols[grep('(T|t)ime|Metadata_T', locCols)[1]] # index 1 at the end in case more matches; select 1st; matches RealTime, realtime, real time, etc.
    
    selectInput(
      'inSelTime',
      'Time column',
      locCols,
      width = '100%',
      selected = locColSel
    )
  })

  output$varSelTimeFreq = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:varSelTimeFreq\n')
    
    if (input$chBtrajInter) {
      numericInput(
        'inSelTimeFreq',
        'Interval between 2 time points',
        min = 1,
        step = 1,
        width = '100%',
        value = 1
      )
    }
  })
  
  # This is the main field to select plot facet grouping
  # It's typically a column with the entire experimental description,
  # e.g.1 Stim_All_Ch or Stim_All_S.
  # e.g.2 a combination of 3 columns called Stimulation_...
  output$varSelGroup = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:varSelGroup\n')
    
    if (input$chBgroup) {
      
      locCols = getDataNucCols()
      
      if (!is.null(locCols)) {
        locColSel = locCols[grep('(G|g)roup|(S|s)tim|(S|s)timulation|(S|s)ite', locCols)[1]]

        #cat('UI varSelGroup::locColSel ', locColSel, '\n')
        selectInput(
          'inSelGroup',
          'Grouping columns',
          locCols,
          width = '100%',
          selected = locColSel,
          multiple = TRUE
        )
      }
    }
  })
  
  # UI for selecting grouping to add to track ID to make 
  # the track ID unique across entire dataset
  output$varSelSite = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:varSelSite\n')
    
    if (input$chBtrackUni) {
      locCols = getDataNucCols()
      locColSel = locCols[grep('(S|s)ite|(S|s)eries|(F|f)ov|(G|g)roup', locCols)[1]] # index 1 at the end in case more matches; select 1st
      
      selectInput(
        'inSelSite',
        'Prepend track ID with',
        locCols,
        width = '100%',
        selected = locColSel,
        multiple = T
      )
    }
  })
  
  
  output$varSelMeas1 = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:varSelMeas1\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[grep('(R|r)atio|(I|i)ntensity|(Y|y)|(M|m)eas', locCols)[1]] # index 1 at the end in case more matches; select 1st

      selectInput(
        'inSelMeas1',
        '1st measurement column',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  
  output$varSelMeas2 = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:varSelMeas2\n')
    
    locCols = getDataNucCols()
    
    if (!is.null(locCols) &&
        !(input$inSelMath %in% c('', '1 / '))) {
      locColSel = locCols[grep('(R|r)atio|(I|i)ntensity|(Y|y)|(M|m)eas', locCols)[1]] # index 1 at the end in case more matches; select 1st

      selectInput(
        'inSelMeas2',
        '2nd measurement column',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  # UI-side-panel-trim x-axis (time) ----
  output$uiSlTimeTrim = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:uiSlTimeTrim\n')
    
    if (input$chBtimeTrim) {
      locTpts  = getDataTpts()
      
      if(is.null(locTpts))
        return(NULL)
      
      locRTmin = min(locTpts)
      locRTmax = max(locTpts)
      
      sliderInput(
        'slTimeTrim',
        label = 'Use time range',
        min = locRTmin,
        max = locRTmax,
        value = c(locRTmin, locRTmax),
        step = 1
      )
      
    }
  })
  
  # UI-side-panel-normalization ----
  
  # select normalisation method
  # - fold-change calculates fold change with respect to the mean
  # - z-score calculates z-score of the selected regione of the time series
  output$uiChBnorm = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:uiChBnorm\n')
    
    if (input$chBnorm) {
      tagList(
      radioButtons(
        'rBnormMeth',
        label = 'Select method',
        choices = list('fold-change' = 'mean', 'z-score' = 'z.score'),
        width = "40%"
      ),
      bsTooltip('rBnormMeth', helpText.server[["rBnormMeth"]], placement = "top", trigger = "hover", options = NULL)
      )
    }
  })
  
  # select the region of the time series for normalisation
  output$uiSlNorm = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:uiSlNorm\n')
    
    if (input$chBnorm) {
      locTpts  = getDataTpts()
      
      if(is.null(locTpts))
        return(NULL)
      
      locRTmin = min(locTpts)
      locRTmax = max(locTpts)
      
      tagList(
      sliderInput(
        'slNormRtMinMax',
        label = 'Time span',
        min = locRTmin,
        max = locRTmax,
        value = c(locRTmin, 0.1 * locRTmax), 
        step = 1
      ),
      bsTooltip('slNormRtMinMax', helpText.server[["slNormRtMinMax"]], placement = "top", trigger = "hover", options = NULL)
      )
    }
  })
  
  # use robust stats (median instead of mean, mad instead of sd)
  output$uiChBnormRobust = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:uiChBnormRobust\n')
    
    if (input$chBnorm) {
      tagList(
      checkboxInput('chBnormRobust',
                    label = 'Robust stats',
                    FALSE, 
                    width = "40%"),
      bsTooltip('chBnormRobust', helpText.server[["chBnormRobust"]], placement = "top", trigger = "hover", options = NULL)
      )
    }
  })
  
  # choose whether normalisation should be calculated for the entire dataset, group, or trajectory
  output$uiChBnormGroup = renderUI({
    if (DEB)
      cat(file = stdout(), 'server:uiChBnormGroup\n')
    
    if (input$chBnorm) {
      tagList(
      radioButtons('chBnormGroup',
                   label = 'Normalisation grouping',
                   choices = list('Entire dataset' = 'none', 'Per group' = 'group', 'Per trajectory' = 'id'), 
                   width = "40%"),
      bsTooltip('chBnormGroup', helpText.server[["chBnormGroup"]], placement = "top", trigger = "hover", options = NULL)
      )
    }
  })
  
  
  # Pop-overs ----
  addPopover(session, 
             "alDataFormat",
             title = "Data format",
             content = helpText.server[["alDataFormat"]],
             trigger = "click")
  

  # Processing-data ----
  
  dataInBoth <- reactive({
    # Without direct references to inDataGen1,2 and inFileLoad, inDataGen2
    #    does not trigger running this reactive once inDataGen1 is used.
    # This is one of the more nuanced areas of reactive programming in shiny
    #    due to the if else logic, it isn't fetched once inDataGen1 is available
    # The morale is use direct retrieval of inputs to guarantee they are available
    #    for if else logic checks!
    
    locInGen1 = input$inDataGen1
    locInLoadNuc = input$inButLoadNuc
    #locInLoadStim = input$inButLoadStim
    
    # Don't wrap around if(DEB) !!!
    cat(
      "server:dataInBoth\n   inGen1: ",
      locInGen1,
      "      prev=",
      isolate(counter$dataGen1),
      "\n   inDataNuc: ",
      locInLoadNuc,
      "   prev=",
      isolate(counter$dataLoadNuc),
      # "\ninDataStim: ",
      # locInLoadStim,
      # "   prev=",
      # isolate(counter$dataLoadStim),
      "\n"
    )
    
    # isolate the checks of the counter reactiveValues
    # as we set the values in this same reactive
    if (locInGen1 != isolate(counter$dataGen1)) {
      cat("server:dataInBoth if inDataGen1\n")
      dm = dataGen1()
      # no need to isolate updating the counter reactive values!
      counter$dataGen1 <- locInGen1
    } else if (locInLoadNuc != isolate(counter$dataLoadNuc)) {
      cat("server:dataInBoth if inDataLoadNuc\n")
      dm = dataLoadNuc()
      
      # convert to long format if radio box set to "wide"
      # the input data in long format should contain:
      # - the first row with a header: group, track id, time points as columns with numeric header
      # - consecutive rows with time series, where columns are time points
      if (input$inRbutLongWide == 1) {
        print(length(names(dm)))
        
        # data in wide format requires at least 3 columns: grouping, track id, 1 time point
        if (length(names(dm)) < 3) {
          dm = NULL
          
          createAlert(session, "alertAnchorSidePanelDataFormat", "alertWideTooFewColumns", 
                      title = "Error",
                      content = helpText.server[["alertWideTooFewColumns"]], 
                      append = FALSE,
                      style = "danger")
          

        } else {
          closeAlert(session, "alertWideTooFewColumns")

          # obtain column headers from the wide format data
          # headers for grouping and track id columns
          loc.cols.idvars = names(dm)[1:2]
          
          # headers for time columns
          loc.cols.time = names(dm)[c(-1, -2)]
          
          # check if time columns are numeric
          # from https://stackoverflow.com/a/21154566/1898713
          loc.cols.time.numres = grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", loc.cols.time)
          
          # melt the table only if time columns are numeric
          if (sum(!loc.cols.time.numres) == 0) {
            closeAlert(session, "alertWideMissesNumericTime")
            
            # long to wide
            dm = melt(dm, id.vars = loc.cols.idvars, variable.name = COLRT, value.name = COLY)
            
            # convert column names with time points to a number
            dm[, (COLRT) := as.numeric(levels(get(COLRT)))[get(COLRT)]]
            
          } else {
            dm = NULL

            createAlert(session, "alertAnchorSidePanelDataFormat", "alertWideMissesNumericTime", title = "Error",
                        content = helpText.server[["alertWideMissesNumericTime"]], 
                        append = FALSE,
                        style = "danger")
          }
        }
      }
      
      # no need to isolate updating the counter reactive values!
      counter$dataLoadNuc <- locInLoadNuc
    } else {
      cat("server:dataInBoth else\n")
      dm = NULL
    }
    
    return(dm)
  })
  
  # return column names of the main dt
  getDataNucCols <- reactive({
    if (DEB)
      cat(file = stdout(), 'server:getDataNucCols: in\n')
    
    loc.dt = dataInBoth()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(colnames(loc.dt))
  })
  
  # return dt with an added column with unique track object label
  dataMod <- reactive({
    if (DEB)
      cat(file = stdout(), 'server:dataMod\n')
    
    loc.dt = dataInBoth()
    
    if (is.null(loc.dt))
      return(NULL)
    
    if (input$chBtrackUni) {
      # create unique track ID based on columns specified in input$inSelSite field and combine with input$inSelTrackLabel
      loc.dt[, (COLIDUNI) := do.call(paste, c(.SD, sep = "_")), .SDcols = c(input$inSelSite, input$inSelTrackLabel) ]
    } else {
      # stay with track ID provided in the loaded dataset; has to be unique
      loc.dt[, (COLIDUNI) := get(input$inSelTrackLabel)]
    }
    
    
    # remove trajectories based on uploaded csv
    if (input$chBtrajRem) {
      if (DEB)
        cat(file = stdout(), 'server:dataMod: trajRem not NULL\n')
      
      loc.dt.rem = dataLoadTrajRem()
      loc.dt = loc.dt[!(trackObjectsLabelUni %in% loc.dt.rem[[1]])]
    }
    
    # check if NAs present
    
    
    return(loc.dt)
  })
  
  # return all unique track object labels (created in dataMod)
  # This will be used to display in UI for trajectory highlighting
  getDataTrackObjLabUni <- reactive({
    if (DEB)
      cat(file = stdout(), 'server:getDataTrackObjLabUni\n')
    
    loc.dt = dataMod()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt$trackObjectsLabelUni))
  })
  
  
  # return all unique time points (real time)
  # This will be used to display in UI for box-plot
  # These timepoints are from the original dt and aren't affected by trimming of x-axis
  getDataTpts <- reactive({
    if (DEB)
      cat(file = stdout(), 'server:getDataTpts\n')
    
    loc.dt = dataMod()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(unique(loc.dt[[input$inSelTime]]))
  })
  
  
  
  # prepare data for plotting time courses
  # returns dt with these columns:
  #    realtime - selected from input
  #    y        - measurement selected from input
  #               (can be a single column or result of an operation on two cols)
  #    id       - trackObjectsLabelUni; created in dataMod based on TrackObjects_Label
  #               and FOV column such as Series or Site (if TrackObjects_Label not unique across entire dataset)
  #    group    - grouping variable for facetting from input
  #    mid.in   - column with trajectory selection status from the input file or
  #               highlight status from UI 
  #               (column created if mid.in present in uploaded data or tracks are selected in the UI)
  #    obj.num  - created if ObjectNumber column present in the input data 
  #    pos.x,y  - created if columns with x and y positions present in the input data
  data4trajPlot <- reactive({
    if (DEB)
      cat(file = stdout(), 'server:data4trajPlot\n')
    
    loc.dt = dataMod()
    if (is.null(loc.dt))
      return(NULL)
    
    # create expression for 'y' column based on measurements and math operations selected in UI
    if (input$inSelMath == '')
      loc.s.y = input$inSelMeas1
    else if (input$inSelMath == '1 / ')
      loc.s.y = paste0(input$inSelMath, input$inSelMeas1)
    else
      loc.s.y = paste0(input$inSelMeas1, input$inSelMath, input$inSelMeas2)
    
    # create expression for 'group' column
    # creates a merged column based on other columns from input
    # used for grouping of plot facets
    if (input$chBgroup) {
      if(length(input$inSelGroup) == 0)
        return(NULL)
      
      loc.s.gr = sprintf("paste(%s, sep=';')",
                         paste(input$inSelGroup, sep = '', collapse = ','))
    } else {
      # if no grouping required, fill 'group' column with 0
      # because all the plotting relies on the presence of the group column
      loc.s.gr = "paste('0')"
    }
    

    # column name with time
    loc.s.rt = input$inSelTime
    
    # Assign tracks selected for highlighting in UI
    loc.tracks.highlight = input$inSelHighlight
    locButHighlight = input$chBhighlightTraj
    
    
    # Find column names with position
    loc.s.pos.x = names(loc.dt)[grep('(L|l)ocation.*X|(P|p)os.x|(P|p)osx', names(loc.dt))[1]]
    loc.s.pos.y = names(loc.dt)[grep('(L|l)ocation.*Y|(P|p)os.y|(P|p)osy', names(loc.dt))[1]]
    
    if (DEB)
      cat('server:data4trajPlot:\n   Position columns: ', loc.s.pos.x, loc.s.pos.y, '\n')
    
    if (!is.na(loc.s.pos.x) & !is.na(loc.s.pos.y))
      locPos = TRUE
    else
      locPos = FALSE
    
    
    # Find column names with ObjectNumber
    # This is different from TrackObject_Label and is handy to keep
    # because labels on segmented images are typically ObjectNumber
    loc.s.objnum = names(loc.dt)[grep('(O|o)bject(N|n)umber', names(loc.dt))[1]]
    #cat('data4trajPlot::loc.s.objnum ', loc.s.objnum, '\n')
    if (is.na(loc.s.objnum)) {
      locObjNum = FALSE
    }
    else {
      loc.s.objnum = loc.s.objnum[1]
      locObjNum = TRUE
    }
    
    
    # if dataset contains column mid.in with trajectory filtering status,
    # then, include it in plotting
    if (sum(names(loc.dt) %in% COLIN) > 0)
      locMidIn = TRUE
    else
      locMidIn = FALSE
    
    ## Build expression for selecting columns from loc.dt
    # Core columns
    s.colexpr = paste0('.(',  COLY, ' = ', loc.s.y,
                       ', ', COLID, ' = ', COLIDUNI, 
                       ', ', COLGR, ' = ', loc.s.gr,
                       ', ', COLRT, ' = ', loc.s.rt)
    
    # account for the presence of 'mid.in' column in uploaded data
    # future: choose this column in UI
    if(locMidIn)
      s.colexpr = paste0(s.colexpr, 
                         ',', COLIN, ' = ', COLIN)
    
    # include position x,y columns in uploaded data
    if(locPos)
      s.colexpr = paste0(s.colexpr, 
                         ', ', COLPOSX, '= ', loc.s.pos.x,
                         ', ', COLPOSY, '= ', loc.s.pos.y)
    
    # include ObjectNumber column
    if(locObjNum)
      s.colexpr = paste0(s.colexpr, 
                         ', ', COLOBJN, ' = ', loc.s.objnum)
    
    # close bracket, finish the expression
    s.colexpr = paste0(s.colexpr, ')')
    
    # create final dt for output based on columns selected above
    loc.out = loc.dt[, eval(parse(text = s.colexpr))]
    
    # Convert track ID to a factor.
    # This is necessary for, e.g. merging data with cluster assignments.
    # If input dataset has track ID as a number, such a merge would fail.
    loc.out[, (COLID) := as.factor(get(COLID))]
    
    
    # if track selection ON
    if (locButHighlight){
      # add a 3rd level with status of track selection
      # to a column with trajectory filtering status in the uploaded file
      if(locMidIn)
        loc.out[, mid.in := ifelse(get(COLID) %in% loc.tracks.highlight, 'SELECTED', get(COLIN))]
      else
        # add a column with status of track selection
        loc.out[, mid.in := ifelse(get(COLID) %in% loc.tracks.highlight, 'SELECTED', 'NOT SEL')]
    }
      

    ## Interpolate missing data and NA data points
    # From: https://stackoverflow.com/questions/28073752/r-how-to-add-rows-for-missing-values-for-unique-group-sequences
    # Tracks are interpolated only within first and last time points of every track id
    # Datasets can have different realtime frequency (e.g. every 1', 2', etc),
    # or the frame number metadata can be missing, as is the case for tCourseSelected files that already have realtime column.
    # Therefore, we cannot rely on that info to get time frequency; user must provide this number!
    
    # check if NA's present
    if (sum(is.na(loc.out[[COLY]])))
      createAlert(session, "alertAnchorSidePanelNAsPresent", "alertNAsPresent", title = "Warning",
                  content = helpText.server[["alertNAsPresent"]], 
                  append = FALSE,
                  style = "warning")
    else
      closeAlert(session, "alertNAsPresent")
    
    setkeyv(loc.out, c(COLGR, COLID, COLRT))

    if (input$chBtrajInter) {
      # here we fill missing rows with NA's
      loc.out = loc.out[setkeyv(loc.out[, 
                                        .(seq(min(get(COLRT), na.rm = T), 
                                              max(get(COLRT), na.rm = T), 
                                              input$inSelTimeFreq)), 
                                        by = c(COLGR, COLID)], c(COLGR, COLID, 'V1'))]
      
      # x-check: print all rows with NA's
      if (DEB) {
        cat(file = stdout(), 'server:data4trajPlot: Rows with NAs:\n')
        print(loc.out[rowSums(is.na(loc.out)) > 0, ])
      }
      
      # NA's may be already present in the dataset'.
      # Interpolate (linear) them with na.interpolate as well
      if(locPos)
        s.cols = c(COLY, COLPOSX, COLPOSY)
      else
        s.cols = c(COLY)
      
      # Interpolated columns should be of type numeric (float)
      # This is to ensure that interpolated columns are of porper type.
      
      # Apparently the loop is faster than lapply+SDcols
      for(col in s.cols) {
        #loc.out[, (col) := as.numeric(get(col))]
        data.table::set(loc.out, j = col, value = as.numeric(loc.out[[col]]))

        loc.out[, (col) := na_interpolation(get(col)), by = c(COLID)]        
      }
      
      # loc.out[, (s.cols) := lapply(.SD, na.interpolation), by = c(COLID), .SDcols = s.cols]
      
      
      # !!! Current issue with interpolation:
      # The column mid.in is not taken into account.
      # If a trajectory is selected in the UI,
      # the mid.in column is added (if it doesn't already exist in the dataset),
      # and for the interpolated point, it will still be NA. Not really an issue.
      #
      # Also, think about the current option of having mid.in column in the uploaded dataset.
      # Keep it? Expand it?
      # Create a UI filed for selecting the column with mid.in data.
      # What to do with that column during interpolation (see above)
      
    }    
    
    ## Trim x-axis (time)
    if(input$chBtimeTrim) {
      loc.out = loc.out[get(COLRT) >= input$slTimeTrim[[1]] & get(COLRT) <= input$slTimeTrim[[2]] ]
    }
    
    ## Normalization
    # F-n normTraj adds additional column with .norm suffix
    if (input$chBnorm) {
      loc.out = LOCnormTraj(
        in.dt = loc.out,
        in.meas.col = COLY,
        in.rt.col = COLRT,
        in.rt.min = input$slNormRtMinMax[1],
        in.rt.max = input$slNormRtMinMax[2],
        in.type = input$rBnormMeth,
        in.robust = input$chBnormRobust,
        in.by.cols = if(input$chBnormGroup %in% 'none') NULL else input$chBnormGroup
      )
      
      # Column with normalized data is renamed to the original name
      # Further code assumes column name y produced by data4trajPlot
      
      loc.out[, c(COLY) := NULL]
      setnames(loc.out, 'y.norm', COLY)
    }
    
    return(loc.out)
  })
  
  
  # prepare data for clustering
  # convert from long to wide; return a matrix with:
  # cells as columns
  # time points as rows
  data4clust <- reactive({
    if (DEB)  
      cat(file = stdout(), 'server:data4clust\n')
    
    loc.dt = data4trajPlotNoOut()
    if (is.null(loc.dt))
      return(NULL)
    
    # convert from long to wide format
    loc.dt.wide = dcast(loc.dt, 
                    reformulate(response = COLID, termlabels = COLRT), 
                    value.var = COLY)
    
    # store row names for later
    loc.rownames = loc.dt.wide[[COLID]]
    
    # omit first column that contains row names
    loc.m.out = as.matrix(loc.dt.wide[, -1])
    
    # assign row names to the matrix
    rownames(loc.m.out) = loc.rownames
    
    return(loc.m.out)
  }) 
  
  
  # prepare data with stimulation pattern
  # this dataset is displayed underneath of trajectory plot (modules/trajPlot.R) as geom_segment
  data4stimPlot <- reactive({
    if (DEB)  
      cat(file = stdout(), 'server:data4stimPlot\n')
    
    if (input$chBstim) {
      if (DEB)  
        cat(file = stdout(), 'server:data4stimPlot: stim not NULL\n')
      
      loc.dt.stim = dataLoadStim()
      return(loc.dt.stim)
    } else {
      if (DEB)  
        cat(file = stdout(), 'server:data4stimPlot: stim is NULL\n')
      
      return(NULL)
    }
  })
  
  # prepare y-axis label in time series plots, depending on UI setting
  
  createYaxisLabel = reactive({
    locLabel = input$inSelMeas1
    
    
    
    return(locLabel)
  })
  
  # download data as prepared for plotting
  # after all modification
  output$downloadDataClean <- downloadHandler(
    filename = FCSVTCCLEAN,
    content = function(file) {
      write.csv(data4trajPlotNoOut(), file, row.names = FALSE)
    }
  )
  
  # Plotting-trajectories ----

  # UI for selecting trajectories
  # The output data table of data4trajPlot is modified based on inSelHighlight field
  output$varSelHighlight = renderUI({
    if (DEB)  
      cat(file = stdout(), 'server:varSelHighlight\n')
    
    locBut = input$chBhighlightTraj
    if (!locBut)
      return(NULL)
    
    loc.v = getDataTrackObjLabUni()
    if (!is.null(loc.v)) {
      selectInput(
        'inSelHighlight',
        'Select one or more trajectories:',
        loc.v,
        width = '100%',
        multiple = TRUE
      )
    }
  })
  
  # Taking out outliers 
  data4trajPlotNoOut = callModule(modSelOutliers, 'returnOutlierIDs', data4trajPlot)
  
  # Trajectory plotting - ribbon
  callModule(modTrajRibbonPlot, 'modTrajRibbon', 
             in.data = data4trajPlotNoOut,
             in.data.stim = data4stimPlot,
             in.fname = function() return(FPDFTCMEAN))
  
  # Trajectory plotting - individual
  callModule(modTrajPlot, 'modTrajPlot', 
             in.data = data4trajPlotNoOut, 
             in.data.stim = data4stimPlot,
             in.fname = function() {return(FPDFTCSINGLE)},
             in.ylab = createYaxisLabel)
  
  # Trajectory plotting - PSD
  callModule(modPSDPlot, 'modPSDPlot',
             in.data = data4trajPlotNoOut,
             in.fname = function() {return(FPDFTCPSD)})
  
  
  # Tabs ----
  ###### AUC calculation and plotting
  callModule(tabAUCplot, 'tabAUC', data4trajPlotNoOut, in.fname = function() return(FPDFBOXAUC))
  
  ###### Box-plot
  callModule(tabDistPlot, 'tabDistPlot', data4trajPlotNoOut, in.fname = function() return(FPDFBOXTP))
  
  ###### Scatter plot
  callModule(tabScatterPlot, 'tabScatter', data4trajPlotNoOut, in.fname = function() return(FPDFSCATTER))
  
  ##### Hierarchical estimation
  callModule(clustValid, 'tabClValid', data4clust)

  ##### Hierarchical clustering
  callModule(clustHier, 'tabClHier', data4clust, data4trajPlotNoOut, data4stimPlot)
  
  ##### Sparse hierarchical clustering using sparcl
  callModule(clustHierSpar, 'tabClHierSpar', data4clust, data4trajPlotNoOut, data4stimPlot)
})
