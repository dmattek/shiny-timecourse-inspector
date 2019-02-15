#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This is the module of a Shiny web application.
# Outlier identification, selection

# UI-remove-outliers ----
modSelOutliersUI = function(id, label = "Outlier Selection") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Remove outliers"
    ),
    fluidRow(
      column(2, 
             numericInput(ns('numOutliersPerc'),
                         label = '% of data',
                         min = 0,
                         max = 100,
                         value = 0, 
                         step = 0.05, width = '100px'),
             checkboxInput(ns('chBtrajInter'), 'Interpolate gaps?', value = F)
      ),
      column(2, 
             radioButtons(ns('rbOutliersType'), 
                          label = 'From', 
                          choices = c('top' = 'top', 'middle' = 'mid', 'bottom' = 'bot'))
             ),
      column(3,
             sliderInput(ns('slOutliersGapLen'),
                         label = 'Remove tracks with gaps equal to or longer than',
                         min = 1,
                         max = 10,
                         value = 1, 
                         step = 1)
      ),
      column(3,
             downloadButton(ns('downOutlierCSV'), label = 'CSV with outlier IDs'),
             htmlOutput(ns("txtOutliersPerc"))
      )
    )
  )
}

# Server-remove-outliers ----
modSelOutliers = function(input, output, session, in.data) {

  # reactive counter to hold number of tracks before and after outlier removal
  nCellsCounter <- reactiveValues(
    nCellsOrig = 0,
    nCellsAfter = 0,
    nOutlierTpts = 0
  )
  
  # reactive vector with cell ids
  vOut = reactiveValues(
    id = NULL
  )

  
  
  # Display number of tracks and outliers  
  output$txtOutliersPerc <- renderText({ 
    cat(file = stdout(), 'modSelOutliers: txtOutliersPerc\n')
    
      sprintf('<b>%d total track(s)<br>%d outlier track(s)<br>%d outlier point(s)</b><br>', 
            nCellsCounter[['nCellsOrig']], 
            nCellsCounter[['nCellsOrig']] - nCellsCounter[['nCellsAfter']],
            nCellsCounter[['nOutlierTpts']])
    })
  
  # button for downloading CSV with ids of removed tracks
  output$downOutlierCSV <- downloadHandler(
    filename = FCSVOUTLIERS,
    content = function(file) {
      loc.dt = vOut[['id']]
      
      if (is.null(loc.dt))
        return(NULL)
      else
        write.csv(unique(loc.dt[, (COLID), with = F]), file, row.names = FALSE, quote = F)
    }
  )
  
# Identify outliers and remove them from dt
  dtReturn = reactive({ 
    cat(file = stdout(), 'modSelOutliers: dtReturn\n')
    
    loc.out = in.data()
    
    if (is.null(loc.out)) {
      return(NULL)
    }

    # Remove outliers if the field with percentage of data to remove is greater than 0
    if (input$numOutliersPerc > 0) {
      
      # store the number of trajectories before prunning
      nCellsCounter[['nCellsOrig']] = length(unique(loc.out[['id']]))

      # scale all measurement points      
      loc.out[, y.sc := scale(get(COLY))]  

      # Identify outlier points
      # In the UI, user selectes percentage of data to remove from the bottom, middle, or top part.
      # loc.outpts stores outlier points
      switch(input$rbOutliersType,
        'top' = {loc.outpts = loc.out[ y.sc > quantile(y.sc, 1 - input$numOutliersPerc * 0.01, na.rm = T)]},
        'mid' = {loc.outpts = loc.out[ y.sc < quantile(y.sc, input$numOutliersPerc * 0.005, na.rm = T) | 
                                     y.sc > quantile(y.sc, 1 - input$numOutliersPerc * 0.005, na.rm = T)]},
        'bot' = {loc.outpts = loc.out[ y.sc < quantile(y.sc, input$numOutliersPerc * 0.01, na.rm = T)]}
      )
      
      if (DEB) {
        cat(file = stdout(), 'selOutliers.dtReturn: Outlier points:\n')
        print(loc.outpts)
      }
        
      if (input$slOutliersGapLen > 1) {
        # remove tracks with gaps longer than the value set in slOutliersGapLen
        # shorter gaps are interpolated linearly
        
        # add index column per trajecory
        loc.out[, (COLIDX) := 1:.N, by = c(COLID)]

        # remove single outlier points (anti-join)
        # From: https://stackoverflow.com/a/46333620/1898713
        loc.out = loc.out[!loc.outpts, on = names(loc.outpts)]

        # calculate diff on index column to see the length of gaps due to removed points
        # the value of that column corresponds to the gap length (hence the "-1")
        loc.out[, (COLIDXDIFF) := c(1, diff(get(COLIDX))) - 1, by = c(COLID)]

        # get track ids where the max gap is equal to or longer than the threshold
        loc.idgaps = loc.out[, max(get(COLIDXDIFF)), by = c(COLID)][V1 >= input$slOutliersGapLen, get(COLID)]
        
        if (DEB) {
          cat(file = stdout(), '\nselOutliers.dtReturn: Track IDs with max gap >= threshold:\n')
          if (length(loc.idgaps) > 0)
            print(loc.idgaps) else
              cat("none\n")
        }
        
        # remove outlier tracks with gaps longer than the value set in slOutliersGapLen
        if (length(loc.idgaps) > 0)
          loc.out = loc.out[!(get(COLID) %in% unique(loc.idgaps))]

        # clean
        loc.out[, c(COLIDX, COLIDXDIFF) := NULL]

        # interpolate gaps due to outliers
        if (input$chBtrajInter) {
          # fill removed outliers with NA's
          setkeyv(loc.out, c(COLGR, COLID, COLRT))
          loc.out = loc.out[setkeyv(loc.out[, .(seq(min(get(COLRT), na.rm = T), max(get(COLRT), na.rm = T), 1)), by = c(COLGR, COLID)], c(COLGR, COLID, 'V1'))]

          # x-check: print all rows with NA's
          if (DEB) {
            cat(file = stdout(), '\nselOutliers.dtReturn: Rows with NAs due to outliers:\n')
            print(loc.out[rowSums(is.na(loc.out)) > 0, ])
          }
          
          # NA's may be already present in the dataset'.
          # Interpolate (linear) them with na.interpolate as well
          if( (COLPOSX %in% names(loc.out)) & (COLPOSY %in% names(loc.out)) )
            s.cols = c(COLY, COLPOSX, COLPOSY)
          else
            s.cols = c(COLY)
          
          
          # Apparently the loop is faster than lapply+SDcols
          for(col in s.cols) {
            # Interpolated columns should be of type numeric (float)
            # This is to ensure that interpolated columns are of porper type.
            data.table::set(loc.out, j = col, value = as.numeric(loc.out[[col]]))
            
            loc.out[, (col) := na.interpolation(get(col)), by = c(COLID)]        
          }
        } 

        
      } else {
        # remove outlier tracks with gaps of length 1 time point
        loc.out = loc.out[!(get(COLID) %in% unique(loc.outpts[[COLID]]))]
      }

      # clean
      loc.out[, y.sc := NULL]

      # count number of trajectories after removing outlier tracks
      nCellsCounter[['nCellsAfter']] = length(unique(loc.out[[COLID]]))
      
      # count number of outlier points
      nCellsCounter[['nOutlierTpts']] = length(loc.outpts[[COLID]])
      
      
      # store a vector of outlier timepoints with the corresponding IDs
      vOut[['id']] = loc.outpts
    }
    
    # return cleaned dt
    if (nrow(loc.out) < 1)
      return(NULL) else
        return(loc.out)
    
  })
  
  return(dtReturn)
}