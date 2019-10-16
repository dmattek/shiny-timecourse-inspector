#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# Auxilary functions & definitions of global constants
#


library(ggplot2)
library(RColorBrewer)
library(gplots) # for heatmap.2
library(grid) # for modifying grob
library(Hmisc) # for CI calculation


# Global parameters ----
# number of miliseconds to delay reactions to changes in the UI
# used to delay output from sliders
MILLIS = 1000

# Number of significant digits to display in table stats
SIGNIFDIGITSINTAB = 3

# if true, additional output printed to R console
DEB = T

# font sizes in pts for plots in the manuscript
# PLOTFONTBASE = 8
# PLOTFONTAXISTEXT = 8
# PLOTFONTAXISTITLE = 8
# PLOTFONTFACETSTRIP = 10
# PLOTFONTLEGEND = 8

# font sizes in pts for screen display
PLOTFONTBASE = 16
PLOTFONTAXISTEXT = 16
PLOTFONTAXISTITLE = 16
PLOTFONTFACETSTRIP = 20
PLOTFONTLEGEND = 16

# height (in pixels) of ribbon and single traj. plots
PLOTRIBBONHEIGHT = 500 # in pixels
PLOTTRAJHEIGHT = 500 # in pixels
PLOTPSDHEIGHT = 500 # in pixels
PLOTBOXHEIGHT = 500 # in pixels
PLOTSCATTERHEIGHT = 500 # in pixels
PLOTWIDTH = 85 # in percent

# default number of facets in plots
PLOTNFACETDEFAULT = 3

# internal column names
COLRT   = 'time'
COLY    = 'y'
COLID   = 'id'
COLIDUNI = 'trackObjectsLabelUni'
COLGR   = 'group'
COLIN   = 'mid.in'
COLOBJN = 'obj.num'
COLPOSX = 'pos.x'
COLPOSY = 'pos.y'
COLIDX = 'IDX'
COLIDXDIFF = 'IDXdiff'
COLCL = 'cl'
COLNTRAJ = "nCells"

# file names
FCSVOUTLIERS = 'outliers.csv'
FCSVTCCLEAN  = 'tCoursesProcessed.csv'
FPDFTCMEAN   = "tCoursesMeans.pdf"
FPDFTCSINGLE = "tCourses.pdf"
FPDFTCPSD    = 'tCoursesPsd.pdf'
FPDFBOXAUC   = 'distributionAUC.pdf'
FPDFBOXTP    = 'distributionTP.pdf'
FPDFSCATTER  = 'scatter.pdf'

# Colour definitions ----
rhg_cols <- c(
  "#771C19",
  "#AA3929",
  "#E25033",
  "#F27314",
  "#F8A31B",
  "#E2C59F",
  "#B6C5CC",
  "#8E9CA3",
  "#556670",
  "#000000"
)

md_cols <- c(
  "#FFFFFF",
  "#F8A31B",
  "#F27314",
  "#E25033",
  "#AA3929",
  "#FFFFCC",
  "#C2E699",
  "#78C679",
  "#238443"
)

# list of palettes for the heatmap
l.col.pal = list(
  "Spectral" = 'Spectral',
  "Red-Yellow-Green" = 'RdYlGn',
  "Red-Yellow-Blue" = 'RdYlBu',
  "Greys" = "Greys",
  "Reds" = "Reds",
  "Oranges" = "Oranges",
  "Greens" = "Greens",
  "Blues" = "Blues"
)

# list of palettes for the dendrogram
l.col.pal.dend = list(
  "Rainbow" = 'rainbow_hcl',
  "Sequential" = 'sequential_hcl',
  "Heat" = 'heat_hcl',
  "Terrain" = 'terrain_hcl',
  "Diverge HCL" = 'diverge_hcl',
  "Diverge HSV" = 'diverge_hsv'
)

# list of palettes for the dendrogram
l.col.pal.dend.2 = list(
  "Colorblind 10" = 'Color Blind',
  "Tableau 10" = 'Tableau 10',
  "Tableau 20" = 'Tableau 20',
  "Classic 10" = "Classic 10",
  "Classic 20" = "Classic 20",
  "Traffic 9" = 'Traffic',
  "Seattle Grays 5" = 'Seattle Grays'
)

# Help text ----
helpText.server = c(
  alDataFormat =  paste0(
    "<p>Switch between long and wide formats of input data. ",
    "TCI accepts CSV or compressed CSV files (gz or bz2).</p>",
    "<p><b>Long format</b> - a row is a single data point and consecutive time series are arranged vertically. ",
    "Data file should contain at least 3 columns separated with a comma:</p>",
    "<li>Identifier of a time series</li>",
    "<li>Time points</li>",
    "<li>A time-varying variable</li>",
    "<br>",
    "<p><b>Wide format</b> - a row is a time series with columns as time points.",
    "At least 3 columns shuold be present:</p>",
    "<li>First two columns in wide format should contain grouping and track IDs</li>",
    "<li>A column with a time point. Headers of columns with time points need to be numeric</li>"
  ),
  inDataGen1 =   paste0(
    "Generate 3 groups with 20 random synthetic time series. ",
    "Every time series contains 101 time points. ",
    "Track IDs are unique across entire dataset."
  ),
  chBtrajRem =   paste0(
    "Load CSV file with a column of track IDs for removal. ",
    "IDs should correspond to those used for plotting."
  ),
  chBstim =      paste0(
    "Load CSV file with stimulation pattern. Should contain 5 columns: ",
    "grouping, start and end time points of stimulation, start and end of y-position, dummy column with ID."
  ),
  chBtrajInter = paste0(
    "Interpolate missing measurements indicated with NAs in the data file. ",
    "In addition, interpolate a row that is completely missing from the data. ",
    "The interval of the time column must be provided to know which rows are missing."
  ),
  chBtrackUni =  paste0(
    "If the track ID in the uploaded dataset is unique only within a group (e.g. an experimental condition), ",
    "make it unique by prepending other columns to the track ID (typically a grouping column)."
  ),
  chBgroup    = "Select columns to group data according to treatment, condition, etc.",
  inSelMath   = "Select math operation to perform on a single or two measurement columns,",
  chBtimeTrim = "Trim time for further processing.",
  chBnorm     = "Divide measurements by the mean/median or calculate z-score with respect to selected time span.",
  rBnormMeth  = "Fold-change or z-score with respect to selected time span.",
  slNormRtMinMax = "Normalise with respect to this time span.",
  chBnormRobust  = "Calculate fold-change and z-score using the median and Median Absolute Deviation, instead of the mean and standard deviation.",
  chBnormGroup   = "Normalise to mean/median of selected time calculated globally, per group, or for individual time series.",
  downloadDataClean = "Download all time series after modifications in this panel.",
  alertNAsPresent              = "NAs present in the measurement column. Consider interpolation.",
  alertNAsPresentLong2WideConv = "Missing rows. Consider interpolation.",
  alertTimeFreq0 = "The interval between 2 time points has to be greater than 0.",
  alertWideMissesNumericTime = "Non-numeric headers of time columns. Data in wide format should have numeric column headers corresponding to time points.",
  alertWideTooFewColumns     = "Insufficient columns. Data in wide format should contain at least 3 columns: grouping, track ID, and a single time point."
)

# Functions for data processing ----
#' Calculate the mean and CI around time series
#'
#' @param in.dt Data table in long format
#' @param in.col.meas Name of the column with the measurement
#' @param in.col.by Column names for grouping (default NULL - no grouping). Typically, you want to use at least a column with time.
#' @param in.type Choice of normal approximation or boot-strapping
#' @param ... Other params passed to smean.cl.normal and smean.cl.boot; these include \code{conf.int} for the confidence level, \code{B} for the number of boot-strapping iterations.
#'
#' @return Datatable with columns: Mean, lower and upper CI, and grouping columns if provided.
#' @export
#' @import data.table
#' @import Hmisc
#'
#' @examples
#'
#'
#' # generate synthetic time series; 100 time points long, with 10 randomly placed NAs
#' dt.tmp = genTraj(100, 10, 6, 3, in.addna = 10)
#'
#' # calculate single stats from all time points
#' calcTrajCI(dt.tmp, 'objCyto_Intensity_MeanIntensity_imErkCor')
#'
#' # calculate the mean and CI along the time course
#' calcTrajCI(dt.tmp, 'objCyto_Intensity_MeanIntensity_imErkCor', 'Metadata_RealTime')
LOCcalcTrajCI = function(in.dt,
                         in.col.meas,
                         in.col.by = NULL,
                         in.type = c('normal', 'boot'),
                         ...) {
  in.type = match.arg(in.type)
  
  if (in.type %like% 'normal')
    loc.dt = in.dt[, as.list(smean.cl.normal(get(in.col.meas), ...)), by = in.col.by]
  else
    loc.dt = in.dt[, as.list(smean.cl.boot(get(in.col.meas), ...)), by = in.col.by]
  
  return(loc.dt)
}


#' Calculate standard error of the mean
#'
#' @param x Vector
#' @param na.rm Remove NAs; default = FALSE
#'
#' @return A scalar with the result
#' @export
#'
#' @examples
LOCstderr = function(x, na.rm = FALSE) {
  if (na.rm)
    x = na.omit(x)
  
  return(sqrt(var(x) / length(x)))
}

#' Calculate the power spectrum density for time-series
#'
#' @param in.dt Data table in long format
#' @param in.col.meas Name of the column with the measurement
#' @param in.col.id Name of the column with the unique series identifier
#' @param in.col.by Column names for grouping (default NULL - no grouping). PSD of individual trajectories will be averaged within a group.
#' @param in.method Name of the method for PSD estimation, must be one of c("pgram", "ar"). Default to "pgram*.
#' @param in.return.period Wheter to return densities though periods (1/frequencies) instead of frequencies.
#' @param ... Other paramters to pass to stats::spectrum()
#'
#' @return Datatable with columns: (frequency or period), spec (the density) and grouping column
#' @export
#' @import data.table
#'
#' @examples
LOCcalcPSD <- function(in.dt,
                       in.col.meas,
                       in.col.id,
                       in.col.by,
                       in.method = "pgram",
                       in.return.period = TRUE,
                       in.time.btwPoints = 1,
                       ...) {
  require(data.table)
  # Method "ar" returns $spec as matrix whereas "pgram" returns a vector, custom function to homogenze output format
  mySpectrum <- function(x, ...) {
    args_spec <- list(x = x, plot = FALSE)
    inargs <- list(...)
    args_spec[names(inargs)] <- inargs
    out <- do.call(spectrum, args_spec)
    out$spec <- as.vector(out$spec)
    return(out)
  }
  if (!in.method %in% c("pgram", "ar")) {
    stop('Method should be one of: c("pgram", "ar"')
  }
  dt_spec <-
    in.dt[, (mySpectrum(get(in.col.meas), plot = FALSE, method = in.method)[c("freq", "spec")]), by = in.col.id]
  dt_group <- in.dt[, .SD[1, get(in.col.by)], by = in.col.id]
  setnames(dt_group, "V1", in.col.by)
  dt_spec <- merge(dt_spec, dt_group, by = in.col.id)
  dt_agg <-
    dt_spec[, .(spec = mean(spec)), by = c(in.col.by, "freq")]
  if (in.return.period) {
    dt_agg[, period := 1 / freq]
    dt_agg[, freq := NULL]
    # Adjust period unit to go from frame unit  to time unit
    dt_agg[, period := period * in.time.btwPoints]
  } else {
    dt_agg[, freq := freq * (1 / in.time.btwPoints)]
    setnames(dt_agg, "freq", "frequency")
  }
  return(dt_agg)
}


#' Generate synthetic CellProfiler output with single-cell time series
#'
#' @param in.ntpts Number of time points (default 60)
#' @param in.ntracks Number of tracks per FOV (default 10)
#' @param in.nfov Number of FOV (default 6)
#' @param in.nwells Number of wells (default 1)
#' @param in.addna Number of NAs to add randomly in the data (default NULL)
#'
#' @return Data table with the follwoing columns: Metadata_Site, Metadata_Well, Metadata_RealTime, objCyto_Intensity_MeanIntensity_imErkCor (normal distributed),
#' objNuc_Intensity_MeanIntensity_imErkCor (normal distributed), objNuc_Location_X and objNuc_Location_Y (uniform ditributed), TrackLabel
#' @export
#' @import data.table

LOCgenTraj <-
  function(in.ntpts = 60,
           in.ntracks = 10,
           in.nfov = 6,
           in.nwells = 1,
           in.addna = NULL,
           in.addout = NULL) {
    x.rand.1 = c(
      rnorm(in.ntpts * in.ntracks * in.nfov * 1 / 3, 0.5, 0.1),
      rnorm(in.ntpts * in.ntracks * in.nfov * 1 / 3,   1, 0.2),
      rnorm(in.ntpts * in.ntracks * in.nfov * 1 / 3,  2, 0.5)
    )
    x.rand.2 = c(
      rnorm(in.ntpts * in.ntracks * in.nfov * 1 / 3, 0.25, 0.1),
      rnorm(in.ntpts * in.ntracks * in.nfov * 1 / 3, 0.5, 0.2),
      rnorm(in.ntpts * in.ntracks * in.nfov * 1 / 3, 1, 0.2)
    )
    
    # add NA's for testing
    if (!is.null(in.addna)) {
      locTabLen = length(x.rand.1)
      x.rand.1[round(runif(in.addna) * locTabLen)] = NA
      x.rand.2[round(runif(in.addna) * locTabLen)] = NA
    }
    
    # add outliers for testing
    if (!is.null(in.addout)) {
      locTabLen = length(x.rand.1)
      x.rand.1[round(runif(in.addout) * locTabLen)] = 5
      x.rand.2[round(runif(in.addout) * locTabLen)] = 5
    }
    
    x.arg = rep(seq(1, in.ntpts), in.ntracks * in.nfov)
    
    dt.nuc = data.table(
      well = rep(LETTERS[1:in.nwells], each = in.ntpts * in.nfov * in.ntracks / in.nwells),
      group = rep(1:in.nfov, each = in.ntpts * in.ntracks),
      time = x.arg,
      y1 = x.rand.1,
      y2  = x.rand.2,
      posx = runif(
        in.ntpts * in.ntracks * in.nfov,
        min = 0,
        max = 1
      ),
      posy = runif(
        in.ntpts * in.ntracks * in.nfov,
        min = 0,
        max = 1
      ),
      id = rep(1:(in.ntracks * in.nfov), each = in.ntpts)
    )
    
    return(dt.nuc)
  }

LOCgenTraj2 <-
  function(n_perGroup = 20,
           sd_noise = 0.01,
           sampleFreq = 0.2,
           endTime = 50)
  {
    # Function definition ----------------------------------
    sim_expodecay_lagged_stim <-
      function (n,
                noise,
                interval.stim = 5,
                lambda = 0.2,
                freq = 0.2,
                end = 40)
      {
        require(data.table)
        tvec <- seq(0, end, by = freq)
        stim_time <- seq(interval.stim, end, interval.stim)
        stim_time_matrix <-
          matrix(stim_time, nrow = length(stim_time),
                 ncol = n)
        noise_matrix <- abs(replicate(n, rnorm(
          n = length(stim_time),
          mean = 0,
          sd = noise
        )))
        stim_time_matrix <- stim_time_matrix + noise_matrix
        trajs <- matrix(0, nrow = length(tvec), ncol = n)
        for (col in 1:ncol(stim_time_matrix)) {
          for (row in 1:nrow(stim_time_matrix)) {
            index <- which(tvec >= stim_time_matrix[row, col])[1]
            trajs[index, col] <- 1
          }
        }
        decrease_factor <- exp(-lambda * freq)
        for (col in 1:ncol(trajs)) {
          for (row in 2:nrow(trajs)) {
            if (trajs[row, col] != 1) {
              trajs[row, col] <- trajs[row - 1, col] * decrease_factor
            }
          }
        }
        trajs <- as.data.table(trajs)
        trajs <- cbind(seq(0, end, by = freq), trajs)
        colnames(trajs)[1] <- "Time"
        trajs <- melt(trajs, id.vars = "Time")
        return(trajs)
      }
    
    
    # Dataset creation -----------------------------------------------
    dt1 <-
      sim_expodecay_lagged_stim(
        n = n_perGroup,
        noise = 0.75,
        interval.stim = 10,
        lambda = 0.4,
        freq = sampleFreq,
        end = endTime
      )
    dt2 <-
      sim_expodecay_lagged_stim(
        n = n_perGroup,
        noise = 0.75,
        interval.stim = 10,
        lambda = 0.1,
        freq = sampleFreq,
        end = endTime
      )
    dt3 <-
      sim_expodecay_lagged_stim(
        n = n_perGroup,
        noise = 0.75,
        interval.stim = 10,
        lambda = 0.4,
        freq = sampleFreq,
        end = endTime
      )
    dt3[, value := value / 3]
    
    dt1[, Group := "fastDecay"]
    dt2[, Group := "slowDecay"]
    dt3[, Group := "lowAmplitude"]
    
    dt <- rbindlist(list(dt1, dt2, dt3))
    dt[, ID := sprintf("%s_%02d", Group, as.integer(gsub('[A-Z]', '', variable)))]
    dt[, variable := NULL]
    dt[, Group := as.factor(Group)]
    
    dt[, value := value + runif(1, -0.1, 0.1), by = .(Group, ID)]
    noise_vec <- rnorm(n = nrow(dt), mean = 0, sd = sd_noise)
    dt[, value := value + noise_vec]
    
    setnames(dt, "value", "Meas")
    setcolorder(dt, c("Group", "ID", "Time", "Meas"))
    
    return(dt)
  }

#' Normalize Trajectory
#'
#' Returns original dt with an additional column with normalized quantity.
#' The column to be normalised is given by 'in.meas.col'.
#' The name of additional column is the same as in.meas.col but with ".norm" suffix added.
#' Normalisation is based on part of the trajectory;
#' this is defined by in.rt.min and max, and the column with time in.rt.col.#'
#'
#' @param in.dt Data table in long format
#' @param in.meas.col String with the column name to normalize
#' @param in.rt.col String with the colum name holding time
#' @param in.rt.min Lower bound for time period used for normalization
#' @param in.rt.max Upper bound for time period used for normalization
#' @param in.by.cols String vector with 'by' columns to calculate normalization per group; if NULL, no grouping is done
#' @param in.robust Whether robust measures should be used (median instead of mean, mad instead of sd); default TRUE
#' @param in.type Type of normalization: z.score or mean (i.e. fold change w.r.t. mean); default 'z-score'
#'
#' @return Returns original dt with an additional column with normalized quantity.
#' @export
#' @import data.table

LOCnormTraj = function(in.dt,
                       in.meas.col,
                       in.rt.col = COLRT,
                       in.rt.min = 10,
                       in.rt.max = 20,
                       in.by.cols = NULL,
                       in.robust = TRUE,
                       in.type = 'z.score') {
  loc.dt <-
    copy(in.dt) # copy so as not to alter original dt object w intermediate assignments
  
  if (is.null(in.by.cols)) {
    if (in.robust)
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) >= in.rt.min &
                                 get(in.rt.col) <= in.rt.max, .(meas.md = median(get(in.meas.col), na.rm = TRUE),
                                                                meas.mad = mad(get(in.meas.col), na.rm = TRUE))]
    else
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) >= in.rt.min &
                                 get(in.rt.col) <= in.rt.max, .(meas.md = mean(get(in.meas.col), na.rm = TRUE),
                                                                meas.mad = sd(get(in.meas.col), na.rm = TRUE))]
    
    loc.dt = cbind(loc.dt, loc.dt.pre.aggr)
  }  else {
    if (in.robust)
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) >= in.rt.min &
                                 get(in.rt.col) <= in.rt.max, .(meas.md = median(get(in.meas.col), na.rm = TRUE),
                                                                meas.mad = mad(get(in.meas.col), na.rm = TRUE)), by = in.by.cols]
    else
      loc.dt.pre.aggr = loc.dt[get(in.rt.col) >= in.rt.min &
                                 get(in.rt.col) <= in.rt.max, .(meas.md = mean(get(in.meas.col), na.rm = TRUE),
                                                                meas.mad = sd(get(in.meas.col), na.rm = TRUE)), by = in.by.cols]
    
    loc.dt = merge(loc.dt, loc.dt.pre.aggr, by = in.by.cols)
  }
  
  
  if (in.type == 'z.score') {
    loc.dt[, meas.norm := (get(in.meas.col) - meas.md) / meas.mad]
  } else {
    loc.dt[, meas.norm := (get(in.meas.col) / meas.md)]
  }
  
  setnames(loc.dt, 'meas.norm', paste0(in.meas.col, '.norm'))
  
  loc.dt[, c('meas.md', 'meas.mad') := NULL]
  return(loc.dt)
}


#' Interpolate missing rows in time series
#'
#' @param inDT Data.table in long format with time series
#' @param inColGr Name of the grouipng column
#' @param inColID Name of the column with unique time series IDs
#' @param inColT Name of the column with time
#' @param inColY Name of the column(s) with variables to interpolate
#' @param inTfreq Interval between two time points
#' @param inDeb Debugging, extended output
#'
#' @return Data.table with interpolated missing time points
#' @export
#'
#' @examples
LOCinterpolate = function(inDT, inColGr, inColID, inColT, inColY, inTfreq = 1, inDeb = F) {
  
  if(is.null(inDT))
    return(NULL)
  else
    loc.out = inDT
  
  # Stretch time series by every time series' min/max time
  # Gaps filled with NA's
  setkeyv(loc.out, c(inColGr, inColID, inColT))
  loc.out = loc.out[setkeyv(loc.out[, 
                                    .(seq(min(get(inColT), na.rm = T), 
                                          max(get(inColT), na.rm = T), 
                                          inTfreq)), 
                                    by = c(inColGr, inColID)], c(inColGr, inColID, 'V1'))]
  
  # x-check: print all rows with NA's
  if (inDeb) {
    cat(file = stdout(), '\nLOCinterpolate: Rows with NAs to interpolate:\n')
    print(loc.out[rowSums(is.na(loc.out)) > 0, ])
  }
  
  # Apparently the loop is faster than lapply+SDcols
  for(col in inColY) {
    if(inDeb)
      cat(file = stdout(), sprintf("Interpolating NAs in column: %s\n", col))
    
    # Interpolated columns should be of type numeric (float)
    # This is to ensure that interpolated columns are of porper type.
    data.table::set(loc.out, j = col, value = as.numeric(loc.out[[col]]))
    
    loc.out[, (col) := na_interpolation(get(col)), by = c(inColID)]        
  }
  
  return(loc.out)
}

#' Remove outlier time points and/or tracks depdending on maximum permissible gap length due to outliers
#'
#' @param inDT Data.table in long format with main dataset
#' @param inDTout Data.table in long format with rows of inDT that include outlier time points
#' @param inColID Name of the column with unique time series IDs
#' @param inGapLen Length of the maximum allowed gap. Tracks with gaps longer than threshold will be removed. Shorter gaps will be interpolated
#' @param inDeb Debugging, extended output
#'
#' @return Data.table with time points and/or time series removed
#' @export
#'
#' @examples
LOCremoveOutTracks = function(inDT, inDTout, inColID, inGapLen = 0, inDeb = F) {
  
  if(is.null(inDT))
    return(NULL)
  else
    loc.out = inDT
  
  # add index column per trajecory
  loc.out[, myColIdx := 1:.N, by = c(inColID)]
  
  # remove single outlier points (anti-join)
  # From: https://stackoverflow.com/a/46333620/1898713
  loc.out = loc.out[!inDTout, on = names(inDTout)]
  
  # calculate diff on index column to see the length of gaps due to removed points
  # the value of that column corresponds to the gap length (hence the "-1")
  loc.out[, 
          myColIdxDiff := c(1, diff(myColIdx)) - 1, 
          by = c(inColID)]
  
  # get track ids where the max gap is longer than the threshold
  loc.idgaps = loc.out[, 
                       max(myColIdxDiff), 
                       by = c(inColID)][V1 > inGapLen, get(inColID)]
  
  if (inDeb) {
    cat(file = stdout(), sprintf('\nLOCremoveTracks: Track IDs with max gap >= %d:\n', inGapLen))
    if (length(loc.idgaps) > 0)
      print(loc.idgaps) else
        cat("none\n")
  }
  
  # remove outlier tracks with gaps longer than the value set in slOutliersGapLen
  if (length(loc.idgaps) > 0)
    loc.out = loc.out[!(get(inColID) %in% unique(loc.idgaps))]
  
  # clean
  loc.out[, `:=`(myColIdx = NULL, myColIdxDiff = NULL)]
  
  return(loc.out)
}

# Cluster validation ----

#Customize factoextra functions to accept dissimilarity matrix from start. Otherwise can't use distance functions that are not in base R, like DTW.
# Inherit and adapt hcut function to take input from UI, used for fviz_clust

LOChcut <-
function(x,
         k = 2,
         isdiss = inherits(x, "dist"),
         hc_func = "hclust",
         hc_method = "average",
         hc_metric = "euclidean") {

    if (!inherits(x, "dist")) {
    stop("x must be a distance matrix")
  }
  return(
    factoextra::hcut(
      x = x,
      k = k,
      isdiss = TRUE,
      hc_func = hc_func,
      hc_method = hc_method,
      hc_metric = hc_metric
    )
  )
}

# Modified from factoextra::fviz_nbclust
# Allow (actually enforce) x to be a distance matrix; no GAP statistics for compatibility

LOCnbclust <-
  function (x,
            FUNcluster = LOChcut,
            method = c("silhouette", "wss"),
            k.max = 10,
            verbose = FALSE,
            barfill = "steelblue",
            barcolor = "steelblue",
            linecolor = "steelblue",
            print.summary = TRUE,
            ...)
  {
    set.seed(123)
    
    if (k.max < 2)
      stop("k.max must bet > = 2")
    
    method = match.arg(method)
    
    if (!inherits(x, c("dist")))
      stop("x should be an object of class dist")
    
    else if (is.null(FUNcluster))
      stop(
        "The argument FUNcluster is required. ",
        "Possible values are kmeans, pam, hcut, clara, ..."
      )
    
    else if (method %in% c("silhouette", "wss")) {
      diss <- x  # x IS ENFORCED TO BE A DISSIMILARITY MATRIX
      
      v <- rep(0, k.max)
      
      if (method == "silhouette") {
        loc.mainlab = "Optimal number of clusters from silhouette analysis"
        loc.ylab <- "Average silhouette width"
        for (i in 2:k.max) {
          clust <- FUNcluster(x, i, ...)
          v[i] <-
            factoextra:::.get_ave_sil_width(diss, clust$cluster)
        }
      }
      else if (method == "wss") {
        loc.mainlab = "Optimal number of clusters from within cluster sum of squares"
        
        loc.ylab <- "Total within cluster sum of squares"
        
        for (i in 1:k.max) {
          clust <- FUNcluster(x, i, ...)
          v[i] <- factoextra:::.get_withinSS(diss, clust$cluster)
        }
      }
      
      df <- data.frame(clusters = as.factor(1:k.max), y = v)
      
      p <- ggpubr::ggline(
        df,
        x = "clusters",
        y = "y",
        group = 1,
        color = linecolor,
        ylab = loc.ylab,
        xlab = "Number of clusters",
        main = loc.mainlab
      )

      return(p)
    }
  }

# Clustering ----

# Return a dt with cell IDs and corresponding cluster assignments depending on dendrogram cut (in.k)
# This one works wth dist & hclust pair
# For sparse hierarchical clustering use getDataClSpar
# Arguments:
# in.dend  - dendrogram; usually output from as.dendrogram(hclust(distance_matrix))
# in.k - level at which dendrogram should be cut

getDataCl = function(in.dend, in.k) {
  cat(file = stderr(), 'getDataCl \n')
  
  loc.clAssign = dendextend::cutree(in.dend, in.k, order_clusters_as_data = TRUE, )
  #print(loc.m)
  
  # The result of cutree containes named vector with names being cell id's
  # THIS WON'T WORK with sparse hierarchical clustering because there, the dendrogram doesn't have original id's
  loc.dt.clAssign = as.data.table(loc.clAssign, keep.rownames = T)
  setnames(loc.dt.clAssign, c(COLID, COLCL))
  
  
  #cat('===============\ndataCl:\n')
  #print(loc.dt.cl)
  return(loc.dt.clAssign)
}


# Return a dt with cell IDs and corresponding cluster assignments depending on dendrogram cut (in.k)
# This one works with sparse hierarchical clustering!
# Arguments:
# in.dend  - dendrogram; usually output from as.dendrogram(hclust(distance_matrix))
# in.k - level at which dendrogram should be cut
# in.id - vector of cell id's

getDataClSpar = function(in.dend, in.k, in.id) {
  cat(file = stderr(), 'getDataClSpar \n')
  
  loc.m = dendextend::cutree(in.dend, in.k, order_clusters_as_data = TRUE)
  #print(loc.m)
  
  # The result of cutree containes named vector with names being cell id's
  # THIS WON'T WORK with sparse hierarchical clustering because there, the dendrogram doesn't have original id's
  loc.dt.cl = data.table(id = in.id,
                         cl = loc.m)
  
  #cat('===============\ndataCl:\n')
  #print(loc.dt.cl)
  return(loc.dt.cl)
}



# Returns a table with 2 columns:
# - gr.no - group numbers, e.g. cluster,
# - gr.col - color assignments.
# 
# The number of rows is determined by dendrogram cut, parameter in.k.
# Colours are obtained from the dendrogram, parameter in.dend, using dendextend::get_leaves_branches_col
LOCgetClCol <- function(in.dend, in.k) {
  loc.col_labels <- dendextend::get_leaves_branches_col(in.dend)
  loc.col_labels <- loc.col_labels[order(order.dendrogram(in.dend))]
  
  return(unique(
    data.table(
      gr.no = dendextend::cutree(in.dend, k = in.k, order_clusters_as_data = TRUE),
      gr.col = loc.col_labels
    )
  ))
}


# Custom plotting functions ----


#' Custom ggPlot theme based on theme_bw
#'
#' @param in.font.base
#' @param in.font.axis.text
#' @param in.font.axis.title
#' @param in.font.strip
#' @param in.font.legend
#'
#' @return
#' @export
#'
#' @examples
#'
LOCggplotTheme = function(in.font.base = 12,
                          in.font.axis.text = 12,
                          in.font.axis.title = 12,
                          in.font.strip = 14,
                          in.font.legend = 12) {
  loc.theme =
    theme_bw(base_size = in.font.base, base_family = "Helvetica") +
    theme(
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 0.25),
      axis.text = element_text(size = in.font.axis.text),
      axis.title = element_text(size = in.font.axis.title),
      strip.text = element_text(size = in.font.strip, face = "bold"),
      strip.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = in.font.legend),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(2, "lines")
    )
  
  return(loc.theme)
}

# Build Function to Return Element Text Object
# From: https://stackoverflow.com/a/36979201/1898713
LOCrotatedAxisElementText = function(angle,
                                     position = 'x',
                                     size = 12) {
  angle     = angle[1]
  
  position  = position[1]
  positions = list(
    x = 0,
    y = 90,
    top = 180,
    right = 270
  )
  if (!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]", paste(names(positions), collapse =
                                                           ", ")), call. = FALSE)
  if (!is.numeric(angle))
    stop("'angle' must be numeric", call. = FALSE)
  rads = (-angle - positions[[position]]) * pi / 180
  hjust = round((1 - sin(rads))) / 2
  vjust = round((1 + cos(rads))) / 2
  element_text(
    size = size,
    angle = angle,
    vjust = vjust,
    hjust = hjust
  )
}

#' Return recycled tableau palette
#'
#' Cycle through a tableau palette (e.g. "Color Blind") and return repeated 
#' colours depending on the required number of colours
#'
#' @param inPalName Name of the tableau colour palette, e.g. "Color Blind"
#' @param inNcolors Number of required colours, default 10
#'
#' @return A vector with a requested number of colors
#' @export
#'
#' @examples
#' # The Color Blind palette has only 10 colors; here the 11th will be recycled
#' LOCreturnTableauPalette("Color Blind", 11)
LOCreturnTableauPalette = function(inPalName, inNcolors = 10) {
  
  # get the max N of colours in the palette
  loc.max.col = attr(ggthemes::tableau_color_pal(inPalName), "max_n")
  
  # get all colours in the palette
  loc.col = ggthemes::tableau_color_pal(inPalName)(n = loc.max.col)
  
  # repeat the full palette for the required number of colours
  loc.col = rep(loc.col, ((inNcolors-1) %/% loc.max.col) + 1)
  
  # return only the required number of colurs
  return(loc.col[1:inNcolors])
}


# Plot individual time series
LOCplotTraj = function(dt.arg,
                       # input data table
                       x.arg,
                       # string with column name for x-axis
                       y.arg,
                       # string with column name for y-axis
                       group.arg,
                       # string with column name for grouping time series (typicaly cell ID)
                       facet.arg,
                       # string with column name for facetting
                       facet.ncol.arg = 2,
                       # default number of facet columns
                       facet.color.arg = NULL,
                       # vector with list of colours for adding colours to facet names (currently a horizontal line on top of the facet is drawn)
                       line.col.arg = NULL,
                       # string with column name for colouring time series (typically when individual time series are selected in UI)
                       xlab.arg = NULL,
                       # string with x-axis label
                       ylab.arg = NULL,
                       # string with y-axis label
                       plotlab.arg = NULL,
                       # string with plot label
                       dt.stim.arg = NULL,
                       # plotting additional dataset; typically to indicate stimulations (not fully implemented yet, not tested!)
                       x.stim.arg = c('tstart', 'tend'),
                       # column names in stimulation dt with x and xend parameters
                       y.stim.arg = c('ystart', 'yend'),
                       # column names in stimulation dt with y and yend parameters
                       tfreq.arg = 1,
                       # unused
                       xlim.arg = NULL,
                       # limits of x-axis; for visualisation only, not trimmimng data
                       ylim.arg = NULL,
                       # limits of y-axis; for visualisation only, not trimmimng data
                       stim.bar.width.arg = 0.5,
                       # width of the stimulation line; plotted under time series
                       aux.label1 = NULL,
                       # 1st point label; used for interactive plotting; displayed in the tooltip; typically used to display values of column holding x & y coordinates
                       aux.label2 = NULL,
                       aux.label3 = NULL,
                       stat.arg = c('', 'mean', 'CI', 'SE')) {
  # match arguments for stat plotting
  loc.stat = match.arg(stat.arg, several.ok = TRUE)
  
  
  # aux.label12 are required for plotting XY positions in the tooltip of the interactive (plotly) graph
  p.tmp = ggplot(dt.arg,
                 aes_string(
                   x = x.arg,
                   y = y.arg,
                   group = group.arg,
                   label = group.arg
                 ))
  #,
  #                          label  = aux.label1,
  #                          label2 = aux.label2,
  #                          label3 = aux.label3))
  
  if (is.null(line.col.arg)) {
    p.tmp = p.tmp +
      geom_line(alpha = 0.25,
                size = 0.25)
  }
  else {
    p.tmp = p.tmp +
      geom_line(aes_string(colour = line.col.arg),
                alpha = 0.5,
                size = 0.5) +
      scale_color_manual(
        name = '',
        values = c(
          "FALSE" = rhg_cols[7],
          "TRUE" = rhg_cols[3],
          "SELECTED" = 'green',
          "NOT SEL" = rhg_cols[7]
        )
      )
  }
  
  # this is temporary solution for adding colour according to cluster number
  # use only when plotting traj from clustering!
  # a horizontal line is added at the top of data
  if (!is.null(facet.color.arg)) {
    loc.y.max = max(dt.arg[, c(y.arg), with = FALSE])
    loc.dt.cl = data.table(xx = 1:length(facet.color.arg), yy = loc.y.max)
    setnames(loc.dt.cl, 'xx', facet.arg)
    
    # adjust facet.color.arg to plot
    
    p.tmp = p.tmp +
      geom_hline(
        data = loc.dt.cl,
        colour = facet.color.arg,
        yintercept = loc.y.max,
        size = 4
      ) +
      scale_colour_manual(values = facet.color.arg,
                          name = '')
  }
  
  if ('mean' %in% loc.stat)
    p.tmp = p.tmp +
    stat_summary(
      aes_string(y = y.arg, group = 1),
      fun.y = mean,
      na.rm = T,
      colour = 'red',
      linetype = 'solid',
      size = 1,
      geom = "line",
      group = 1
    )
  
  if ('CI' %in% loc.stat)
    p.tmp = p.tmp +
    stat_summary(
      aes_string(y = y.arg, group = 1),
      fun.data = mean_cl_normal,
      na.rm = T,
      colour = 'red',
      alpha = 0.25,
      geom = "ribbon",
      group = 1
    )
  
  if ('SE' %in% loc.stat)
    p.tmp = p.tmp +
    stat_summary(
      aes_string(y = y.arg, group = 1),
      fun.data = mean_se,
      na.rm = T,
      colour = 'red',
      alpha = 0.25,
      geom = "ribbon",
      group = 1
    )
  
  
  
  p.tmp = p.tmp +
    facet_wrap(as.formula(paste("~", facet.arg)),
               ncol = facet.ncol.arg,
               scales = "free_x")
  
  # plot stimulation bars underneath time series
  # dt.stim.arg is read separately and should contain 4 columns with
  # xy positions of beginning and end of the bar
  if (!is.null(dt.stim.arg)) {
    p.tmp = p.tmp + geom_segment(
      data = dt.stim.arg,
      aes_string(
        x = x.stim.arg[1],
        xend = x.stim.arg[2],
        y = y.stim.arg[1],
        yend = y.stim.arg[2],
        group = 'group'
      ),
      colour = rhg_cols[[3]],
      size = stim.bar.width.arg
    )
  }
  
  p.tmp = p.tmp + coord_cartesian(xlim = xlim.arg, ylim = ylim.arg)
  
  p.tmp = p.tmp +
    xlab(paste0(xlab.arg, "\n")) +
    ylab(paste0("\n", ylab.arg)) +
    ggtitle(plotlab.arg) +
    LOCggplotTheme(
      in.font.base = PLOTFONTBASE,
      in.font.axis.text = PLOTFONTAXISTEXT,
      in.font.axis.title = PLOTFONTAXISTITLE,
      in.font.strip = PLOTFONTFACETSTRIP,
      in.font.legend = PLOTFONTLEGEND
    ) +
    theme(legend.position = "top")
  
  return(p.tmp)
}



#' Plot average time series with CI together in one facet
#'
#' @param dt.arg Data.table with aggregated time series in long format
#' @param x.arg String with column name for x-axis
#' @param y.arg String with column name for y-axis
#' @param group.arg String with column name for grouping time series (e.g. a column with grouping by condition)
#' @param col.arg Colour pallette for individual time series
#' @param dt.stim.arg Data.table with stimulation segments to plot under time series
#' @param x.stim.arg Column names in stimulation dt with x and xend parameters, default c('tstart', 'tend')
#' @param y.stim.arg Column names in stimulation dt with y and yend parameters, default c('ystart', 'yend')
#' @param stim.bar.width.arg Width of the stimulation segment, default 0.5
#' @param xlim.arg Limits of x-axis; for visualisation only, not trimmimng data
#' @param ylim.arg Limits of y-axis; for visualisation only, not trimmimng data
#' @param ribbon.lohi.arg Column names containing lower and upper bound for plotting the ribbon, e.g. for CI; default c('Lower', 'Upper'); set to NULL to avoid plotting the ribbon
#' @param ribbon.fill.arg Color to fill the ribbon, default 'grey50'
#' @param ribbon.alpha.arg Transparency of the ribbon, default 0.5
#' @param xlab.arg X-axis label
#' @param ylab.arg Y-axis label
#' @param plotlab.arg Plot label
#'
#' @return Ggplot object
#' @export
#'
#' @examples
LOCplotTrajRibbon = function(dt.arg,
                             x.arg,
                             y.arg,
                             group.arg = NULL,
                             col.arg = NULL,
                             dt.stim.arg = NULL,
                             x.stim.arg = c('tstart', 'tend'),
                             y.stim.arg = c('ystart', 'yend'),
                             stim.bar.width.arg = 0.5,
                             xlim.arg = NULL,
                             ylim.arg = NULL,
                             ribbon.lohi.arg = c('Lower', 'Upper'),
                             ribbon.fill.arg = 'grey50',
                             ribbon.alpha.arg = 0.5,
                             xlab.arg = NULL,
                             ylab.arg = NULL,
                             plotlab.arg = NULL) {
  
  p.tmp = ggplot(dt.arg, aes_string(x = x.arg, group = group.arg))
  
  if (!is.null(ribbon.lohi.arg))
    p.tmp = p.tmp +
      geom_ribbon(
        aes_string(ymin = ribbon.lohi.arg[1], ymax = ribbon.lohi.arg[2]),
        fill = ribbon.fill.arg,
        alpha = ribbon.alpha.arg
      )
  
  p.tmp = p.tmp + geom_line(aes_string(y = y.arg, colour = group.arg), size = 1.25)
  
  
  # plot stimulation bars underneath time series
  # dt.stim.arg is read separately and should contain 4 columns with
  # xy positions of beginning and end of the bar
  if (!is.null(dt.stim.arg)) {
    p.tmp = p.tmp + geom_segment(
      data = dt.stim.arg,
      aes_string(
        x = x.stim.arg[1],
        xend = x.stim.arg[2],
        y = y.stim.arg[1],
        yend = y.stim.arg[2]
      ),
      colour = rhg_cols[[3]],
      size = stim.bar.width.arg,
      group = 1
    )
  }
  
  p.tmp = p.tmp + coord_cartesian(xlim = xlim.arg, ylim = ylim.arg)
  
  if (is.null(col.arg)) {
    p.tmp = p.tmp +
      scale_color_discrete(name = '')
  } else {
    p.tmp = p.tmp +
      scale_colour_manual(values = col.arg, name = '')
  }
  
  if (!is.null(plotlab.arg))
    p.tmp = p.tmp + ggtitle(plotlab.arg)
  
  p.tmp = p.tmp +
    xlab(xlab.arg) +
    ylab(ylab.arg)
  
  return(p.tmp)
}

# Plot average power spectrum density per facet
LOCplotPSD <- function(dt.arg,
                       # input data table
                       x.arg,
                       # string with column name for x-axis
                       y.arg,
                       # string with column name for y-axis
                       group.arg = NULL,
                       # string with column name for grouping time series (here, it's a column corresponding to grouping by condition)
                       xlab.arg = x.arg,
                       ylab.arg = y.arg,
                       facet.color.arg = NULL) {
  require(ggplot2)
  if (length(setdiff(c(x.arg, y.arg, group.arg), colnames(dt.arg))) > 0) {
    stop(paste("Missing columns in dt.arg: ", setdiff(
      c(x.arg, y.arg, group.arg), colnames(dt.arg)
    )))
  }
  p.tmp <- ggplot(dt.arg, aes_string(x = x.arg, y = y.arg)) +
    geom_line() +
    geom_rug(sides = "b",
             alpha = 1,
             color = "lightblue") +
    facet_wrap(group.arg) +
    labs(x = xlab.arg, y = ylab.arg)
  
  if (!is.null(facet.color.arg)) {
    loc.y.max = max(dt.arg[, c(y.arg), with = FALSE])
    loc.dt.cl = data.table(xx = 1:length(facet.color.arg), yy = loc.y.max)
    setnames(loc.dt.cl, 'xx', group.arg)
    
    # adjust facet.color.arg to plot
    
    p.tmp = p.tmp +
      geom_hline(
        data = loc.dt.cl,
        colour = facet.color.arg,
        yintercept = loc.y.max,
        size = 4
      ) +
      scale_colour_manual(values = facet.color.arg,
                          name = '')
  }
  
  return(p.tmp)
}

#' Plot a scatter plot with an optional linear regression
#'
#' @param dt.arg input of data.table with 2 columns with x and y coordinates
#' @param facet.arg
#' @param facet.ncol.arg
#' @param xlab.arg
#' @param ylab.arg
#' @param plotlab.arg
#' @param alpha.arg
#' @param trend.arg
#' @param ci.arg

LOCggplotScat = function(dt.arg,
                         facet.arg = NULL,
                         facet.ncol.arg = 2,
                         xlab.arg = NULL,
                         ylab.arg = NULL,
                         plotlab.arg = NULL,
                         alpha.arg = 1,
                         trend.arg = T,
                         ci.arg = 0.95) {
  p.tmp = ggplot(dt.arg, aes(x = x, y = y, label = id)) +
    geom_point(alpha = alpha.arg)
  
  if (trend.arg) {
    p.tmp = p.tmp +
      stat_smooth(
        method = "lm",
        fullrange = FALSE,
        level = ci.arg,
        colour = 'blue'
      )
  }
  
  if (!is.null(facet.arg)) {
    p.tmp = p.tmp +
      facet_wrap(as.formula(paste("~", facet.arg)),
                 ncol = facet.ncol.arg)
    
  }
  
  if (!is.null(xlab.arg))
    p.tmp = p.tmp +
      xlab(paste0(xlab.arg, "\n"))
  
  if (!is.null(ylab.arg))
    p.tmp = p.tmp +
      ylab(paste0("\n", ylab.arg))
  
  if (!is.null(plotlab.arg))
    p.tmp = p.tmp +
      ggtitle(paste0(plotlab.arg, "\n"))
  
  p.tmp = p.tmp +
    LOCggplotTheme(
      in.font.base = PLOTFONTBASE,
      in.font.axis.text = PLOTFONTAXISTEXT,
      in.font.axis.title = PLOTFONTAXISTITLE,
      in.font.strip = PLOTFONTFACETSTRIP,
      in.font.legend = PLOTFONTLEGEND
    ) +
    theme(legend.position = "none")
  
  return(p.tmp)
}


LOCplotHeatmap <- function(data.arg,
                           dend.arg,
                           palette.arg,
                           palette.rev.arg = TRUE,
                           dend.show.arg = TRUE,
                           key.show.arg = TRUE,
                           margin.x.arg = 5,
                           margin.y.arg = 20,
                           nacol.arg = 0.5,
                           colCol.arg = NULL,
                           labCol.arg = NULL,
                           font.row.arg = 1,
                           font.col.arg = 1,
                           breaks.arg = NULL,
                           title.arg = 'Clustering') {
  loc.n.colbreaks = 99
  
  if (palette.rev.arg)
    my_palette <-
      rev(colorRampPalette(brewer.pal(9, palette.arg))(n = loc.n.colbreaks))
  else
    my_palette <-
      colorRampPalette(brewer.pal(9, palette.arg))(n = loc.n.colbreaks)
  
  
  col_labels <- get_leaves_branches_col(dend.arg)
  col_labels <- col_labels[order(order.dendrogram(dend.arg))]
  
  if (dend.show.arg) {
    assign("var.tmp.1", dend.arg)
    var.tmp.2 = "row"
  } else {
    assign("var.tmp.1", FALSE)
    var.tmp.2 = "none"
  }
  
  loc.p = heatmap.2(
    data.arg,
    Colv = "NA",
    Rowv = var.tmp.1,
    srtCol = 90,
    dendrogram = var.tmp.2,
    trace = "none",
    key = key.show.arg,
    margins = c(margin.x.arg, margin.y.arg),
    col = my_palette,
    na.col = grey(nacol.arg),
    denscol = "black",
    density.info = "density",
    RowSideColors = col_labels,
    colRow = col_labels,
    colCol = colCol.arg,
    labCol = labCol.arg,
    #      sepcolor = grey(input$inPlotHierGridColor),
    #      colsep = 1:ncol(loc.dm),
    #      rowsep = 1:nrow(loc.dm),
    cexRow = font.row.arg,
    cexCol = font.col.arg,
    main = title.arg,
    symbreaks = FALSE,
    symkey = FALSE,
    breaks = if (is.null(breaks.arg))
      NULL
    else
      seq(breaks.arg[1], breaks.arg[2], length.out = loc.n.colbreaks + 1)
  )
  
  return(loc.p)
}
