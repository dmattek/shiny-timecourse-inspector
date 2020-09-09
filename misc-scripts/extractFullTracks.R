require(data.table)

# Returns dt with trajectories that last from the first till last frame
# and include at most in.max.break interruptions
myTrajExtr = function(in.dt,
                      in.max.break = 1,
                      in.met.series = 'Metadata_Series',
                      in.met.t = 'Metadata_T',
                      in.met.tracklabel = 'TrackObjects_Label',
                      in.aggr.cols = NULL) {
  loc.dt = copy(in.dt)
  
  # TrackObjects assigns the same label for different cells from IdentifyPrimaryObjects
  # The following aggregation makes sure there's a unique TrackObjects_Label
  # for every Site at every time point: it takes the mean intensity of duplicated labels.
  # Make sure it makes sense!!!
  # Roughly 10% of objects affected
  
  if (!is.null(in.aggr.cols)) {
    loc.dt = loc.dt[, lapply(.SD, function(x) mean(x, na.rm = TRUE)),
                    by = c(in.met.series, in.met.t, in.met.tracklabel), .SDcols = in.aggr.cols]
  }
  
  # cells from different sites have the same TrackObjects_Label
  # make it unique accross the experiment and pad numbers with zeros
  
  loc.types = lapply(loc.dt, class)
  if(loc.types[[in.met.series]] == 'numeric')
    loc.dt[, TrackObjects_Label_uni := paste(sprintf("%02d", get(in.met.series)), sprintf("%04d", get(in.met.tracklabel)), sep = "_")] else
      loc.dt[, TrackObjects_Label_uni := paste(sprintf("%s", get(in.met.series)), sprintf("%04d", get(in.met.tracklabel)), sep = "_")] 
  
  ####
  ## Allow for single-breaks in the middle of the trajectory
  ## Single-breaks usually result from a single frame with lost focus
  ## no interpolation
  
  # build vector with unique timepoints for the entire experiment
  loc.t.range = unique(loc.dt[, c(in.met.t), with = FALSE])
  
  # Select cells with number of timepoints equal to
  # the range nrow(loc.t.range)  AND
  # with first and last frame at the beginning and end of the movie, respectively.
  # If no aggregation columns (in.aggr.cols) are provided,
  # tracks with forks will be omitted here because the number of timepoints exceeds nrow(loc.t.range)
  loc.dt.tmp1 = loc.dt[, .(
    Ntpt = .N,
    T.start = first(get(in.met.t)),
    T.end = last(get(in.met.t))
  ),
  by = TrackObjects_Label_uni][Ntpt <=  nrow(loc.t.range) &
                                 T.start == min(loc.t.range) &
                                 T.end == max(loc.t.range)]
  
  # With cells selected above,
  # select tcourses with at most 1 break.
  ## Single-breaks usually result from a single frame with lost focus
  ## no interpolation
  # Calculation based on differences between consecutive Metadata_T.
  # Metadata_T increases by 1, thus a single-frame break would result in a difference of 2.
  
  # select cells recorded at the beginning and end of the experiment
  loc.dt.tmp2 = loc.dt[TrackObjects_Label_uni %in% loc.dt.tmp1$TrackObjects_Label_uni]
  
  # set order
  setkeyv(loc.dt.tmp2, c(in.met.series, in.met.tracklabel, in.met.t))
  
  # calculate difference in consecutive time
  loc.dt.tmp2[, Metadata_T.diff := c(-1, diff(get(in.met.t))), by = TrackObjects_Label_uni]
  
  # identify cells with at least one break longer than 1 frame
  # column Nframes stores the number of instances with break longer than 1 frame
  loc.dt.tmp2 = loc.dt.tmp2[Metadata_T.diff > 1 + in.max.break, .(Nframes = .N), by = TrackObjects_Label_uni]
  
  # Selected trajectories with frames at 1st and last time points AND with at most 1-frame break
  loc.out = loc.dt[TrackObjects_Label_uni %in% setdiff(loc.dt.tmp1$TrackObjects_Label_uni,
                                                       loc.dt.tmp2$TrackObjects_Label_uni)]
  return(loc.out)
}


## Definitions
# directory location of the csv file
s.dircore = '~/Projects/Olivier/Paolo/2017-12-21_MCF10Amutants_H2B-miRFP_ERKKTR-mTurquiose_20xAir_T5min_Stim30min-1ngmlEGF_30starving/cp.out5/'

# file name of the input (without h5 extension)
s.fname.in = 'meas4_short'

# relevant columns
# time
s.met.t = 'Metadata_Z'

# FOV
s.met.ser = 'Metadata_Site'

# TrackLabel
s.met.track = 'TrackObjects_Label'

dt = fread(paste0(s.dircore, s.fname.in, ".csv"))

dt.sel = myTrajExtr(dt,
           in.max.break = 0,
           in.met.series = s.met.ser, 
           in.met.t = s.met.t, 
           in.met.tracklabel = s.met.track)

# number of full tracks (no break)
length(unique(dt.sel$TrackObjects_Label_uni))
