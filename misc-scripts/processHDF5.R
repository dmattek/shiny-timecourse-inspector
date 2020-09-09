#' Process HDF5 file from CellProfiler and save as CSV file
#' Tuned for time-lapse experiments
#' Two CSV files are saved, a full output and a reduced output
#' Extracts measurements corresponding to a simple object (e.g. a nucleus)
#' and merges it with image metadata (e.g. stored in Image)


library(rhdf5)


## Definitions
# directory location of the h5 file
s.dircore = '~/Projects/Olivier/Paolo/2017-12-21_MCF10Amutants_H2B-miRFP_ERKKTR-mTurquiose_20xAir_T5min_Stim30min-1ngmlEGF_30starving/cp.out5/'

# file name of the input (without h5 extension)
s.fname.in = 'meas4'

# file name for the the output
s.fname.out = s.fname.in

# name of the item inside the h5 file with Image measurements and metadata
s.fimage = 'Image'

# name of the item inside the h5 file with object measurements
s.fobj = 'objNuclei'

# columns to keep from Image section
s.colnames = c('ImageNumber',
               'Metadata_Site',
               'Metadata_Z',
               'Metadata_Well')


# list of all items inside h5 file
h5items = as.data.table(h5ls(paste0(s.dircore, s.fname.in, ".h5")))
s.item = paste0('Measurements/', h5items[group == '/Measurements'][['name']], '/')

## READ
# Read the file and store object-related items
E = h5read(paste0(s.dircore, s.fname.in, ".h5"), 
           paste0(s.item, s.fobj))
E.colnames = names(E)

# every item has data and index sub-items
# data is 1D vector
# index is a matrix 3 by the number of frames in the time lapse experiment
# index item holds indices of data vector for each frame
# The function below extracts data from data vector according to indices for every frame
l.E = lapply(E.colnames, function(x) {
  loc.indx.dt = as.data.table(t(E[[x]]$index))
  setnames(loc.indx.dt, c('ii', 'start', 'end'))
  loc.indx.dt[, start := start + 1]
  
  loc.data = as.vector(E[[x]]$data)
  
  loc.dt = loc.indx.dt[, .(tmpname = loc.data[seq(start, end, 1)]), by = ii]
  loc.dt = loc.dt[, 'tmpname', with = FALSE]
  setnames(loc.dt, 'tmpname', x)
  return(loc.dt)})

dt.E = do.call('cbind', l.E)


# Read the file and store image-related items
G = h5read(paste0(s.dircore, s.fname.in, ".h5"), 
           paste0(s.item, s.fimage))
G.colnames = names(G)

G.nrows = length(G[[1]]$data)

l.G = lapply(G.colnames, function(x) {
  loc.dt = data.table(tmpname = G[[x]]$data[1:G.nrows])
  setnames(loc.dt, 'tmpname', x)
  return(loc.dt)})

dt.G = do.call('cbind', l.G)

dt.G = dt.G[, (s.colnames), with = FALSE]

dt = merge(dt.E, dt.G)

# write csv with full output (can be a very large file)
write.csv(x = dt, 
          file = paste0(s.dircore, s.fname.out, ".csv"), 
          row.names = F, quote = FALSE)


# write csv with only selected columns
cnames = c(s.colnames,
           'TrackObjects_Label',
           'ObjectNumber',
           'Location_Center_X',
           'Location_Center_Y',
           'Intensity_MeanIntensity_imERK')

write.csv(x = dt[, (cnames), with = FALSE], 
          file = paste0(s.dircore, s.fname.out, "_short.csv"), 
          row.names = F, quote = FALSE)

