# Look for objects with duplicated track IDs
# Rationale:
# In some cases, the output from CP LAP tracking assigns the same track label 
# to different objects. This typically happens when cells split either because of 
# actual mitosis or because of the (wrong) segmentation.

require(data.table)


## DEFS
# Input file name
s.fname = '/Volumes/imaging.data/Paolo/MCF10A_TimeLapse/2017-11-20_MCF10A_WT_H2BmiRFP_ERKKTRmTurq_Geminin-mCherry_GrowthMedium_T5min_40xOil_Spinning/cp.out14/output/out_0001/objNuclei.csv'

# Metadata with track label from CP tracking module
s.track = 'TrackObjects_Label'

# Metadata with FOV
s.fov = 'Metadata_Site'

# Column name with unique track ID created from s.track and s.fov
s.track.uni = 'TrackObjects_Label_Uni'
  
# Metadata with time frame number (can also be Metadata_Frame)
s.time = 'Metadata_T'

# Metadata with Image number
s.imnum = 'ImageNumber'


## Load file
dt1 = fread(s.fname)

# remove duplicated columns
# happens when CP adds MEtadata to object files
s.head = names(dt1)
dt1 = dt1[, s.head[!duplicated(s.head)], with = FALSE]

# remove unnecessary columns
s.cols = c('Metadata_C',
           'Metadata_Channel',
           'Metadata_ChannelName',
           'Metadata_ColorFormat',
           'Metadata_FileLocation',
           'Metadata_Plate',
           'Metadata_SizeC',
           'Metadata_SizeT',
           'Metadata_SizeX',
           'Metadata_SizeY',
           'Metadata_SizeZ',
           'Metadata_Z')

dt1[, (s.cols) := NULL]

# Create unique track ID
dt1[, (s.track.uni) := paste(get(s.fov), get(s.track), sep = '_')]

## Number of track pieces
cat('Number of tracks: ', length(unique(dt1[[s.track.uni]])), '\n')

## Average track length
cat('Average track length: ', dt1[, .(trackLength = .N), by = s.track.uni][, mean(trackLength)])


## Track IDs assigned to multiple objects in a frame
# Look whether there were more objects with the same track ID in the frame
# Such track IDs will have TRUE assigned in 'dup' column
# Keep only s.track column with dup=TRUE
dt.duptracks = dt1[, .(dup = (sum(duplicated(get(s.imnum))) > 0)), by = s.track.uni][dup == TRUE, s.track.uni, with = FALSE]

# Display original data with identified tracks
setkeyv(dt1, s.track.uni)
dt.res = dt1[dt.duptracks]

cat('Track IDs with multiple objects assigned in a frame\n')
print(dt.res)


