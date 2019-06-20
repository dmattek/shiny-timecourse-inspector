# Test case dataset

This dataset contains around 234 time-series with 121 time points each. The set comes from a microfluidic time-lapse microscopy experiment. In this experiment, PC-12 cells were stimulated with 3 dosages of fibroblast growth factor 2 (FGF2): 2.5 ng/ml, 25 ng/ml, and 250 ng/ml. The stimulation was performed with 3' pulses of FGF2 at 20' intervals. The readout is the measurement of ERK activity using a FRET biosensor.

The set comes from a publication Blum *et al.* (2019) *Temporal perturbation of Erk dynamics reveals network architecture of FGF2-MAPK signaling* (**preprint**), available in [bioRxiv](https://www.biorxiv.org/content/10.1101/629287v1"bioRxiv")].

Files in this set:

**`mp3-20_FGF_ekar.csv.gz`** 

Main dataset in the long format with following columns:

- `intensity_ekar` - measurement of the biosensor
- `realtime` - time points in minutes
- `id` - identifiers of time-series
- `group` - grouping according to treatment with 3 concentrations of FGF2
- `fov` - additional grouping according to the field of view during experiment acquisition. 

Note, the `id` column contains identifiers that are unique only within a single field of view. Thus, in order to create data-wide unique identifiers, you need to combine the `fov` and `id` columns.

The `csv.gz` file can be uploaded directly into Time Course Inspector (TCI).


**`mp3-20_FGF_badTraj.csv`**

A single-column csv file with identifiers of outlier trajectories. This file can be uploaded into TCI, and listed time series will be removed from further analysis. The IDs need to be in the same format as in the app. If a unique identifier was created within the app (e.g. by combining ''fov'' and ''id'' columns), then the IDs in this file need to be of the same form.


**`mp3-20_FGF_pulsey_y0-97.csv`** and **`mp3-20_FGF_pulse_y1100`**

Files with coordinates of segments to plot under time-series. Each row should contain x and y coordinates of the start and end of segments (4 columns: `tstart`, `tend`, `ystart`, `yend`), `group` column used for grouping of the main dataset in the app, a dummy `id` column.