# Test case dataset

This dataset contains 234 time series with 121 time points each. The set comes from a microfluidic time-lapse microscopy experiment. In this experiment, PC-12 cells were stimulated with 3 dosages of fibroblast growth factor 2 (FGF2): 2.5, 25, and 250 ng/ml. The stimulation was performed with 3' pulses of FGF2 at 20' intervals. The readout is the measurement of ERK activity using a FRET biosensor.

The set comes from a publication Blum *et al.* (2019) *Temporal perturbation of Erk dynamics reveals network architecture of FGF2/MAPK signaling*, **Mol Sys Biol** 15:e8947, [doi](https://doi.org/10.15252/msb.20198947 "doi").

Files in this set:

**`mp3-20_FGF_ekar.csv.gz`**

Main dataset in the long format with following columns:

- `intensity_ekar` - measurement of the biosensor,
- `realtime` - time points in minutes,
- `id` - identifiers of time series (unique inly within the FOV),
- `group` - grouping according to treatment with 3 concentrations of FGF2,
- `fov` - additional grouping according to the field of view during experiment acquisition.

Note, the `id` column contains identifiers that are unique only within a single field of view. Thus, in order to create data-wide unique identifiers, you need to combine the `fov` and `id` columns.

The compressed `csv.gz` file can be uploaded directly into Time-Course Inspector (TCI).


**`mp3-20_FGF_badTraj.csv`**

A single-column CSV file with identifiers of outlier trajectories. This file can be uploaded into TCI, and listed time series will be removed from further analysis. The IDs need to be in the same format as in the app. If a unique identifier was created within the app (e.g. by combining `fov` and `id` columns), then the IDs in this file need to have the same format.


**`mp3-20_FGF_pulse_y1100`** and **`mp3-20_FGF_pulse_y0-97.csv`**

Files with coordinates of segments that correspond to the stimulation time. When loaded into TCI, they are shown as segments under the time series. Each row contains x and y coordinates of the start and end of segments (4 columns: `tstart`, `tend`, `ystart`, `yend`), `group` column used for grouping of the main dataset in the app, a dummy `id` column.

The two files contain segments that can be plotted at different y levels. For example, the `*y1100.csv` file can be uploaded when plotting raw data. The stimulation segments will be positioned at `y=1100`. The `*y0-97.csv` contains segments that will be positioned at `y=0.97`.
