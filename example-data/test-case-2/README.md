# Test case dataset

This dataset contains 339 time series with 101 time points each. The set comes from a microfluidic time-lapse microscopy experiment. In this experiment, PC-12 cells were treated with 4 concentrations of epidermal growth (EGF): 0.25, 2.5, 25, and 250 ng/ml. The treatment was applied at 43' in a sustained manner. The readout is the measurement of ERK activity using a FRET biosensor.

The set comes from a publication Blum *et al.* (2019) *Temporal perturbation of Erk dynamics reveals network architecture of FGF2/MAPK signaling*, **Mol Sys Biol** 15:e8947, [doi](https://doi.org/10.15252/msb.20198947 "doi").

Files in this set:

**`sustained_EGF_ekar.csv.gz`**

Main dataset in the long format has following columns:

- `treat` with a grouping according to 4 concentrations of EGF,
- `fov` with an additional grouping according to the field of view during experiment acquisition,
- `trackid` with time series identifiers (unique only within the FOV),
- `frame` with frame numbers,
- `realtime` with time points in minutes,
- `meas` with ERK activity from the measurement of the FRET ratio.

Note, the `trackid` column contains identifiers that are unique only within a single field of view. Thus, in order to create data-wide unique identifiers, combine the `fov` and `trackid` columns.

The compressed `csv.gz` file can be uploaded directly into Time-Course Inspector (TCI).


**`sustained_EGF_badTraj.csv`**

A single-column CSV file with identifiers of outlier trajectories. This file can be uploaded into TCI, and listed time series will be removed from further analysis. The IDs need to be in the same format as in the app. If a unique identifier was created within the app (e.g. by combining `fov` and `id` columns), then the IDs in this file need to have the same format.

**`tCoursesProcessed.csv.gz`**

Single-cell time series after pre-processing in the app. The measurement is trimmed from 0'-200' down to 20'-150', normalised per time series to the 20'-40' range, and NA's and missing rows are interpolated.

The columns include:

- `group` with grouping according to treatment with 4 concentrations of EGF,
- `id` with a data-wide unique time series identifier,
- `time` with time points in minutes,
- `meas` with ERK activity from the measurement of the FRET ratio.

**`sustained_EGF_y1100.csv`** and **`sustained_EGF_y0-97.csv`**

Files with coordinates of segments that correspond to the stimulation time. When loaded into TCI, they are shown as segments under the time series. Each row contains x and y coordinates of the start and end of segments (4 columns: `tstart`, `tend`, `ystart`, `yend`), `group` column used for grouping of the main dataset in the app, a dummy `id` column.

The two files contain segments that can be plotted at different y levels. For example, the `*y1100.csv` file can be uploaded when plotting raw data from the `sustained_EGF_ekar.csv.gz`. The stimulation segments span the same duration as the experiment, i.e. up to 200', and will be positioned at `y=1100`. The `*y0-97.csv` file spans a shorter period, up to 150', which corresponds to the time range in the trimmed and normalised file `tCoursesProcessed.csv.gz`. The segment will be positioned at `y=0.97`.
