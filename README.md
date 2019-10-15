# Time-course analysis web-app

- [Time-course analysis web-app](#time-course-analysis-web-app)
  * [Running the app](#running-the-app)
    + [Runnning instance](#runnning-instance)
    + [Running the app on the server](#running-the-app-on-the-server)
    + [Running the app locally](#running-the-app-locally)
  * [Input file](#input-file)
      - [Long format](#long-format)
      - [Wide format](#wide-format)
  * [Unique track IDs](#unique-track-ids)
  * [Modules and Functionality](#modules-and-functionality)


## Running the app
### Running instance
Access the running instance of the app on [shinyapps.io](https://macdobry.shinyapps.io/tcourse-inspector/ "TimeCourse Inspector")

### Running the app on the server
The app can be deployed on RStudio/Shiny server. Follow instruction [here](https://shiny.rstudio.com/deploy/ "Shiny - Hosting").

### Running the app locally
Alternatively, after downloading the code, the app can run within RStudio. Open `server.R` or `ui.R` file, then click "Run App" button with green triangle in the upper right corner of the window with code open.

Following packages need to be installed in order to run the app locally:

* shiny
* shinyjs
* shinyBS
* shinycssloaders
* data.table
* DT
* ggplot2
* gplots
* plotly
* scales
* grid
* dendextend
* RColorBrewer
* ggthemes
* sparcl
* dtw
* factoextra
* imputeTS
* MASS
* robust
* pracma
* Hmisc

Install packages using `install.packages('name_of_the_package_from_the_list_above')` command in RStudio command line.

```
install.packages(c("shiny", "shinyjs", "shinyBS", "shinycssloaders",
					"data.table", "DT",
					"ggplot2", "gplots", "plotly", "scales", "grid",
					"dendextend", "RColorBrewer", "ggthemes",
					"sparcl", "dtw", "factoextra",
					"imputeTS", "MASS", "robust", "pracma", "Hmisc")) 
```

### Running the app locally with a temporary copy
Running the 2 following lines should get you started immediatly with a temporary copy of the app:
```
library(shiny)
runGitHub("dmattek/shiny-timecourse-inspector")
```

## Input file

The app recognizes CSV (comma-separated values) files where data columns are separated by a comma and floating point numbers use a dot (full-stop). Compressed CSV files in zip or bz2 format can be uploaded directly without decompression. Both long and wide data formats are accepted but we highly recommend using the long format because it allows for multiple groupings and multivariate measurements.

#### Long format
In the long format, the first row should include column headers. The input CSV file should contain at least these three columns:

* Identifier of a time series, i.e. a track label
* Time points
* Time-varying variable

| ID | Time | Meas1 |
|----|------|-------|
| 1  |  1   | 3.3   |
| 1  |  2   | 2.1   |
| 1  |  4   | 4.3   |
|----|------|-------|
| 2  |  1   | 2.8   |
| 2  |  2   | 1.9   |
| 2  |  3   | 1.7   |
| 2  |  4   | 2.2   |

In case of multivariate time series, additional columns with variables can be added in the input. Then, the GUI allows for choosing a single or a combination of two variables to display.

Time series can be grouped by introducing a grouping column:

| Group | ID | Time | Meas1 |
|-------|----|------|-------|
| gr1   | 1  |  1   | 3.3   |
| gr1   | 1  |  2   | 2.1   |
| gr1   | 1  |  4   | 4.3   |
|-------|----|------|-------|
| gr1   | 2  |  1   | 2.8   |
| gr1   | 2  |  2   | 1.9   |
| gr1   | 2  |  3   | 1.7   |
| gr1   | 2  |  4   | 2.2   |
|-------|----|------|-------|
| gr2   | 1  |  1   | 5.1   |
| gr2   | 1  |  2   | 5.4   |
| gr2   | 1  |  3   | 5.3   |

Introduction of grouping allows for the analysis and displaying data per group.

#### Wide format
In wide format, entire univariate time series are stored as rows, with columns treated as time points. The first two columns should contain a grouping and the identifier of time series.

| Group | ID | 0   | 1   | 2   | further time points |
|-------|----|-----|-----|-----|---------------------|
| gr1   | 1  | 3.0 | 3.3 | 3.1 | ...                 |
| gr1   | 2  | 2.0 | 2.1 | 1.9 | ...                 |
| gr2   | 1  | 4.9 | 5.1 | 5.0 | ...                 |
| gr2   | 2  | 5.2 | 5.4 | 5.3 | ...                 |
| gr2   | 3  | 5.5 | 5.3 | 5.6 | ...                 |

We do not recommend this format because of its lack of flexibility. In wide format, only one grouping column and one measurement can be passed at a time, this means any new grouping or measurement analysis requires to create a dedicated file.

## Unique track IDs

For the analysis, track labels need to be unique across the entire dataset. If the track label column is not unique in the uploaded dataset, there's an option in the UI to create a unique track ID. Check the *Create unique track label* box on and choose grouping columns that will be added to the existing non-unique track label. 

In the example above, the `ID` column is not unique across the dataset (ID=1 is repeated in group `gr1` and `gr2`), therefore the unique track label has to consist of columns `Group` and `ID`. The resulting track label will be `gr1_1`, `gr2_1`, etc.


## Modules and Functionality

The app opens with a default window that allows to plot population averages, individual time series, and power spectral density. 

The following features of time series analysis are available in the app:

- Perform simple **math calculations** on an individual variable (inversion 1/X), or on two variables (division, sum, multiplication, subtraction).
- **Trim** the time axis of the data.
- **Normalise** to the average of data points in a selected interval. Time series can be normalised with respect to the entire dataset, a group, or a single time series. The latter would normalise every time course to the mean of its own selected interval.
- **Remove outlier time points** by removing a percentage of data from the top, bottom, or both tails of pooled data points. Gaps in time series due to outlier removal can be then linearly interpolated or tracks can be removed entirely from the set. The UI allows for selecting the size of gaps above which the track is removed.
- **Highlight** individual time series by selecting a unique series identifier.
- Calculate area under individual time series and visualise as a dot-, violin-, or a box-plot. The UI allows for selection of the time series range used for **AUC** calculation.
- Display a dot-, violin-, box-, or a line-plot for selected time points.
- Display a scatter-plot to identify **correlations** between two time points.
- Calculate the **power spectral density (PSD)** using smoothed periodogram or autoregressive fit. Both estimations rely on the R's built-in implementation \texttt{spectrum}. PSD plots can be visualized in the frequency or period domain and independently for each time-series groups. Axis can be transformed with common functions (log, inverse...) to facilitate the identification of spectral patterns.
- Perform **hierarchical and sparse-hierarchical clustering** of individual time series. In these modules, the dendrogram can be cut at a chosen level to help visualising clusters. Addiitonally available are plots with cluster averages, individual times series per cluster, and contribution of time series from different groupings to clusters.
- Perform **cluster validation**. In this module both relative and internal validations are available. Relative validation with a sweep through a range of possible cluster numbers and a report of average silhouette width and within cluster sum of squares. Internal cluster validation, for a fixed number of clusters return 3 visualizations: a dendrogram colored according to the cut, the silhouette plot and a visualization of the clusters on the first 2 principal components. This analysis relies on the implementation in the R package \texttt{factoextra}.
