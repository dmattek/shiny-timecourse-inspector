# Time-course analysis web-app

## Running the app from the server
The app can be accessed here:
http://pertzlab.unibe.ch:3838/shiny-timecourse-inspector/ (UniBe VPN only!)

## Running the app locally
Alternatively, after downloading the code, the app can be run within RStudio. Open `server.R` or `ui.R` file, then click "Run App" button with green triangle in the upper right corner of the window with code open.

Following packages need to be installed in order to run the app locally:

* shiny
* shinyjs
* data.table
* ggplot2
* gplots
* scales
* plotly
* d3heatmap
* dendextend
* RColorBrewer
* sparcl
* imputeTS
* dtw
* DT
* MASS
* robust
* pracma
* Hmisc

Install packages using `install.packages('name_of_the_package_from_the_list_above')` command in RStudio command line.

```
install.packages(c("shiny", "shinyjs", 
					"data.table", "DT",
					"ggplot2", "gplots", "plotly", "d3heatmap", "scales",
					"dendextend", "dendextend", "RColorBrewer",
					"sparcl", "dtw",
					"imputeTS",
					"MASS", "robust", "pracma", "Hmisc")) 
```

## Input file
The app recognises CSV (comma-separated values) files: data columns separated by a comma, floating point numbers using a dot (full-stop).

The data file has to be in a so called long format, where individual time-courses (tracks) are arranged one after another. Note a wide-format where individual tracks are arranged in neighbouring columns is NOT supported!

Sample few lines of the input file:
```
"Metadata_Series", "TrackObjects_Label", "Intensity_MeanIntensity_Ratio", "RealTime", "Stim_All_Ch", "Stim_All_S"
0, 2, 1149.00311105531, 0, "Ch01: FGF 250 ng/ml 60min pulse", "S00: FGF 250 ng/ml 60min pulse"
0, 3, 1160.43905280656, 0, "Ch01: FGF 250 ng/ml 60min pulse", "S00: FGF 250 ng/ml 60min pulse"
0, 4, 1303.06046656558, 0, "Ch01: FGF 250 ng/ml 60min pulse", "S00: FGF 250 ng/ml 60min pulse"
```

The first row should include column headers. Necessary columns include:

* Unique number of the field of view (FOV), here "Metadata_Series"
* Unique identifier of track ID within the FOV, here "TrackObjects_Label"
* Time point, here "RealTime"
* Measurement column (can be many columns), here "Intensity_MeanIntensity_Ratio"

Additionally, columns with condition names can be included. In the example above, "Stim_All_S" identifies condition per FOV, and "Stim_All_Ch" relates to a condition name within the well. These two columns are useful to group and plot time-courses in separate facets per FOV or per well.
