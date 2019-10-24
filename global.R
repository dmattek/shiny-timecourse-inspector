# Check if all required packages are installed, if not, attempt to install the missing ones
required_packages = c(
  "shiny",
  "shinyjs",
  "shinyBS",
  "shinycssloaders",
  "R.utils",
  "data.table",
  "DT",
  "ggplot2",
  "gplots",
  "plotly",
  "scales",
  "grid",
  "dendextend",
  "RColorBrewer",
  "ggthemes",
  "sparcl",
  "dtw",
  "factoextra",
  "imputeTS",
  "MASS",
  "robust",
  "pracma",
  "Hmisc"
)
missing_packages =
  required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages)) {
  cat(paste(
    "Missing packages:",
    paste(missing_packages, collapse = ";"),
    "\nAttempting to install them."
  ))
  install.packages(missing_packages)
}

# Load modules
source('modules/auxfunc.R')
source('modules/selOutliers.R')
source('modules/downPlot.R')
source('modules/downCSV.R')
source('modules/dispStats.R')
source('modules/dispTrackStats.R')
source('modules/trajPlot.R')
source('modules/trajRibbonPlot.R')
source('modules/trajPsdPlot.R')
source('modules/aucPlot.R')
source('modules/distPlot.R')
source('modules/clDistPlot.R')
source('modules/tabScatter.R')
source('modules/tabDist.R')
source('modules/tabAUC.R')
source('modules/tabClValid.R')
source('modules/tabClHier.R')
source('modules/tabClHierWdist.R')
source('modules/tabClHierSpar.R')