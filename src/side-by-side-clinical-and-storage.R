setwd("D://mzimbric/Desktop/Projects/inflammation")
library(ProjectTemplate)
load.project()
source(file = "src/helpers.R")
source(file = "src/patient-age-marker-plots.R")
source(file = "src/storage-plots.R")

#This script produces a side-by-side storage and clinical plot for a single marker
#The inputs should be the storage.data and combined.clinical data frames produced
#during data munging, the cytokine name as a string, the vector of thresholds and the y.max
sidebyside.plot <- function(storage, clinical, marker_name, thresholds, y.value) {
  require(cowplot)
  marker_threshold <- thresholds[marker_name]
  storage.plot <- plot.storage.marker(storage, marker_name, marker_threshold, y.value)
  patient.plot <- age.marker.plot(clinical, marker_name, marker_threshold, y.value)
  side.by.side <- plot_grid(storage.plot, patient.plot, ncol = 1, align = 'h', labels = c('A', 'B'))
  return(side.by.side)
}

