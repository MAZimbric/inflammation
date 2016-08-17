setwd("D://mzimbric/Desktop/Projects/inflammation")
library(ProjectTemplate)
load.project()
source(file = "src/helpers.R")

#This file will create plots for levels of markers in clinical patients, 
#with patient age as the independent variable and marker level as the 
#dependent variable.

plot.clinical <- function(clinical) {
  markers <- names(clinical[10:ncol(clinical)])
  patients <- levels(as.factor(combined.clinical$retro_ID))
  for (i in seq_along(markers)) {
    for (j in patients){
      #subset by patient id
      plot.data <- subset(clinical, clinical$retro_id == j)
      
      patient.plot <- ggplot(plot.data, aes(x=SP_age, y = markers[i])) +
        geom_point() +
        geom_line() +
        geom_vline(X1st_NTM_age[1])
    }
  }
}

plot.clinical(combined.clinical)
