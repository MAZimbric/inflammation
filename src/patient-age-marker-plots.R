setwd("D://mzimbric/Desktop/Projects/inflammation")
library(ProjectTemplate)
load.project()
source(file = "src/helpers.R")

#This file will create plots for levels of markers in clinical patients, 
#with patient age as the independent variable and marker level as the 
#dependent variable.

#drop columns 35-42, since none of the patients have more than two measurements of these markers
combined.clinical <- combined.clinical[-35:-42]

age.marker.plot <- function(dataframe, marker_name) {
  plot.data <- dataframe[c("retro_ID", "disease_status", "sample_timing", "SP_age", "X1st_NTM_age", marker_name)]
  
  if (all(is.na(plot.data[marker_name]))) {
    return()
  }
  
  #divider <- plot.data$X1st_NTM_age[1]
  
  marker.plot <- ggplot(plot.data, aes_string(x= "SP_age", y = markers[i])) +
    geom_point() +
    geom_line() +
   #geom_vline(xintercept = divider, color = "red", linetype = "dashed") +
    scale_x_continuous(name = "Patient age") +
    scale_y_continuous(name = paste("log10 of", markers[i], "pg/mL"))+
    facet_grid(retro_ID ~disease_status)
    
  
  return(marker.plot)
}

plot.clinical <- function(clinical){
  markers <- names(clinical[10:ncol(clinical)])
  patients <- levels(as.factor(combined.clinical$retro_ID))
  for (i in seq_along(markers)) {
    for (j in patients){
      #subset by patient id
      plot.data <- clinical[clinical$retro_ID == j,]
      
      #bug in this loop, I think, when the column is all NA
      if (all(is.na(clinical[markers[i]]))) {
        next
      }
      else {
        divider <- plot.data$X1st_NTM_age[1] 
      
        patient.plot <- ggplot(plot.data, aes_string(x= "SP_age", y = markers[i])) +
          geom_point() +
          geom_line() +
          geom_vline(xintercept = divider, color = "red", linetype = "dashed") +
          scale_x_continuous(name = "Patient age") +
          scale_y_continuous(name = paste("log10 of", markers[i], "pg/mL"))
          
        ggsave(patient.plot, filename=paste("figures/clinical/preliminary-plot-", markers[i], "patient-", j, ".png",sep=""))
      }
    }  
  }
}

plot.clinical(combined.clinical)

