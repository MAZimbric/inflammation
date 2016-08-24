setwd("D://mzimbric/Desktop/Projects/inflammation")
library(ProjectTemplate)
load.project()
source(file = "src/helpers.R")

#This file will create plots for levels of markers in clinical patients, 
#with patient age as the independent variable and marker level as the 
#dependent variable.

#drop columns 35-42, since none of the patients have more than two measurements of these markers
combined.clinical <- combined.clinical[-35:-42]

#retain only patients with both before and after cultures
combined.clinical <- filter(combined.clinical, retro_ID %in% c("204", "132", "491"))
  
#create column of time relative to first positive NTM culture
combined.clinical <- mutate(combined.clinical, relative_time = X1st_NTM_age - SP_age)

age.marker.plot <- function(dataframe, marker_name) {
  plot.data <- dataframe[c("retro_ID", "disease_status", "relative_time", marker_name)]
  
  if (all(is.na(plot.data[marker_name]))) {
    return()
  }
  
  marker.plot <- ggplot(plot.data, aes_string(x= "relative_time", y = marker_name, color = "retro_ID")) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name = "Time relative to first positive NTM culture") +
    scale_y_continuous(name = paste("log10 of", marker_name, "pg/mL"))
    
  
  return(marker.plot)
}

plot.all.age.marker <- function(clinical){
  markers <- names(clinical[10:ncol(clinical)])
  for (i in seq_along(markers)) {
      marker.plot <- age.marker.plot(combined.clinical, markers[i])
      ggsave(marker.plot, filename=paste("figures/clinical/preliminary-zeroed-", markers[i], "-plot.png",sep=""))
  }
}
  

plot.all.age.marker(combined.clinical)