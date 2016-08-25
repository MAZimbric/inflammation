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
combined.clinical <- combined.clinical[c(1:9, 35, 10:34)]

#stripping out unneeded data
combined.clinical <- combined.clinical[c(3,6,10,11:35)]

#melt data into long form
long.combined.clinical <- melt(combined.clinical, id = c(1:3), na.rm = TRUE)
long.combined.clinical <- rename(long.combined.clinical, marker = variable, marker_value = value)

age.marker.plot <- function(dataframe, marker_name) {
  plot.data <- filter(dataframe, marker == marker_name)
  
  marker.plot <- ggplot(plot.data, aes_string(x= "relative_time", y = "marker_value", color = "retro_ID")) +
    geom_point() +
    geom_line(aes(linetype = disease_status)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(name = "Time (years) relative to first positive NTM culture") +
    scale_y_continuous(name = paste("log10 of", marker_name, "pg/mL"))+
    scale_linetype_discrete(name = "Disease Status")+
    scale_color_discrete(name = "Patient")
  
  return(marker.plot)
}

#This one doesn't work. Try rewriting with d_ply
plot.all.age.marker <- function(clinical){
  markers <- levels(clinical$marker)
  for (i in seq_along(markers)) {
      marker.plot <- age.marker.plot(combined.clinical, markers[i])
      ggsave(marker.plot, filename=paste("figures/clinical/preliminary-zeroed-", markers[i], "-plot.png",sep=""))
  }
}

#This function will take in the clinical data and a vector of marker names and produce a faceted plot 
plot.faceted.age.marker <- function(clinical.subset){
}
  
print(age.marker.plot(long.combined.clinical, "IL.1b"))
