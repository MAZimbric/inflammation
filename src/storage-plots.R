setwd("D://mzimbric/Desktop/Projects/inflammation")
library(ProjectTemplate)
load.project()
source(file = "src/helpers.R")

#This script generates plots for each inflammatory marker
#with number of days stored as the independent variable and 
#marker concentration as the dependent variable

#creates a storage plot for an individual marker. Pass a processed storage data frame 
plot.storage.marker <- function(data, marker, threshold, y.value) {
  
  data <- filter(data, marker_name == marker)
  
  marker.plot <- ggplot(plot.data, aes(x=storage_days, y=mn, colour=samplesource)) + 
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=mn-sem, ymax=mn+sem), width=.1) +
    geom_hline(yintercept = threshold) +
    scale_x_continuous(name = "Days Stored at 4 C", breaks = c(0,3,7,14,28)) + 
    scale_y_continuous(name = paste("log10 of", marker, "(pg/mL)"),
                       limits = c(-0.5, y.value),
                       breaks = seq(0,y.max,by = 1)) +
    scale_colour_hue("Sample", labels = c("A", "B", "C", "D", "E"))
  
  return(marker.plot)
}


#function wrapper for generating plots for all markers
plot.marker.all <- function(x, thresh, y.value){
  markers <- levels(marker_name)
  for (i in seq_along(markers)){
    #define the threshold
    threshold <- thresh[markers[i]]
    
    marker.plot <- plot.storage.marker(marker.summary, markers[i], threshold, y.value)
    ggsave(marker.plot,filename=paste("figures/storage/storage-plot-", markers[i],".png",sep=""))
    }
}

#function for creating faceted plot
faceted.storage <- function(data, marker_vector, y.value){
  plot.data <- filter(data, marker_name %in% marker_vector)
  
  plot <- ggplot(plot.data, aes(x=storage_days, y=mn, colour=samplesource)) + 
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=mn-sem, ymax=mn+sem), width=.1) +
    scale_x_continuous(name = "Days Stored at 4 C", breaks = c(0,3,7,14,28)) + 
    scale_y_continuous(name = paste("log10 of cytokine level (pg/mL)"),
                       limits = c(-0.5, y.value),
                       breaks = seq(0,y.max,by = 1)) +
    scale_colour_hue("Sample", labels = c("A", "B", "C", "D", "E"))+
    facet_wrap(~marker_name)
}


