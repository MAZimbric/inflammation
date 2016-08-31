setwd("D://mzimbric/Desktop/Projects/inflammation")
library(ProjectTemplate)
load.project()
source(file = "src/helpers.R")

#This script generates plots for each inflammatory marker
#with number of days stored as the independent variable and 
#marker concentration as the dependent variable
#Initially we'll make Line plots with each of the 4 sample sources.

process.storage.data <- function(data) {
  
  #calculate means and standard deviations and number of replicates subsetted by samplesource and storage_days
  mn <- aggregate.data.frame(data, list(data$storage_days, data$samplesource), mean)
  mn <- agg.out(mn)
  mnr <- rename(mn, mn = marker_level)

  sd <- aggregate.data.frame(data, list(data$storage_days, data$samplesource), sd)
  sd <- agg.out(sd)
  sd <- rename(sd, sd = marker_level)
  
  reps <- aggregate.data.frame(data, list(data$storage_days, data$samplesource), length)
  reps <- agg.out(reps)
  reps <- rename(reps, observations = marker_level)
  
  
  #merge mean and standard deviation data frames
  summary <- left_join(mn,sd)
  summary <- left_join(summary, reps)
  
  #calculate standard error
  summary$sem <- summary$sd/sqrt(summary$observations)
  
  return(summary)
}

#creates a storage plot for an individual marker
plot.storage.marker <- function(data, marker, threshold, y.value) {
  
  plot.data <- process.storage.data(data = data, marker = marker)
  
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




