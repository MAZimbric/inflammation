#setwd("D://mzimbric/Desktop/Projects/inflammation")
# library(ProjectTemplate)
# load.project()
# source(file = "src/helpers.R")

#This script generates plots for each inflammatory marker
#with number of days stored as the independent variable and 
#marker concentration as the dependent variable
#Initially we'll make Line plots with each of the 4 sample sources.

process.storage.data <- function(data, marker) {
  #subset data
  data.marker <- data[c('samplesource','replicate','storage_days', marker)]
  #remove NA so that aggregate doesn't throw an error
  data.marker <- na.omit(data.marker)
  
  #calculate means and standard deviations and number of replicates subsetted by samplesource and storage_days
  mn.marker <- aggregate.data.frame(data.marker, list(data.marker$storage_days, data.marker$samplesource), na.mean)
  mn.marker <- agg.out(mn.marker)
  names(mn.marker)[names(mn.marker) == marker] <- "mn"

  sd.marker <- aggregate.data.frame(data.marker, list(data.marker$storage_days, data.marker$samplesource), sd)
  sd.marker <- agg.out(sd.marker)
  names(sd.marker)[names(sd.marker)== marker] <- "sd"
  
  reps <- aggregate.data.frame(data.marker, list(data.marker$storage_days, data.marker$samplesource), length)
  reps <- agg.out(reps)
  names(reps)[names(reps)== marker] <- "observations"
  
  
  #merge mean and standard deviation data frames
  marker.summary <- left_join(mn.marker,sd.marker)
  marker.summary <- left_join(marker.summary, reps)
  
  #calculate standard error
  marker.summary$sem <- marker.summary$sd/sqrt(marker.summary$observations)
  
  return(marker.summary)
}

#creates a storage plot for an individual marker
plot.storage.marker <- function(data, marker, threshold, y.value) {
  if (ncol(data) > 6) {
    plot.data <- process.storage.data(data = data, marker = marker)
  }
  
  else {
    plot.data <- data
  }
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
  markers <- names(x[4:ncol(x)])
  for (i in seq_along(markers)){
    marker.summary <- process.storage.data(x, markers[i])
    #define the threshold
    threshold <- thresh[markers[i]]
    
    marker.plot <- plot.storage.marker(marker.summary, markers[i], threshold, y.value)
    ggsave(marker.plot,filename=paste("figures/storage/storage-plot-", markers[i],".png",sep=""))
    }
}




