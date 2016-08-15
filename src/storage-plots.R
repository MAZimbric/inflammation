setwd("D://mzimbric/Desktop/Projects/inflammation")
library(ProjectTemplate)
load.project()

#This script generates plots for each inflammatory marker
#with number of days stored as the independent variable and 
#marker concentration as the dependent variable
#Initially we'll make Line plots with each of the 4 sample sources.

#mean helper function to pass to others, setting na.rm to True
na.mean <- function(x) mean(x, na.rm = TRUE)

#helper to deal with the output of aggregate
agg.out <- function(x) {
  x$storage_days <- x$Group.1
  x$samplesource <- x$Group.2
  x <- x[c(3,5,6)]
  x$storage_days <- as.integer(x$storage_days)
  return(x)
}

#function wrapper for plots
plot.markers <- function(x, na.rm = TRUE){
  markers <- names(inflammation.data[4:ncol(inflammation.data)])
  for (i in seq_along(markers)){
  
    #subset data
    data.marker <- inflammation.data[c('samplesource','replicate','storage_days',markers[i])]

    #calculate means and standard deviations and number of replicates subsetted by samplesource and storage_days
    mn.marker <- aggregate.data.frame(data.marker, list(storage_days, samplesource), na.mean)
    mn.marker <- agg.out(mn.marker)
    names(mn.marker)[names(mn.marker)== markers[i]] <- "mn"
    #mn.marker <- rename(mn.marker, replace = c(as.character(markers[i]) = "mn"))

    sd.marker <- aggregate.data.frame(data.marker, list(storage_days, samplesource), sd)
    sd.marker <- agg.out(sd.marker)
    names(sd.marker)[names(sd.marker)== markers[i]] <- "sd"
    
    reps <- aggregate.data.frame(data.marker, list(storage_days, samplesource), length)
    reps <- agg.out(reps)
    names(reps)[names(reps)== markers[i]] <- "observations"
    
    
    #merge mean and standard deviation data frames
    marker.summary <- join(mn.marker,sd.marker)
    marker.summary <- join(marker.summary, reps)

    #calculate standard error
    marker.summary$sem <- marker.summary$sd/sqrt(marker.summary$observations)

    marker.plot <- ggplot(marker.summary, aes(x=storage_days, y=mn, colour=samplesource), main = paste("Levels of", marker, "after storage")) + 
      geom_point() +
      geom_line() +
      geom_errorbar(aes(ymin=mn-sem, ymax=mn+sem), width=.1) +
      scale_x_continuous(name = "Days Stored at 4ºC", breaks = c(0,3,7,14,28)) + 
      scale_y_log10(name = paste("pg/mL of", marker)) +
      scale_colour_hue("Patient", labels = c("A", "B", "C", "D", "E"))
    
    ggsave(plots,filename=paste("myplot", markers[i],".png",sep=""))
   
    
    }
}

plot.markers(inflammation.data)
