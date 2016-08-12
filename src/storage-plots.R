setwd("D://mzimbric/Desktop/Projects/inflammation")
library(ProjectTemplate)
load.project()

#This script generates plots for each inflammatory marker
#with number of days stored as the independent variable and 
#marker concentration as the dependent variable
#Initially we'll make Line plots with each of the 5 sample sources.
attach(inflammation.data)
storage_days <- as.factor(storage_days)

#mean helper function to pass to others, setting na.rm to True
na.mean <- function(x) mean(x, na.rm = TRUE)

#helper to deal with the output of aggregate
agg.out <- function(x) {
  x$storage_days <- x$Group.1
  x$samplesource <- x$Group.2
  x <- x[c(3,5,6)]
  return(x)
}

#subset data
data.IL.1b <- inflammation.data[c('samplesource','replicate','storage_days','IL.1b')]

#calculate means and standard deviations subsetted by samplesource and storage_days


mn.IL1b <- aggregate.data.frame(data.IL.1b, list(storage_days, samplesource), na.mean)
mn.IL1b <- agg.out(mn.IL1b)

sd.IL1b <- aggregate.data.frame(data.IL.1b, list(storage_days, samplesource), sd)
sd.IL1b <- agg.out(sd.IL1b)

ggplot(inflammation.data, aes(x=storage_days, y=IL.1b, color=samplesource, shape=samplesource)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)
