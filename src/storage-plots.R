setwd("D://mzimbric/Desktop/Projects/inflammation")
library(ProjectTemplate)
load.project()

#This script generates plots for each inflammatory marker
#with number of days stored as the independent variable and 
#marker concentration as the dependent variable
#Initially we'll make Line plots with each of the 5 sample sources.
attach(inflammation.data)
storage_days <- as.factor(storage_days)

ggplot(inflammation.data, aes(x=storage_days, y=IL.1b, color=samplesource, shape=samplesource)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)
