setwd("D://mzimbric/Desktop/Projects/inflammation")
library(ProjectTemplate)
load.project()

#This script generates plots for each inflammatory marker
#with number of days stored as the independent variable and 
#marker concentration as the dependent variable
#Initially we'll make Line plots with each of the 5 sample sources.
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

#drop rows with NA as the value
data.IL.1b <- data.IL.1b[which(!is.na(IL.1b)),]
attach(data.IL.1b)

#calculate means and standard deviations and number of replicates subsetted by samplesource and storage_days
mn.IL1b <- aggregate.data.frame(data.IL.1b, list(storage_days, samplesource), na.mean)
mn.IL1b <- agg.out(mn.IL1b)
mn.IL1b <- rename(mn.IL1b, replace = c("IL.1b" = "mn"))

sd.IL1b <- aggregate.data.frame(data.IL.1b, list(storage_days, samplesource), sd)
sd.IL1b <- agg.out(sd.IL1b)
sd.IL1b <- rename(sd.IL1b, c("IL.1b" = "sd"))

reps <- aggregate.data.frame(data.IL.1b, list(storage_days, samplesource), length)
reps <- agg.out(reps)
reps <- rename(reps, c("IL.1b" = "observations"))
#merge mean and standard deviation data frames
IL1b <- join(mn.IL1b,sd.IL1b)
IL1b <- join(IL1b, reps)

#calculate standard error
IL1b$sem <- IL1b$sd/sqrt(IL1b$observations)

ggplot(IL1b, aes(x=storage_days, y=mn, colour=samplesource)) + 
  geom_point() +
  geom_line(group = mn ) +
  geom_errorbar(aes(ymin=mn-sem, ymax=mn+sem), width=.1)
  
  


