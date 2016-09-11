# setwd("D://mzimbric/Desktop/Projects/inflammation")
# library(ProjectTemplate)
# load.project()
# source(file = "src/helpers.R")

#This file will create plots for levels of markers in clinical patients, 
#with patient age as the independent variable and marker level as the 
#dependent variable.

prep.clinical.data <- function (data) {
  #drop columns 35-42, since none of the patients have more than two measurements of these markers
  combined.clinical <- data[-35:-42]

  #retain only patients with both before and after cultures
  combined.clinical <- filter(combined.clinical, retro_ID %in% c("204", "132", "491"))
  
  #create column of time relative to first positive NTM culture
  combined.clinical <- mutate(combined.clinical, relative_time = X1st_NTM_age - SP_age)
  combined.clinical <- combined.clinical[c(1:9, 35, 10:34)]
  
  #filter out timepoints greater than 2 years from initial NTM detection
  combined.clinical <- filter(combined.clinical, relative_time < 2 & relative_time > -2)

  #stripping out unneeded data
  combined.clinical <- combined.clinical[c(3,6,10,11:35)]

  #melt data into long form
  long.combined.clinical <- melt(combined.clinical, id = c(1:3), na.rm = TRUE)
  long.combined.clinical <- rename(long.combined.clinical, marker = variable, marker_value = value)
  return(long.combined.clinical)
}

#This is the basic plotting function. It takes a clinical data dataframe and the name 
#of the marker of interest as a string, a threshold value and a y.value
age.marker.plot <- function(dataframe, marker_name, threshold, y.value) {
  if (ncol(dataframe) > 5) {
    dataframe <- prep.clinical.data(dataframe)
  }
  plot.data <- filter(dataframe, marker == marker_name)
  
  marker.plot <- ggplot(plot.data, 
                        aes_string(x= "relative_time", y = "marker_value", 
                                   color = "retro_ID")) +
    geom_point() +
    geom_line(aes(linetype = disease_status)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = threshold) +
    scale_x_continuous(name = "Years Relative to First Positive NTM culture",
                       breaks = seq(-2,2),
                       limits = c(-2,1.5)) +
    scale_y_continuous(name = paste("log10 of", marker_name, "(pg/mL)"), 
                       limits = c(-0.5, y.value), 
                       breaks = seq(0,y.value,by = 1)) +
    scale_linetype_discrete(name = "Disease Status")+
    scale_color_discrete(name = "Patient")
  
  return(marker.plot)
}

#This function saves zeroed plots with all patients for all markers individually
plot.all.age.marker <- function(clinical, thresholds, y.value){
  plot.data <- prep.clinical.data(clinical)
  markers <- levels(plot.data$marker)
  for (i in seq_along(markers)) {
      marker.plot <- age.marker.plot(plot.data, markers[i], thresholds[markers[i]], y.value)
      ggsave(marker.plot, filename=paste("figures/clinical/zeroed-", markers[i], "-plot.png",sep=""))
  }
}

#This function produces a faceted plot given a character vector of marker names
labels <- c(IL.1b = "IL-1beta", MIG = "MIG", MCP.1 = "MCP-1", IL.8 = "IL-8", IL.1RA = "IL-1Ra", IL.4 = "IL-4" )

faceted.clinical <- function(clinical, markers, labels) {
  clinical <- prep.clinical.data(clinical)
  clinical <- filter(clinical, marker %in% markers)
  
  marker.plot <- ggplot(clinical, 
                        aes_string(x= "relative_time", y = "marker_value", 
                                   color = "retro_ID")) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(name = "Years Relative to First Positive NTM culture",
                       breaks = seq(-2,2),
                       limits = c(-2,1.5)) +
    scale_y_continuous(name = paste("log10 Sputum Level (pg/mL)"), 
                       limits = c(0, 5.5), 
                       breaks = seq(0,5,by = 1)) +
    #scale_linetype_discrete(name = "Disease Status")+
    scale_color_discrete(name = "Patient")+
    facet_wrap(~marker, labeller = labeller(marker = as_labeller(labels)))+
    theme(panel.background = element_rect(fill = 'white', colour = 'grey'), 
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_line(color = "grey90"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          strip.text = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
  
  return(marker.plot)
}