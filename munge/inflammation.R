source(file = "src/helpers.R")

#the data are contained in multiple files that need to be merged
data <- merge.data.frame(reads1, reads2, all = TRUE)
#the 3rd sheet has extra empty rows for some reason, remove them
reads3 <- reads3[1:26,]
data <- merge.data.frame(reads3, data, all = TRUE)

initial.data.cleaning <- function(data){
  #replace < X with 1 to act as "zeros" in the log transformed data
  len.id <- ncol(data)
  data[3:len.id] <- as.data.frame(sapply(data[3:len.id], function (x) str_replace(x, "^< [0-9]*[.][0-9]*", "1")))
  
  #After the update to stringr 1.1.0, str_replace is unable to replace a string with NA, 
  #so using a stupidly high number as a placeholder
  data[3:len.id] <- as.data.frame(sapply(data[3:len.id], function (x) str_replace(x, "^> [0-9]*", "1000000000")))
  data[3:len.id] <- as.data.frame(sapply(data[3:len.id], as.numeric))
  
  
  #log transform the data
  data[3:ncol(data)] <- apply(data[3:ncol(data)], 2, log10)
  return(data)
}

storage.cleaning <- function(data){
  #lines 5 - 60 clean the and prepare the storage data
  #this function takes the dataset and extracts only the storage sests
  storage.data <- extract.match.rows(data, data$Sample, "^\\d\\d?")
  
  #The following translates the storage sample code into something useable
  #The initial digit represents the number of days the sample was stored 
  storage.data$storage_days <- str_match(storage.data$Sample, "^[:digit:][:digit:]?")
  
  storage.data$replicate[grep(pattern = "[ACEGIKM]", storage.data$Sample, value = FALSE)] <- 1
  storage.data$replicate[grep(pattern = "[BDFHJL]", storage.data$Sample, value = FALSE)] <- 2
  storage.data$replicate <- as.factor(storage.data$replicate)
  
  #The letter represents the patient. A and B are the same patient. C and D are the same patient, etc. 
  #When you figure out an elegant way to do this, please do it.
  storage.data$samplesource <- str_match(storage.data$Sample, "[:alpha:]$")
  for (i in 1:length(storage.data$samplesource)) {
    if (storage.data$samplesource[i] == "A" | storage.data$samplesource[i] == "B") storage.data$samplesource[i] <- "1"
    else if (storage.data$samplesource[i] == "C" | storage.data$samplesource[i] == "D") storage.data$samplesource[i] <- "2"
    else if (storage.data$samplesource[i] == "E" | storage.data$samplesource[i] == "F") storage.data$samplesource[i] <- "3"
    else if (storage.data$samplesource[i] == "G" | storage.data$samplesource[i] == "H") storage.data$samplesource[i] <- "4"
    else if (storage.data$samplesource[i] == "I" | storage.data$samplesource[i] == "J") storage.data$samplesource[i] <- "5"
    else storage.data$samplesource[i] <- "unknown"
  }
  #removed processed code
  storage.data <- storage.data[3:ncol(storage.data)]
  
  #rearrange columns
  columns <- ncol(storage.data)
  storage.data <- storage.data[c(columns,columns-1,columns-2,1:(columns-3))]
  
  #correct for dilution of samples
  storage.data[4:columns] <- lapply(storage.data[4:columns], function(x) x+log10(2))
  
  #make storage days an integer
  storage.data$storage_days <- as.integer(storage.data$storage_days)
  
  #remove source 4 since the data is incomplete
  storage.data <- storage.data[which(storage.data$samplesource != 4),]
  
  #remove source 5 since the data is from BAL, not sputum
  storage.data <- storage.data[which(storage.data$samplesource !=5),]
  
  #make samplesource a factor
  storage.data$samplesource <- as.factor(storage.data$samplesource)
  
  #melt data into long form
  storage.data <- melt(storage.data, id = c("samplesource", "replicate", "storage_days"), na.rm = TRUE)
  storage.data <- rename(storage.data, marker_name = variable, marker_level = value)
  
  return(storage.data)
}

#We would like to extract the maximum of the seventh standard values for each marker to use as a threshold
generate.thresholds <- function(data){
  standards <- extract.match.rows(data, data$Sample, "^S7")
  thresholds <- apply(standards[3:ncol(standards)], 2, na.max)
  return(thresholds)
}

#find the global maximum of all marker levels to use as a common y-axis limit
find.max <- function(data) {
  unknowns <- extract.match.rows(data, data$Plate, "^Unknown")
  #correct for dilution
  unknowns[4:ncol(unknowns)] <- lapply(unknowns[4:ncol(unknowns)], function(x) x+log10(2))
  
  unknowns.list <- apply(unknowns[3:ncol(unknowns)], 2, na.max)
  y.max <- max(unknowns.list)
  #add a buffer value to y.max
  y.max <- y.max + .05*y.max
  return(y.max)
}

clinical.cleaning <- function(data, patient.data) {
  #extract clinical data
  clinical.data <- extract.match.rows(data, data$Sample, "^SP\\d")
  
  #correct for dilution of samples
  clinical.data[4:ncol(clinical.data)] <- lapply(clinical.data[4:ncol(clinical.data)], function(x) x+log10(2))
  
  #data frame has several empty columns as an artefact of the Project Template loading process
  patient.data <- drop.na.column(patient.data)
  
  #combine clinical and patient data
  combined.clinical <- merge(x=patient.data, y=clinical.data, by.x="sample", by.y="Sample")
  combined.clinical <- combined.clinical[-10]
  
  #rename columns
  combined.clinical <- rename(combined.clinical, disease_status = disease_status..0.no..1.yes., sample_timing = sample_timing..0.pre..1.post.)
  
  combined.clinical$disease_status <- as.factor(combined.clinical$disease_status)
  levels(combined.clinical$disease_status) <- c("no", "yes")
  
  combined.clinical$sample_timing <- as.factor(combined.clinical$sample_timing)
  levels(combined.clinical$sample_timing) <- c("before", "after")
  
  combined.clinical$retro_ID <- as.factor(combined.clinical$retro_ID)
  return(combined.clinical)
}

process.storage.data <- function(data) {
  
  #calculate means and standard deviations and number of replicates subsetted by samplesource and storage_days
  mn <- aggregate.data.frame(data, list(data$storage_days, data$samplesource, data$marker_name), mean)
  mn <- agg.out(mn)
  mn <- rename(mn, mn = marker_level)
  
  sd <- aggregate.data.frame(data, list(data$storage_days, data$samplesource, data$marker_name), sd)
  sd <- agg.out(sd)
  sd <- rename(sd, sd = marker_level)
  
  reps <- aggregate.data.frame(data, list(data$storage_days, data$samplesource, data$marker_name), length)
  reps <- agg.out(reps)
  reps <- rename(reps, observations = marker_level)
  
  
  #merge mean and standard deviation data frames
  summary <- left_join(mn,sd)
  summary <- left_join(summary, reps)
  
  #calculate standard error
  summary$sem <- summary$sd/sqrt(summary$observations)
  
  return(summary)
}

#call the functions
data <- initial.data.cleaning(data)
storage.data <- storage.cleaning(data)
thresholds <- generate.thresholds(data)
y.max <- find.max(data)
combined.clinical <- clinical.cleaning(data, patient.data)
summary.storage <- process.storage.data(storage.data)

#cytokines of interest
cyt <- c("MCP.1", "IL.8", "IL.1RA", "IL.1b", "G.CSF",  "MIG")

