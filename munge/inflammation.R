source(file = "src/helpers.R")

#the data are contained in multiple files that need to be merged
inflammation.data <- merge.data.frame(inflammation.reads1, inflammation.reads2, all = TRUE)
#the 3rd sheet has extra empty rows for some reason, remove them
inflammation.reads3 <- inflammation.reads3[1:26,]
inflammation.data <- merge.data.frame(inflammation.reads3, inflammation.data, all = TRUE)

#replace < X with 1 to act as "zeros" in the log transformed data
len.id <- ncol(inflammation.data)
inflammation.data[3:len.id] <- as.data.frame(sapply(inflammation.data[3:len.id], function (x) str_replace(x, "^< [0-9]*[.][0-9]*", "1")))
inflammation.data[3:len.id] <- as.data.frame(sapply(inflammation.data[3:len.id], function (x) str_replace(x, "^> [0-9]*[.][0-9]*", NA)))


#log transform the data
inflammation.data[3:ncol(inflammation.data)] <- apply(inflammation.data[3:ncol(inflammation.data)], 2, log10)

#lines 5 - 60 clean the and prepare the storage data
#this function takes the dataset and extracts only the storage sests
inflammation.storage.data <- extract.match.rows(inflammation.data, inflammation.data$Sample, "^\\d\\d?")

#The following translates the storage sample code into something useable
#The initial digit represents the number of days the sample was stored 
inflammation.storage.data$storage_days <- str_match(inflammation.storage.data$Sample, "^[:digit:][:digit:]?")

inflammation.storage.data$replicate[grep(pattern = "[BDFHJL]", inflammation.storage.data$Sample, value = FALSE)] <- 2
inflammation.storage.data$replicate[grep(pattern = "[ACEGIKM]", inflammation.storage.data$Sample, value = FALSE)] <- 1
inflammation.storage.data$replicate <- as.factor(inflammation.storage.data$replicate)

#The letter represents the patient. A and B are the same patient. C and D are the same patient, etc. 
#When you figure out an elegant way to do this, please do it.
inflammation.storage.data$samplesource <- str_match(inflammation.storage.data$Sample, "[:alpha:]$")
for (i in 1:length(inflammation.storage.data$samplesource)) {
  if (inflammation.storage.data$samplesource[i] == "A" | inflammation.storage.data$samplesource[i] == "B") inflammation.storage.data$samplesource[i] <- "1"
  else if (inflammation.storage.data$samplesource[i] == "C" | inflammation.storage.data$samplesource[i] == "D") inflammation.storage.data$samplesource[i] <- "2"
  else if (inflammation.storage.data$samplesource[i] == "E" | inflammation.storage.data$samplesource[i] == "F") inflammation.storage.data$samplesource[i] <- "3"
  else if (inflammation.storage.data$samplesource[i] == "G" | inflammation.storage.data$samplesource[i] == "H") inflammation.storage.data$samplesource[i] <- "4"
  else if (inflammation.storage.data$samplesource[i] == "I" | inflammation.storage.data$samplesource[i] == "J") inflammation.storage.data$samplesource[i] <- "5"
  else inflammation.storage.data$samplesource[i] <- "unknown"
}
#removed processed code
inflammation.storage.data <- inflammation.storage.data[3:ncol(inflammation.storage.data)]

#rearrange columns
columns <- ncol(inflammation.storage.data)
inflammation.storage.data <- inflammation.storage.data[c(columns,columns-1,columns-2,1:(columns-3))]

#correct for dilution of samples
inflammation.storage.data[4:columns] <- lapply(inflammation.storage.data[4:columns], function(x) x*2)

#make storage days an integer
inflammation.storage.data$storage_days <- as.integer(inflammation.storage.data$storage_days)

#remove source 4 since the data is incomplete
inflammation.storage.data <- inflammation.storage.data[which(inflammation.storage.data$samplesource != 4),]

#remove columns that are all NA
inflammation.storage.data <- drop.na.column(inflammation.storage.data)



#This space intentionally left blank





#Lines 60 - 69 clean and prepare the standards 
#We would like to extract the maximum of the seventh standard values for each marker to use as a threshold
inflammation.standards <- extract.match.rows(inflammation.data, inflammation.data$Sample, "^S7")
thresholds <- apply(inflammation.standards[3:ncol(inflammation.standards)], 2, na.max)




#find the global maximum of all marker levels to use as a common y-axis limit
unknowns <- extract.match.rows(inflammation.data, inflammation.data$Plate, "^Unknown")

unknowns.list <- apply(unknowns[3:ncol(unknowns)], 2, na.max)
y.max <- max(unknowns.list)
#add a buffer value to y.max
y.max <- y.max + .05*y.max



#extract clinical data
clinical.inflammation.data <- extract.match.rows(inflammation.data, inflammation.data$Sample, "^SP\\d")
clinical.inflammation.data <- drop.na.column(clinical.inflammation.data)

combined.clinical <- merge(x=clinical.inflammation.data, y=clinical.data, by.x="Sample", by.y="sample")
#drop excess columns 
combined.clinical <- combined.clinical[c(-2,-41:-45)]
combined.clinical <- rename(combined.clinical, c("disease_status..0.no..1.yes." = "disease_status", "sample_timing..0.pre..1.post." = "sample_timing"))
combined.clinical <- combined.clinical[c(1,32:39,2:31)]

combined.clinical$disease_status <- as.factor(combined.clinical$disease_status)
levels(combined.clinical$disease_status) <- c("no", "yes")

combined.clinical$sample_timing <- as.factor(combined.clinical$sample_timing)
levels(combined.clinical$sample_timing) <- c("before", "after")
