#the data are contained in multiple files that need to be merged
inflammation.data <- merge.data.frame(inflammation.reads1, inflammation.reads2, all = TRUE)

#this function takes the dataset and removes standards and clinical samples
extract_useful <- function(data, samples) {
  matches <- grep(pattern = "^\\d\\d?", samples, value = FALSE)
  data <- data[matches,] 
  return(data)
}
#and now we call them on the two sets
inflammation.data <- extract_useful(inflammation.data, inflammation.data$Sample)

#The following translates the sample code into something useable
#The initial digit represents the number of days the sample was stored 
inflammation.data$storage_days <- str_match(inflammation.data$Sample, "^[:digit:][:digit:]?")

inflammation.data$replicate[grep(pattern = "[ACEGIKM]", inflammation.data$Sample, value = FALSE)] <- 1
inflammation.data$replicate[grep(pattern = "[BDFHJL]", inflammation.data$Sample, value = FALSE)] <- 2
inflammation.data$replicate <- as.factor(inflammation.data$replicate)

#The letter represents the patient. A and B are the same patient. C and D are the same patient, etc. 
#When you figure out an elegant way to do this, please do it.
inflammation.data$samplesource <- str_match(inflammation.data$Sample, "[:alpha:]$")
for (i in 1:length(inflammation.data$samplesource)) {
  if (inflammation.data$samplesource[i] == "A" | inflammation.data$samplesource[i] == "B") inflammation.data$samplesource[i] <- "1"
  else if (inflammation.data$samplesource[i] == "C" | inflammation.data$samplesource[i] == "D") inflammation.data$samplesource[i] <- "2"
  else if (inflammation.data$samplesource[i] == "E" | inflammation.data$samplesource[i] == "F") inflammation.data$samplesource[i] <- "3"
  else if (inflammation.data$samplesource[i] == "G" | inflammation.data$samplesource[i] == "H") inflammation.data$samplesource[i] <- "4"
  else if (inflammation.data$samplesource[i] == "I" | inflammation.data$samplesource[i] == "J") inflammation.data$samplesource[i] <- "5"
  else inflammation.data$samplesource[i] <- "unknown"
}
#removed processed code
inflammation.data <- inflammation.data[3:ncol(inflammation.data)]

#rearrange columns
columns <- ncol(inflammation.data)
inflammation.data <- inflammation.data[c(columns,columns-1,columns-2,1:(columns-3))]

#correct for dilution of samples
inflammation.data[4:columns] <- lapply(inflammation.data[4:columns], function(x) x*2)

#make storage days an integer
inflammation.data$storage_days <- as.integer(inflammation.data$storage_days)

#remove source 4 since the data is incomplete
inflammation.data <- inflammation.data[which(inflammation.data$samplesource != 4),]
