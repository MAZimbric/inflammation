#this function takes the dataset and removes standards and clinical samples
extract_useful <- function(data, samples) {
  matches <- grep(pattern = "^\\d\\d?", samples, value = FALSE)
  data <- data[matches,] 
  return(data)
}
#and now we call them on the two sets
inflammation.reads1 <- extract_useful(inflammation.reads1, inflammation.reads1$Sample)
inflammation.reads2 <- extract_useful(inflammation.reads2, inflammation.reads2$Sample)

#In order to merge the sets, we need to deal with the < entries
rm_less_thans <- function(data){
  
}

inflammation_data <- merge(inflammation.reads2, inflammation.reads1)

#The following translates the sample code into something useable
#The initial digit represents the number of days the sample was stored 
inflammation.reads1$storage_days <- str_match(inflammation.reads1$Sample, "^[:digit:][:digit:]?")

inflammation.reads1$replicate[grep(pattern = "[BDFHJL]", inflammation.reads1$Sample, value = FALSE)] <- 2
inflammation.reads1$replicate[grep(pattern = "[ACEGIKM]", inflammation.reads1$Sample, value = FALSE)] <- 1
inflammation.reads1$replicate <- as.factor(inflammation.reads1$replicate)

#The letter represents the patient. A and B are the same patient. C and D are the same patient, etc. 
#When you figure out an elegant way to do this, please do it.
inflammation.reads1$samplesource <- str_match(inflammation.reads1$Sample, "[:alpha:]$")
