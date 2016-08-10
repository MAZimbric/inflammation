#The following translates the sample code into something useable
#The initial digit represents the number of days the sample was stored 
inflammation.reads1$storage_days <- str_match(inflammation.reads1$Sample, "^[:digit:][:digit:]?")

inflammation.reads1$replicate[grep(pattern = "[BDFHJL]", inflammation.reads1$Sample, value = FALSE)] <- 2
inflammation.reads1$replicate[grep(pattern = "[ACEGIKM]", inflammation.reads1$Sample, value = FALSE)] <- 1
inflammation.reads1$replicate <- as.factor(inflammation.reads1$replicate)

#The letter represents the patient. A and B are the same patient. C and D are the same patient, etc. 
inflammation.reads1$samplesource <- str_match(inflammation.reads1$Sample, "[:alpha:]$")
