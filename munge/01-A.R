#this function takes the dataset and removes standards and clinical samples
extract_useful <- function(data, samples) {
  matches <- grep(pattern = "^\\d\\d?", samples, value = FALSE)
  data <- data[matches,] 
  return(data)
}
#and now we call them on the two sets
inflammation.reads1 <- extract_useful(inflammation.reads1, inflammation.reads1$Sample)
inflammation.reads2 <- extract_useful(inflammation.reads2, inflammation.reads2$Sample)

# The followinwg problem was dealt with by naughty actions on the xlsx file. See README
# #In order to merge the sets, we need to deal with the < entries and convert the marker values to numerics
# rm_less_thans <- function(data_column){
#   less_thans <- grep(pattern = "<", as.character(data_column), value = FALSE)
#   data_column[less_thans] <- NA
#   return(data_column)
# }
# 
# f <- colwise(rm_less_thans)
# inflammation.reads1 <- f(inflammation.reads1)
# inflammation.reads2 <- f(inflammation.reads2)
# 
# #function to deal with lack in R, factor to numeric conversion
# factor.to.numeric <- function (d) {
#   d <- as.numeric(as.character(d))
#   return(d)
# }
#   
# #this code does not work
# inflammation.reads1[3:ncol(inflammation.reads1)] <- ddply(inflammation.reads1[3:ncol(inflammation.reads1)], is.factor == TRUE, factor.to.numeric)
# inflammation.reads2 <- ddply(inflammation.reads2, 3:27, factor.to.numeric)


#The following translates the sample code into something useable
#The initial digit represents the number of days the sample was stored 
inflammation.reads1$storage_days <- str_match(inflammation.reads1$Sample, "^[:digit:][:digit:]?")

inflammation.reads1$replicate[grep(pattern = "[BDFHJL]", inflammation.reads1$Sample, value = FALSE)] <- 2
inflammation.reads1$replicate[grep(pattern = "[ACEGIKM]", inflammation.reads1$Sample, value = FALSE)] <- 1
inflammation.reads1$replicate <- as.factor(inflammation.reads1$replicate)

#The letter represents the patient. A and B are the same patient. C and D are the same patient, etc. 
#When you figure out an elegant way to do this, please do it.
inflammation.reads1$samplesource <- str_match(inflammation.reads1$Sample, "[:alpha:]$")
