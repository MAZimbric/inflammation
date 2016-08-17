#helper functions for this project

#takes a dataframe and extracts only the rows matching a pattern in column
extract.match.rows <- function(data, column, ptrn) {
  matches <- grep(pattern = ptrn, column, value = FALSE)
  data <- data[matches,] 
  return(data)
}

#sets na.rm to true in max
na.max <- function(x) max(x, na.rm = TRUE)

#mean helper function to pass to others, setting na.rm to True
na.mean <- function(x) mean(x, na.rm = TRUE)

#helper to deal with the output of aggregate
agg.out <- function(x) {
  x$storage_days <- x$Group.1
  x$samplesource <- x$Group.2
  x <- x[c(3,5,6)]
  x$storage_days <- as.integer(x$storage_days)
  return(x)
}

#removes columns that are all NA
drop.na.column <- function (df) df[,colSums(is.na(df))<nrow(df)]
