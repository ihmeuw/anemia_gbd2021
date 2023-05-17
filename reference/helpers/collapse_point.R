########################################################################################################################
## Date: 09/14/18
## Purpose: Function for collapsing draws into mean, lower, and upper (95% UI)
########################################################################################################################

library(matrixStats)

#----COLLAPSE FUNCTION--------------------------------------------------------------------------------------------------
# write function for collapsing draws into mean-lower-upper
collapse_point <- function(input_file, draws_name="draw", variance=FALSE) {
  
  # get data to collapse
  collapsed       <- copy(input_file)
  
  # columns to collapse
  collapse_cols   <- c(colnames(collapsed)[grep(draws_name, colnames(collapsed))])
  
  # column names to keep
  cols <- c(colnames(collapsed)[!colnames(collapsed) %like% draws_name], 
            "mean", "lower", "upper")
  
  # calculate mean, lower, and upper
  collapsed$mean  <- rowMeans(subset(collapsed, select=collapse_cols))
  collapsed$lower <- apply(subset(collapsed, select=collapse_cols), 1, function(x) quantile(x, 0.025))
  collapsed$upper <- apply(subset(collapsed, select=collapse_cols), 1, function(x) quantile(x, 0.975))
  
  # calculate variance
  if (variance) { collapsed$variance <- apply(subset(collapsed, select=collapse_cols), 1, function(x) var(x))
  cols <- c(cols, "variance") }
  
  # just keep needed columns
  collapsed <- subset(collapsed, select=cols)
  
  return(collapsed)
}
#***********************************************************************************************************************