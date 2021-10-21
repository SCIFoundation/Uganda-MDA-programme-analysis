#===================================================================================================================#
#=====================================    Estimating coverage functions    =========================================#

#==================================================================#
# coverage estimation function # 1 (total doses/ total targeted)   #

estimate_coverage_func1 <- function(dat1, dat2) {
  
  mat <- matrix(NA, nrow = 121, ncol = 7) # make empty matrix with correct dimensions for cov x district/yesr
  
  data_frame <- data.frame(mat) # turn into dataframe
  
  #names(data_frame) <- paste0("Cov_", seq_along(data_frame)) # rename col names (coverage x year)
  
  names(data_frame) <- paste0("Cov_", 2003:2009) # rename col names (coverage x year)
  
  cov_data <- cbind(dat1, dat2[ , 3:9], data_frame) # combine dataframes for calculations
  
  for (i in 17:ncol(cov_data)) {
    
    cov_data[ , i] <- cov_data[ , i - 14] / cov_data[ , i - 7] * 100 # calculate coverage (total dose/ total targeted)
    
  }
  
  cov_data[ , 17:23] <- round(cov_data[ , 17:23], digits = 2) # round cov values to 2 digits
  
  cov_data <- cov_data[ -c(71,108), c(1,2,17:23)] # reintroduce # if want df only with cov values
  
  return(cov_data)
  
}


#==================================================================#
# coverage estimation function # 2 (total doses/ district pop)     #

estimate_coverage_func2 <- function(dat1, dat2) {
  
  mat <- matrix(NA, nrow = 121, ncol = 7) # make empty matrix with correct dimensions for cov x district/yesr
  
  data_frame <- data.frame(mat) # turn into dataframe
  
  #names(data_frame) <- paste0("Cov_", seq_along(data_frame)) # rename col names (coverage x year)
  
  names(data_frame) <- paste0("Cov_", 2003:2009) # rename col names (coverage x year)
  
  cov_data <- cbind(dat1, dat2[ , 5:11], data_frame) # combine dataframes for calculations
  
  for (i in 17 : ncol(cov_data)) {
    
    cov_data[ , i] <- cov_data[ , i - 14] / cov_data[ , i - 7] * 100 # calculate coverage (total dose/ total targeted)
    
  }
  
  cov_data[ , 17:23] <- round(cov_data[ , 17:23], digits = 2) # round cov values to 2 digits
  
  cov_data <- cov_data[ -c(71,108), c(1,2,17:23)] # reintroduce # if want df only with cov values
  
  return(cov_data)
  
}


#===============================================================================#
# coverage estimation function # 3 (total doses/ largest or max target pop)     #

estimate_coverage_func3 <- function(dat1, dat2) {
  
  mat <- matrix(NA, nrow = 121, ncol = 7) # make empty matrix with correct dimensions for cov x district/yesr
  
  data_frame <- data.frame(mat) # turn into dataframe
  
  #names(data_frame) <- paste0("Cov_", seq_along(data_frame)) # rename col names (coverage x year)
  
  names(data_frame) <- paste0("Cov_", 2003:2009) # rename col names (coverage x year)
  
  cov_data <- cbind(dat1, dat2[ , 3], data_frame) # combine dataframes for calculations
  
  for (i in 11 : ncol(cov_data)) {
    
    cov_data[ , i] <- cov_data[ , i - 8] / cov_data[ , 10] * 100 # calculate coverage (total dose/ total targeted)
    
  }
  
  cov_data[ , 11:17] <- round(cov_data[ , 11:17], digits = 2) # round cov values to 2 digits
  
  cov_data <- cov_data[ -c(71,108), c(1,2,11:17)] # reintroduce # if want df only with cov values
  
  return(cov_data)
  
}
