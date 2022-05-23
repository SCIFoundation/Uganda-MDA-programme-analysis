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

estimate_coverage_originaldistr_func1 <- function(dat1, dat2) {
  
  mat <- matrix(NA, nrow = 168, ncol = 17)  # make empty matrix with correct dimensions for cov x district/yesr
  
  data_frame <- data.frame(mat) # turn into dataframe
  
  #names(data_frame) <- paste0("Cov_", seq_along(data_frame)) # rename col names (coverage x year)
  
  names(data_frame) <- paste0("Cov_", 2003:2019) # rename col names (coverage x year) 
  
  data_frame <- data_frame %>%
    add_column(Cov_2009rnd2 = NA,
               .after = "Cov_2009")
  
  cov_data <- cbind(dat1[ , 2:19], dat2[ , 2:19], data_frame) # combine dataframes for calculations
  
  for (i in 37:ncol(cov_data)) {
    
    cov_data[ , i] <- cov_data[ , i - 36] / cov_data[ , i - 18] * 100 # calculate coverage (total dose/ total targeted)
    
  } 
  
  cov_data[ , 37:54] <- round(cov_data[ , 37:54], digits = 2) # round cov values to 2 digits
  
  cov_data <- cov_data[, c(37:54)] # reintroduce # if want df only with cov values
  
  cov_data_names <- dat1[, c(1,20)]

  cov_data <- cbind(cov_data_names, cov_data)
  
  return(cov_data)
  
}

estimate_coverage_originaldistr_func1a <- function(dat1, dat2) {
  
  mat <- matrix(NA, nrow = 56, ncol = 19)  # make empty matrix with correct dimensions for cov x district/yesr
  
  data_frame <- data.frame(mat) # turn into dataframe
  
  #names(data_frame) <- paste0("Cov_", seq_along(data_frame)) # rename col names (coverage x year)
  
  names(data_frame) <- paste0("Cov_", 2003:2019) # rename col names (coverage x year) 
  
  data_frame <- data_frame %>%
    add_column(Cov_2009rnd2 = NA,
               .after = "Cov_2009")
  
  data_frame <- data_frame %>%
    add_column(Cov_2010rnd2 = NA,
               .after = "Cov_2010")
  
  cov_data <- cbind(dat1[ , 2:20], dat2[ , 2:20], data_frame[ , 1:19]) # combine dataframes for calculations
  
  for (i in 39:ncol(cov_data)) {
    
    cov_data[ , i] <- cov_data[ , i - 38] / cov_data[ , i - 19] * 100 # calculate coverage (total dose/ total targeted)
    
  } 
  
  cov_data[ , 39:57] <- round(cov_data[ , 39:57], digits = 2) # round cov values to 2 digits
  
  cov_data <- cov_data[, c(39:57)] # reintroduce # if want df only with cov values
  
  cov_data_names <- dat1[, c(1,20)]
  
  cov_data <- cbind(cov_data_names, cov_data)
  
  return(cov_data)
  
}


# TO DO: extend coverage calc this to 2020

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


estimate_coverage_originaldistr_func2 <- function(dat1, dat2) {
  
  mat <- matrix(NA, nrow = 168, ncol = 17)  # make empty matrix with correct dimensions for cov x district/yesr
  
  data_frame <- data.frame(mat) # turn into dataframe
  
  #names(data_frame) <- paste0("Cov_", seq_along(data_frame)) # rename col names (coverage x year)
  
  names(data_frame) <- paste0("Cov_", 2003:2019) # rename col names (coverage x year) 
  
  data_frame <- data_frame %>%
    add_column(Cov_2009rnd2 = NA,
               .after = "Cov_2009")
  
  temp_2009_dat <- dat2$pop_2009
  
  temp_districtpop_dat <- dat2
  
  temp_districtpop_dat <- temp_districtpop_dat %>%
    add_column(pop2009_rnd2 = temp_2009_dat,
               .after = "pop_2009")
  
  cov_data <- cbind(dat1[ , 2:19], temp_districtpop_dat[ , 2:19], data_frame) # combine dataframes for calculations
  
  for (i in 37:ncol(cov_data)) {
    
    cov_data[ , i] <- cov_data[ , i - 36] / cov_data[ , i - 18] * 100 # calculate coverage (total dose/ total targeted)
    
  } 
  
  cov_data[ , 37:54] <- round(cov_data[ , 37:54], digits = 2) # round cov values to 2 digits
  
  cov_data <- cov_data[, c(37:54)] # reintroduce # if want df only with cov values
  
  cov_data_names <- dat1[, c(1,20)]
  
  cov_data <- cbind(cov_data_names, cov_data)
  
  return(cov_data)
  
}

estimate_coverage_originaldistr_func2a <- function(dat1, dat2) {
  
  mat <- matrix(NA, nrow = 56, ncol = 19)  # make empty matrix with correct dimensions for cov x district/yesr
  
  data_frame <- data.frame(mat) # turn into dataframe
  
  #names(data_frame) <- paste0("Cov_", seq_along(data_frame)) # rename col names (coverage x year)
  
  names(data_frame) <- paste0("Cov_", 2003:2019) # rename col names (coverage x year) 
  
  data_frame <- data_frame %>%
    add_column(Cov_2009rnd2 = NA,
               .after = "Cov_2009")
  
  data_frame <- data_frame %>%
    add_column(Cov_2010rnd2 = NA,
               .after = "Cov_2010")
  
  temp_2009_dat <- dat2$pop_2009
  
  temp_districtpop_dat <- dat2
  
  temp_districtpop_dat <- temp_districtpop_dat %>%
    add_column(pop2009_rnd2 = temp_2009_dat,
               .after = "pop_2009")
  
  temp_2010_dat <- dat2$pop_2010
  
  temp_districtpop_dat <- temp_districtpop_dat %>%
    add_column(pop2010_rnd2 = temp_2010_dat,
               .after = "pop_2010")
  
  cov_data <- cbind(dat1[ , 2:20], temp_districtpop_dat[ , 2:20], data_frame) # combine dataframes for calculations
  
  for (i in 39:ncol(cov_data)) {
    
    cov_data[ , i] <- cov_data[ , i - 38] / cov_data[ , i - 19] * 100 # calculate coverage (total dose/ total targeted)
    
  } 
  
  cov_data[ , 39:57] <- round(cov_data[ , 39:57], digits = 2) # round cov values to 2 digits
  
  cov_data <- cov_data[, c(39:57)] # reintroduce # if want df only with cov values
  
  cov_data_names <- dat1[, c(1,20)]
  
  cov_data <- cbind(cov_data_names, cov_data)
  
  return(cov_data)
  
}


# TO DO: extend coverage calc this to 2020

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

# TO DO: extend coverage calc this to 2020

estimate_coverage_originaldistr_func3 <- function(dat1, dat2) {
  
  mat <- matrix(NA, nrow = 168, ncol = 17)  # make empty matrix with correct dimensions for cov x district/yesr
  
  data_frame <- data.frame(mat) # turn into dataframe
  
  #names(data_frame) <- paste0("Cov_", seq_along(data_frame)) # rename col names (coverage x year)
  
  names(data_frame) <- paste0("Cov_", 2003:2019) # rename col names (coverage x year) 
  
  data_frame <- data_frame %>%
    add_column(Cov_2009rnd2 = NA,
               .after = "Cov_2009")
  
  cov_data <- cbind(dat1[ , 2:19], dat2[ , 3], data_frame) # combine dataframes for calculations
  
  for (i in 20:ncol(cov_data)) {
    
    cov_data[ , i] <- cov_data[ , i - 19] / cov_data[ , 19] * 100 # calculate coverage (total dose/ total targeted)
    
  } 
  
  cov_data[ , 20:37] <- round(cov_data[ , 20:37], digits = 2) # round cov values to 2 digits
  
  cov_data <- cov_data[, c(20:37)] # reintroduce # if want df only with cov values
  
  cov_data_names <- dat1[, c(1,20)]
  
  cov_data <- cbind(cov_data_names, cov_data)
  
  return(cov_data)
  
}

estimate_coverage_originaldistr_func3a <- function(dat1, dat2) {
  
  mat <- matrix(NA, nrow = 56, ncol = 19)  # make empty matrix with correct dimensions for cov x district/yesr
  
  data_frame <- data.frame(mat) # turn into dataframe
  
  #names(data_frame) <- paste0("Cov_", seq_along(data_frame)) # rename col names (coverage x year)
  
  names(data_frame) <- paste0("Cov_", 2003:2019) # rename col names (coverage x year) 
  
  data_frame <- data_frame %>%
    add_column(Cov_2009rnd2 = NA,
               .after = "Cov_2009")
  
  data_frame <- data_frame %>%
    add_column(Cov_2010rnd2 = NA,
               .after = "Cov_2010")
  
  cov_data <- cbind(dat1[ , 2:20], dat2[ , 3], data_frame[ , 1:19]) # combine dataframes for calculations
  
  for (i in 21:ncol(cov_data)) {
    
    cov_data[ , i] <- cov_data[ , i - 20] / cov_data[ , 20] * 100 # calculate coverage (total dose/ total targeted)
    
  } 
  
  cov_data[ , 21:39] <- round(cov_data[ , 21:39], digits = 2) # round cov values to 2 digits
  
  cov_data <- cov_data[, c(21:39)] # reintroduce # if want df only with cov values
  
  cov_data_names <- dat1[, c(1,20)]
  
  cov_data <- cbind(cov_data_names, cov_data)
  
  return(cov_data)
  
}

