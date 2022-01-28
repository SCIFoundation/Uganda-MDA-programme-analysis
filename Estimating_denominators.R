#===================================================================================================================#
#=====================================    Estimating denominator functions =========================================#

# function 1 - estimating district pop numbers using pop growth estimates

estimate_district_pop_func <- function(dat) {

  for (i in 5:ncol(dat)) {

    dat[ , i] <- dat[ , i - 1] + (dat[ , i - 1] * dat[ , 3]) # multiply new disitrct pop by previous years pop * annual pop growth
    
  }

  dat[ , 5:11] <- round(dat[ , 5:11], digits = 0) # round to integer
  
  return(dat)

}


# function 2 - finding max/largest target pop for each district across years

estimate_largest_target_pop_func <- function(dat) {
  
  #dat$Age <- as.factor(dat$Age)
  
  #dat <- subset(dat, Age == "ALL")
  
  data <- dat[ , c(1,20)]
  
  data[, "largest_target_pop"] <- apply(dat[, 2:19], 1, hablar::max_) # find max/largest target pop for each row (distirct) across years
  
  data$largest_target_pop <- round(data$largest_target_pop, digits = 0)
    
  return(data)
  
}


