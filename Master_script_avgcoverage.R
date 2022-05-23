#====================================================================================================================#
#===================================    MASTER SCRIPT - average coverage   ==========================================#
#====================================================================================================================#

rm(list = ls())

#=============================================#
#       Load data files                       #
#=============================================#

#=======================#                                                                                             
# Initiatie sub-scripts #                                                                                             
source('libraries.R')
source('Estimating_denominators.R')
source('Estimating_coverages.R')
# source('Mapping_coverage.R')
# source('Plotting_coveragebyyear_boxplots.R')
# source('Produce_outputs.R')

# Load data files (.csv) - first analysis #

# total_doses <- read.csv("~/Uganda-MDA-programme-analysis/Data/first analysis/Total_doses_district_year.csv") # Total PZQ doses by district/year
# 
# total_targeted <- read.csv("~/Uganda-MDA-programme-analysis/Data/first analysis/Total_targeted_district&year.csv") # Total targted / target population by district/year
# 
# district_pop_year <- read.csv("~/Uganda-MDA-programme-analysis/Data/first analysis/District_pop_year.csv") # district population numbers from 2020 census / annual pop growth 
# 
# district_pop_year_preestimated <- read.csv("~/Uganda-MDA-programme-analysis/Data/first analysis/District_pop_year_preestimated.csv") # district pop numbers estimated by year 
# (where district pop numbers available from SCI databases 


# ================================================= #                                                                     # for a given year, included here instead of 
# Load data files (.csv) - old / original districts #

total_doses_originaldistricts <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_doses_district_byoriginaldistricts_V2.csv") # Total PZQ doses by district/year

total_targeted_originaldistricts <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_trgt_district_byoriginaldistricts_V2.csv") # Total targted / target population by district/year

district_pop_originaldistricts <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_censuspop_district_byoriginaldistricts.csv") # district population numbers from 2020 census / annual pop growth 


# locking files using lft : https://docs.gitlab.com/ee/user/project/file_lock.html                                                                                                                       # pop based on annual growth change)

#districts_2006 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2006 district/UGA_district_boundaries_2006.shp") # need this for 2006
# districts_2001 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp") # need this for 2006


#======================================================#
#   ANALYSIS BY ORIGINAL DISTRICTS                     #
#======================================================#


#======================================================#
#==========  Denominator estimation ===================#

# 1) # create df with district pop projections x year

# district_pop_year <- estimate_district_pop_func(dat = district_pop_year) # not required as pre-estimated now


# 2) # create df with max/larget target pop denominator x district/year 

largest_target_pop <- estimate_largest_target_pop_func(dat = total_targeted_originaldistricts) 

#======================================================#
#============== Coverage estimation  ==================#


# 1) coverage (total doses / total targeted)

coverage_dataframe1 <- estimate_coverage_originaldistr_func1(dat1 = total_doses_originaldistricts, dat2 = total_targeted_originaldistricts)

# 2) coverage (total doses / district pop) -  using constant pop growth across years

coverage_dataframe2 <- estimate_coverage_originaldistr_func2(dat1 = total_doses_originaldistricts, dat2 = district_pop_originaldistricts) 

# 3) coverage (total doses / largest target pop) 

coverage_dataframe3 <- estimate_coverage_originaldistr_func3a(dat1 = total_doses_originaldistricts, dat2 = largest_target_pop) 

coverage_dataframe3$avgCov <- apply(coverage_dataframe3[,3:21], 1, function(x) { mean(x, na.rm=TRUE) })
coverage_dataframe3$avgCov2 <- apply(coverage_dataframe3[,3:21], 1, function(x) { mean(replace(x, x>100, NA), na.rm=TRUE) }) # avg cov if remove cov's > 100%
#write.csv(coverage_dataframe3, file="Cov_allages.csv")

# ======================================================================================================================#
#======================================================#
#   ANALYSIS BY ORIGINAL DISTRICTS  (by age group)     #
#======================================================#

total_doses_originaldistricts$Age <- as.factor(total_doses_originaldistricts$Age)
total_targeted_originaldistricts$Age <- as.factor(total_targeted_originaldistricts$Age)
district_pop_originaldistricts$Age <- as.factor(district_pop_originaldistricts$Age)

#======================================================#
#      By SAC                                          #

# ================================================= #                                                                     # for a given year, included here instead of 
# Load data files (.csv) - old / original districts #

total_dosesSAC_originaldistricts <- subset(total_doses_originaldistricts, Age == "SAC", select=District:PZQ_2019)

total_targetedSAC_originaldistricts <- subset(total_targeted_originaldistricts, Age == "SAC", select=District:trgt_2019)

district_popSAC_originaldistricts <- subset(district_pop_originaldistricts, Age == "SAC", select=District:pop_2019)


#======================================================#
#==========  Denominator estimation ===================#

# 1) # create df with district pop projections x year

# district_pop_year <- estimate_district_pop_func(dat = district_pop_year) # not required as pre-estimated now


# 2) # create df with max/larget target pop denominator x district/year 

largest_target_popSAC <- estimate_largest_target_pop_func(dat = total_targetedSAC_originaldistricts) 

#======================================================#
#============== Coverage estimation  ==================#


# 1) coverage (total doses / total targeted)

coverage_dataframe_SAC1 <- estimate_coverage_originaldistr_func1a(dat1 = total_dosesSAC_originaldistricts, dat2 = total_targetedSAC_originaldistricts)

# 2) coverage (total doses / district pop) -  using constant pop growth across years

coverage_dataframe_SAC2 <- estimate_coverage_originaldistr_func2a(dat1 = total_dosesSAC_originaldistricts, dat2 = district_popSAC_originaldistricts) 

# 3) coverage (total doses / largest target pop) 

coverage_dataframe_SAC3 <- estimate_coverage_originaldistr_func3a(dat1 = total_dosesSAC_originaldistricts, dat2 = largest_target_popSAC) 

coverage_dataframe_SAC3$avgCov <- apply(coverage_dataframe_SAC3[,3:21], 1, function(x) { mean(x, na.rm=TRUE) })
coverage_dataframe_SAC3$avgCov2 <- apply(coverage_dataframe_SAC3[,3:21], 1, function(x) { mean(replace(x, x>100, NA), na.rm=TRUE) }) # avg cov if remove cov's > 100%
#write.csv(coverage_dataframe_SAC3, file="Cov_SACages.csv")

#======================================================#
#      By non-SAC                                      #

# ================================================= #                                                                     # for a given year, included here instead of 
# Load data files (.csv) - old / original districts #

total_doses_nonSAC_originaldistricts <- subset(total_doses_originaldistricts, Age == "non-SAC", select=District:PZQ_2019)

total_targeted_nonSAC_originaldistricts <- subset(total_targeted_originaldistricts, Age == "non-SAC", select=District:trgt_2019)

district_pop_nonSAC_originaldistricts <- subset(district_pop_originaldistricts, Age == "non-SAC", select=District:pop_2019)

#======================================================#
#==========  Denominator estimation ===================#

# 1) # create df with district pop projections x year

# district_pop_year <- estimate_district_pop_func(dat = district_pop_year) # not required as pre-estimated now


# 2) # create df with max/larget target pop denominator x district/year 

largest_target_popnonSAC <- estimate_largest_target_pop_func(dat = total_targeted_nonSAC_originaldistricts) 

#======================================================#
#============== Coverage estimation  ==================#


# 1) coverage (total doses / total targeted)

coverage_dataframe_nonSAC1 <- estimate_coverage_originaldistr_func1a(dat1 = total_doses_nonSAC_originaldistricts, dat2 = total_targeted_nonSAC_originaldistricts)

# 2) coverage (total doses / district pop) -  using constant pop growth across years

coverage_dataframe_nonSAC2 <- estimate_coverage_originaldistr_func2a(dat1 = total_doses_nonSAC_originaldistricts, dat2 = district_pop_nonSAC_originaldistricts) 

# 3) coverage (total doses / largest target pop) 

coverage_dataframe_nonSAC3 <- estimate_coverage_originaldistr_func3a(dat1 = total_doses_nonSAC_originaldistricts, dat2 = largest_target_popnonSAC) 

coverage_dataframe_nonSAC3$avgCov <- apply(coverage_dataframe_nonSAC3[,3:21], 1, function(x) { mean(x, na.rm=TRUE) })
coverage_dataframe_nonSAC3$avgCov2 <- apply(coverage_dataframe_nonSAC3[,3:21], 1, function(x) { mean(replace(x, x>100, NA), na.rm=TRUE) }) # avg cov if remove cov's > 100%
#write.csv(coverage_dataframe_nonSAC3, file="Cov_nonSACages.csv")
