#====================================================================================================================#
#===================================              MASTER SCRIPT            ==========================================#
#====================================================================================================================#

rm(list = ls())

#=============================================#
#       1) COVERAGE ESTIMATION                #
#=============================================#

# Load data files (.csv) #

total_doses <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_doses_district&year.csv") # Total PZQ doses by district/year

total_targeted <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_targeted_district&year.csv") # Total targted / target population by district/year

district_pop_year <- read.csv("~/Uganda-MDA-programme-analysis/Data/District_pop_year.csv") # district population numbers from 2020 census / annual pop growth 

district_pop_year_preestimated <- read.csv("~/Uganda-MDA-programme-analysis/Data/District_pop_year_preestimated.csv") # district pop numbers estimated by year 
                                                                                                                      # (where district pop numbers available from SCI databases 
#=======================#                                                                                                                      # for a given year, included here instead of 
# Initiatie sub-scripts #                                                                                                                     # pop based on annual growth change)

source('libraries.R')
source('Estimating_denominators.R')
source('Estimating_coverages.R')


#======================================================#
#              Denominator estimation                  #

# 1) # create df with district pop projections x year

district_pop_year <- estimate_district_pop_func(dat = district_pop_year) 

# 2) # create df with max/larget target pop denominator x district/year 

largest_target_pop <- estimate_largest_target_pop_func(dat = total_targeted) 

#======================================================#
#               Coverage estimation                    #


# 1) coverage (total doses / total targeted)

coverage_dataframe1 <- estimate_coverage_func1(dat1 = total_doses, dat2 = total_targeted)

# 2) coverage (total doses / district pop) -  using constant pop growth across years

coverage_dataframe2 <- estimate_coverage_func2(dat1 = total_doses, dat2 = district_pop_year) 

# 3) coverage (total doses / district pop) -  using constant pop growth across years & SCIF numbers

coverage_dataframe3 <- estimate_coverage_func2(dat1 = total_doses, dat2 = district_pop_year_preestimated) 

# 4) coverage (total doses / max target pop) 

coverage_dataframe4 <- estimate_coverage_func3(dat1 = total_doses, dat2 = largest_target_pop) 
