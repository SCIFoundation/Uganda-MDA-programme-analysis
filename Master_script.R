#====================================================================================================================#
#===================================              MASTER SCRIPT            ==========================================#
#====================================================================================================================#

rm(list = ls())

#=============================================#
#       Load data files                       #
#=============================================#

# Load data files (.csv) #

total_doses <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_doses_district&year.csv") # Total PZQ doses by district/year

total_targeted <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_targeted_district&year.csv") # Total targted / target population by district/year

district_pop_year <- read.csv("~/Uganda-MDA-programme-analysis/Data/District_pop_year.csv") # district population numbers from 2020 census / annual pop growth 

district_pop_year_preestimated <- read.csv("~/Uganda-MDA-programme-analysis/Data/District_pop_year_preestimated.csv") # district pop numbers estimated by year 
                                                                                                                      # (where district pop numbers available from SCI databases 
                                                                                                                      # for a given year, included here instead of 
                                                                                                                      # pop based on annual growth change)

districts_2006 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2006 district/UGA_district_boundaries_2006.shp") # need this for 2006


#=======================#                                                                                             
# Initiatie sub-scripts #                                                                                             
source('libraries.R')
source('Estimating_denominators.R')
source('Estimating_coverages.R')
source('Mapping_coverage.R')

#======================================================#
#==========  Denominator estimation ===================#

# 1) # create df with district pop projections x year

district_pop_year <- estimate_district_pop_func(dat = district_pop_year) 

# 2) # create df with max/larget target pop denominator x district/year 

largest_target_pop <- estimate_largest_target_pop_func(dat = total_targeted) 

#======================================================#
#============== Coverage estimation  ==================#


# 1) coverage (total doses / total targeted)

coverage_dataframe1 <- estimate_coverage_func1(dat1 = total_doses, dat2 = total_targeted)

# 2) coverage (total doses / district pop) -  using constant pop growth across years

coverage_dataframe2 <- estimate_coverage_func2(dat1 = total_doses, dat2 = district_pop_year) 

# 3) coverage (total doses / district pop) -  using constant pop growth across years & SCIF numbers

coverage_dataframe3 <- estimate_coverage_func2(dat1 = total_doses, dat2 = district_pop_year_preestimated) 

# 4) coverage (total doses / max target pop) 

coverage_dataframe4 <- estimate_coverage_func3(dat1 = total_doses, dat2 = largest_target_pop) 


#======================================================#
#=========== Plotting/ mapping coverage ===============#

# 1) produce base national map

national_map <- Plot_nationalmap_Uganda_func()
national_map

# 2) overlay districts (covering 2003-2009 period) & create dataframe
district_map_0309 <- UGA_district_boundaries_0309_function(shape_file = districts_2006, 
                                                                 national_map_input = national_map) 

district_names_0309 <- district_name_func(shape_file = districts_2006) 

# 3) Mapping coverage by department x years
# 2003 maps 
MDA_2003_dataframe <- district_MDA_coverage_mapping03_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                     data3 = coverage_dataframe3, data4 = coverage_dataframe4, 
                                                                     district_names_0309 = district_names_0309,
                                                                     district_map_0309 = district_map_0309)

MDA_2003_maps <- plot_UGA_2003_MDA_func(national_map = national_map, MDA_data_03 = MDA_2003_dataframe)
MDA_2003_maps

# 2004
