#====================================================================================================================#
#===================================              MASTER SCRIPT            ==========================================#
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
source('Mapping_coverage.R')
source('Plotting_coveragebyyear_boxplots.R')
source('Produce_outputs.R')

# Load data files (.csv) #

total_doses <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_doses_district_year.csv") # Total PZQ doses by district/year

total_targeted <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_targeted_district&year.csv") # Total targted / target population by district/year

district_pop_year <- read.csv("~/Uganda-MDA-programme-analysis/Data/District_pop_year.csv") # district population numbers from 2020 census / annual pop growth 

district_pop_year_preestimated <- read.csv("~/Uganda-MDA-programme-analysis/Data/District_pop_year_preestimated.csv") # district pop numbers estimated by year 
                                                                                                                      # (where district pop numbers available from SCI databases 
                                                                                                                      # for a given year, included here instead of 
# locking files using lft : https://docs.gitlab.com/ee/user/project/file_lock.html                                                                                                                       # pop based on annual growth change)

districts_2006 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2006 district/UGA_district_boundaries_2006.shp") # need this for 2006


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
#======================================================#

# ============================#
# 1) produce base national map

national_map <- Plot_nationalmap_Uganda_func()
national_map

# ====================================================================#
# 2) overlay districts (covering 2003-2009 period) & create dataframe
district_map_0309 <- UGA_district_boundaries_0309_function(shape_file = districts_2006, 
                                                                 national_map_input = national_map) 

district_names_0309 <- district_name_func(shape_file = districts_2006) 

# =========================================#
# 3) Mapping coverage by department x years

# 2003 maps #
MDA_2003_dataframe <- district_MDA_coverage_mapping0309_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                     data3 = coverage_dataframe3, data4 = coverage_dataframe4, 
                                                                     district_names_0309 = district_names_0309,
                                                                     district_map_0309 = district_map_0309,
                                                                     year_input = 2003)

MDA_2003_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2003_dataframe)
MDA_2003_maps

# 2004 #
MDA_2004_dataframe <- district_MDA_coverage_mapping0309_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                       data3 = coverage_dataframe3, data4 = coverage_dataframe4, 
                                                                       district_names_0309 = district_names_0309,
                                                                       district_map_0309 = district_map_0309,
                                                                       year_input = 2004)

MDA_2004_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2004_dataframe)
MDA_2004_maps

# 2005 #
MDA_2005_dataframe <- district_MDA_coverage_mapping0309_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                       data3 = coverage_dataframe3, data4 = coverage_dataframe4, 
                                                                       district_names_0309 = district_names_0309,
                                                                       district_map_0309 = district_map_0309,
                                                                       year_input = 2005)

MDA_2005_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2005_dataframe)
MDA_2005_maps # two dark grey districts (alpha should be set to 0.01 but not doing this?)

# 2006 #
MDA_2006_dataframe <- district_MDA_coverage_mapping0309_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                       data3 = coverage_dataframe3, data4 = coverage_dataframe4, 
                                                                       district_names_0309 = district_names_0309,
                                                                       district_map_0309 = district_map_0309,
                                                                       year_input = 2006)

MDA_2006_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2006_dataframe)
MDA_2006_maps #

# 2007 #
MDA_2007_dataframe <- district_MDA_coverage_mapping0309_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                       data3 = coverage_dataframe3, data4 = coverage_dataframe4, 
                                                                       district_names_0309 = district_names_0309,
                                                                       district_map_0309 = district_map_0309,
                                                                       year_input = 2007)

MDA_2007_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2007_dataframe)
MDA_2007_maps #

# 2008 #
MDA_2008_dataframe <- district_MDA_coverage_mapping0309_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                       data3 = coverage_dataframe3, data4 = coverage_dataframe4, 
                                                                       district_names_0309 = district_names_0309,
                                                                       district_map_0309 = district_map_0309,
                                                                       year_input = 2008)

MDA_2008_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2008_dataframe)
MDA_2008_maps #

# 2009 #
MDA_2009_dataframe <- district_MDA_coverage_mapping0309_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                       data3 = coverage_dataframe3, data4 = coverage_dataframe4, 
                                                                       district_names_0309 = district_names_0309,
                                                                       district_map_0309 = district_map_0309,
                                                                       year_input = 2009)

MDA_2009_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2009_dataframe)
MDA_2009_maps #


#=========================#
#       Print maps        #

print_PDF_Cov_maps(map1 = MDA_2003_maps, map2 = MDA_2004_maps, map3 = MDA_2005_maps, map4 = MDA_2006_maps,
                   map5 = MDA_2007_maps, map6 = MDA_2008_maps, map7 = MDA_2009_maps)


#================================================================#
#=========== Plotting coverage by year (boxplots) ===============#
#================================================================#

# ================================#
#   Make dataframes for plotting  #

master_dataframe_cov1 <- create_dataframe_plotting_coverageboxplots_func(cov_type = "cov1",
                                                                         coverage_dataframe1 = coverage_dataframe1, 
                                                                         coverage_dataframe2 = coverage_dataframe2,
                                                                         coverage_dataframe3 = coverage_dataframe3, 
                                                                         coverage_dataframe4 = coverage_dataframe4)

master_dataframe_cov2 <- create_dataframe_plotting_coverageboxplots_func(cov_type = "cov2",
                                                                         coverage_dataframe1 = coverage_dataframe1, 
                                                                         coverage_dataframe2 = coverage_dataframe2,
                                                                         coverage_dataframe3 = coverage_dataframe3, 
                                                                         coverage_dataframe4 = coverage_dataframe4)

master_dataframe_cov3 <- create_dataframe_plotting_coverageboxplots_func(cov_type = "cov3",
                                                                         coverage_dataframe1 = coverage_dataframe1, 
                                                                         coverage_dataframe2 = coverage_dataframe2,
                                                                         coverage_dataframe3 = coverage_dataframe3, 
                                                                         coverage_dataframe4 = coverage_dataframe4)

master_dataframe_cov4 <- create_dataframe_plotting_coverageboxplots_func(cov_type = "cov4",
                                                                         coverage_dataframe1 = coverage_dataframe1, 
                                                                         coverage_dataframe2 = coverage_dataframe2,
                                                                         coverage_dataframe3 = coverage_dataframe3, 
                                                                         coverage_dataframe4 = coverage_dataframe4)

#===================================#
#    plot for each type of coverage #

boxplots_cov1 <- plot_boxplot_MDAcov_func(cov_type = "cov1", master_dataframe_cov = master_dataframe_cov1)
boxplots_cov2 <- plot_boxplot_MDAcov_func(cov_type = "cov2", master_dataframe_cov = master_dataframe_cov2)
boxplots_cov3 <- plot_boxplot_MDAcov_func(cov_type = "cov3", master_dataframe_cov = master_dataframe_cov3)
boxplots_cov4 <- plot_boxplot_MDAcov_func(cov_type = "cov4", master_dataframe_cov = master_dataframe_cov4)

#=========================#
#       Print maps        #

print_PDF_Cov_boxplots(boxplt1 = boxplots_cov1, boxplt2 = boxplots_cov2, boxplt3 = boxplots_cov3, boxplt4 = boxplots_cov4)


