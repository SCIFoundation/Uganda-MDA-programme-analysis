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

# Load data files (.csv) - first analysis #

# total_doses <- read.csv("~/Uganda-MDA-programme-analysis/Data/first analysis/Total_doses_district_year.csv") # Total PZQ doses by district/year
# 
# total_targeted <- read.csv("~/Uganda-MDA-programme-analysis/Data/first analysis/Total_targeted_district&year.csv") # Total targted / target population by district/year
# 
# district_pop_year <- read.csv("~/Uganda-MDA-programme-analysis/Data/first analysis/District_pop_year.csv") # district population numbers from 2020 census / annual pop growth 
# 
# district_pop_year_preestimated <- read.csv("~/Uganda-MDA-programme-analysis/Data/first analysis/District_pop_year_preestimated.csv") # district pop numbers estimated by year 
                                                                                                                      # (where district pop numbers available from SCI databases 
# ====================================== #                                                                            # for a given year, included here instead of 
# Load data files (.csv) - new districts #

total_doses_newdistricts <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_doses_district_bynewdistricts.csv") # Total PZQ doses by district/year

total_targeted_newdistricts <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_trgt_district_bynewdistricts.csv") # Total targted / target population by district/year

district_pop_newdistricts <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_censuspop_district_bynewdistricts.csv") # district population numbers from 2020 census / annual pop growth 
 

# ================================================= #                                                                     # for a given year, included here instead of 
# Load data files (.csv) - old / original districts #

total_doses_originaldistricts <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_doses_district_byoriginaldistricts.csv") # Total PZQ doses by district/year

total_targeted_originaldistricts <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_trgt_district_byoriginaldistricts.csv") # Total targted / target population by district/year

district_pop_originaldistricts <- read.csv("~/Uganda-MDA-programme-analysis/Data/Total_censuspop_district_byoriginaldistricts.csv") # district population numbers from 2020 census / annual pop growth 


# locking files using lft : https://docs.gitlab.com/ee/user/project/file_lock.html                                                                                                                       # pop based on annual growth change)

#districts_2006 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2006 district/UGA_district_boundaries_2006.shp") # need this for 2006
districts_2001 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp") # need this for 2006


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

coverage_dataframe3 <- estimate_coverage_originaldistr_func3(dat1 = total_doses_originaldistricts, dat2 = largest_target_pop) 


#======================================================#
#=========== Plotting/ mapping coverage ===============#
#======================================================#

# ============================#
# 1) produce base national map

national_map <- Plot_nationalmap_Uganda_func()
national_map

# ====================================================================#
# 2) overlay districts (covering 2003-2019 period) & create dataframe
district_map_0319 <- UGA_district_boundaries_function(shape_file = districts_2001, national_map_input = national_map) 
district_map_0319[[1]]

district_names_0319 <- district_name_func(shape_file = districts_2001) 

# TO DO: extend this to 2020; or produce split maps

# ================================================================#
# 3) Mapping coverage by department x years for ALL DENOMINATORS  #

# 2003 maps #
MDA_2003_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                     data3 = coverage_dataframe3,
                                                                     district_names = district_names_0319,
                                                                     district_map = district_map_0319[[2]],
                                                                     year_input = 2003, age_target = "ALL")

MDA_2003_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2003_dataframe)
MDA_2003_maps

# 2004 #
MDA_2004_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2004, age_target = "ALL")

MDA_2004_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2004_dataframe)
MDA_2004_maps

# 2005 #
MDA_2005_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2005, age_target = "ALL")

MDA_2005_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2005_dataframe)
MDA_2005_maps # two dark grey districts (alpha should be set to 0.01 but not doing this?)

# 2006 #
MDA_2006_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2006, age_target = "ALL")

MDA_2006_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2006_dataframe)
MDA_2006_maps #

# 2007 #
MDA_2007_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2007, age_target = "ALL")

MDA_2007_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2007_dataframe)
MDA_2007_maps #

# 2008 #
MDA_2008_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2008, age_target = "ALL")

MDA_2008_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2008_dataframe)
MDA_2008_maps #

# 2009 #
MDA_2009_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2009, age_target = "ALL")

MDA_2009_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2009_dataframe)
MDA_2009_maps #

# 2009 (round 2)#
MDA_2009rnd2_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = "2009rnd2", age_target = "ALL")

MDA_2009rnd2_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2009rnd2_dataframe)
MDA_2009rnd2_maps #

# 2010 #
MDA_2010_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2010, age_target = "ALL")

MDA_2010_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2010_dataframe)
MDA_2010_maps #

# 2012 #
MDA_2012_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2012, age_target = "ALL")

MDA_2012_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2012_dataframe)
MDA_2012_maps #

# 2013 #
MDA_2013_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2013, age_target = "ALL")

MDA_2013_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2013_dataframe)
MDA_2013_maps #

# 2014 #
MDA_2014_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2014, age_target = "ALL")

MDA_2014_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2014_dataframe)
MDA_2014_maps #

# 2015 #
MDA_2015_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2015, age_target = "ALL")

MDA_2015_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2015_dataframe)
MDA_2015_maps #

# 2016 #
MDA_2016_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2016, age_target = "ALL")

MDA_2016_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2016_dataframe)
MDA_2016_maps #

# 2017 #
MDA_2017_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2017, age_target = "ALL")

MDA_2017_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2017_dataframe)
MDA_2017_maps #

# 2018 #
MDA_2018_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2018, age_target = "ALL")

MDA_2018_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2018_dataframe)
MDA_2018_maps #

# 2019 #
MDA_2019_dataframe <- originaldistrict_MDA_coverage_mapping0319_dataframe_func(data1 = coverage_dataframe1, data2 = coverage_dataframe2, 
                                                                               data3 = coverage_dataframe3,
                                                                               district_names = district_names_0319,
                                                                               district_map = district_map_0319[[2]],
                                                                               year_input = 2019, age_target = "ALL")

MDA_2019_maps <- plot_UGA_MDA_func(national_map = national_map, MDA_data = MDA_2019_dataframe)
MDA_2019_maps #

#=========================#
#       Print maps        #

print_PDF_Cov_maps(map1 = MDA_2003_maps, map2 = MDA_2004_maps, map3 = MDA_2005_maps, map4 = MDA_2006_maps,
                   map5 = MDA_2007_maps, map6 = MDA_2008_maps, map7 = MDA_2009_maps, map8 = MDA_2009rnd2_maps, 
                   map9 = MDA_2010_maps, map10 = MDA_2012_maps, map11 = MDA_2013_maps, map12 = MDA_2014_maps, 
                   map13 = MDA_2015_maps, map14 = MDA_2016_maps, map15 = MDA_2017_maps, map16 = MDA_2018_maps, 
                   map17 = MDA_2019_maps)


# ==============================================================================================================#
# 3) Mapping coverage by department x years for largest targeted denominator (across 03-19) for given district  #

# 2003 maps #
MDA_2003_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2003_dataframe)
MDA_2003_denom3_maps

# 2004 #
MDA_2004_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2004_dataframe)
MDA_2004_denom3_maps

# 2005 #
MDA_2005_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2005_dataframe)
MDA_2005_denom3_maps

# 2006 #
MDA_2006_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2006_dataframe)
MDA_2006_denom3_maps

# 2007 #
MDA_2007_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2007_dataframe)
MDA_2007_denom3_maps

# 2008 #
MDA_2008_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2008_dataframe)
MDA_2008_denom3_maps

# 2009 #
MDA_2009_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2009_dataframe)
MDA_2009_denom3_maps

# 2009 (round 2)#
MDA_2009rnd2_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2009rnd2_dataframe)
MDA_2009_denom3_maps

# 2010 #
MDA_2010_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2010_dataframe)
MDA_2010_denom3_maps

# 2012 #
MDA_2012_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2012_dataframe)
MDA_2012_denom3_maps

# 2013 #
MDA_2013_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2013_dataframe)
MDA_2013_denom3_maps

# 2014 #
MDA_2014_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2014_dataframe)
MDA_2014_denom3_maps

# 2015 #
MDA_2015_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2015_dataframe)
MDA_2015_denom3_maps

# 2016 #
MDA_2016_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2016_dataframe)
MDA_2016_denom3_maps

# 2017 #
MDA_2017_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2017_dataframe)
MDA_2017_denom3_maps

# 2018 #
MDA_2018_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2018_dataframe)
MDA_2018_denom3_maps

# 2019 #
MDA_2019_denom3_maps <- plot_UGA_denominator3_MDA_func(national_map = national_map, MDA_data = MDA_2019_dataframe)
MDA_2019_denom3_maps

print_PDF_Cov_maps2(map1 = MDA_2003_denom3_maps, map2 = MDA_2004_denom3_maps, map3 = MDA_2005_denom3_maps, map4 = MDA_2006_denom3_maps,
                   map5 = MDA_2007_denom3_maps, map6 = MDA_2008_denom3_maps, map7 = MDA_2009_denom3_maps, map8 = MDA_2009rnd2_denom3_maps, 
                   map9 = MDA_2010_denom3_maps, map10 = MDA_2012_denom3_maps, map11 = MDA_2013_denom3_maps, map12 = MDA_2014_denom3_maps, 
                   map13 = MDA_2015_denom3_maps, map14 = MDA_2016_denom3_maps, map15 = MDA_2017_denom3_maps, map16 = MDA_2018_denom3_maps, 
                   map17 = MDA_2019_denom3_maps)

# note: if re-runing this code, make sure you close the PDF first

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
boxplots_cov1
boxplots_cov2 <- plot_boxplot_MDAcov_func(cov_type = "cov2", master_dataframe_cov = master_dataframe_cov2)
boxplots_cov2
boxplots_cov3 <- plot_boxplot_MDAcov_func(cov_type = "cov3", master_dataframe_cov = master_dataframe_cov3)
boxplots_cov3
boxplots_cov4 <- plot_boxplot_MDAcov_func(cov_type = "cov4", master_dataframe_cov = master_dataframe_cov4)
boxplots_cov4

#=========================#
#       Print maps        #

print_PDF_Cov_boxplots(boxplt1 = boxplots_cov1, boxplt2 = boxplots_cov2, boxplt3 = boxplots_cov3, boxplt4 = boxplots_cov4)


