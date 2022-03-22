# ================================================================================================================================================ #
#                           MASTER: analysing MDA location by sub-counties & avg. risk score at sub-county levels                                  #

#=======================#                                                                                             
# Initiatie sub-scripts #                                                                                             
source('libraries.R')
source('Mapping_coverage.R')
source('Risk_factor_plotting_funcs.R')

#=====================#
#   Data to load      #
districts_2001 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp") # need this for 2006
Uganda_dist <- rgdal::readOGR("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp")

subcounties_2010 <- rgdal::readOGR("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2010 sub-counties/Uganda_Subcounties2010.shp") # need this for 2006
subcounties_2010_sp <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2010 sub-counties/Uganda_Subcounties2010.shp") # need this for 2006

# manually located lat/lon data for subc-ounty locations (point data)
Sub_district_MDA_locations_2001_2005_df <- read.csv("") 
Sub_district_MDA_locations_2011_2015_df <- read.csv("") 

# risk factor spatial data #
load("~/Uganda-MDA-programme-analysis/Data/pig_populationFAO.rdata") # Pig density map (only one available from FAO/GLW 2007 data)

# 2001 - 2005 #
load("~/Uganda-MDA-programme-analysis/Data/sanitation2001.rdata") # 2001 sanitation distribution spatial file
load("~/Uganda-MDA-programme-analysis/Data/poverty_lowest40_2001.rdata") # 2001 poverty distribution spatial file

# 2011 - 2015 #
load("~/Uganda-MDA-programme-analysis/Data/sanitation2011.rdata") # 2011 sanitation distribution spatial file
load("~/Uganda-MDA-programme-analysis/Data/poverty_lowest40_2011.rdata") # 2011 poverty distribution spatial file


# ============================#
# 1) produce base national map

national_map <- Plot_nationalmap_Uganda_func()
#national_map

# ================================================# 
# 2) map sub-counties & obtain sub-county names   #
#=================================================#

UGA_subcounty_object <- UGA_subcounties_boundaries_function(subcounty_shape_file = subcounties_2010, 
                                                            district_map = districts_2001, national_map_input = national_map)

UGA_subcounty_object[[1]]

# extract sub-county names for 2003-2009 & 2010-2020 #
subcounties_name_20062010_object <- subcounty_name_func(shape_file = UGA_subcounty_object[[2]]) 


subcounties_2006_names <- subcounties_name_20062010_object[[1]] # for 2003 - 2009 sub-counties 

subcounties_2010_names <- subcounties_name_20062010_object[[2]] # for 2010 sub-counties onwards

#=============================================================================================#
# Extract sub-counties with MDA to i) work out average risk & ii) plot over PCC risk factors  #
#=============================================================================================#

# for 2004 #
SC_names_2006 <- subcounties_name_func2(shape_file = subcounties_2010, year = 2004) 


subcounties_MDA_object <- subcounty_MDA_processing_plotting_func(sc_names = SC_names_2006, UGA_subcounties_tidy = UGA_subcounty_object[[3]], 
                                       district_2001 = district_2001, national_map_input = national_map, year = 2004)

subcounties_MDA_object[[3]]

# for > 2010 #
SC_names_2010 <- subcounties_name_func2(shape_file = subcounties_2010, year = 2012) 

subcounties_MDA_object <- subcounty_MDA_processing_plotting_func(sc_names = SC_names_2010, UGA_subcounties_tidy = UGA_subcounty_object[[3]], 
                                                                 district_2001 = district_2001, national_map_input = national_map, year = 2012)

subcounties_MDA_object[[3]]

#====================================================================================#
#               Average risk score per sub-county                                    #
#====================================================================================#

# ============================ #
# for 2004 MDA sub-county data #

# first: need to call & produce risk factor overlay 
overlay_2001 <- plotting_overlays_func(risk_factor1 = sanitation_2001[[3]], risk_factor2 = pig_populationFAO[[1]], 
                                       risk_factor3 = poverty_lowest40_2001[[3]],
                                       admin_processed = districts_2001, admin = districts_2001, year = "2001-2005") # for 2001-2005

# second: calculate avg. risk by sub-county & plot
risk_fact_subcounties_object <- average_risk_subcounties_func(RF_data = overlay_2001[[3]], subcounties_2010 = subcounties_2010_sp, 
                              subcounty_MDA_data = subcounties_MDA_object[[1]], scnames = subcounties_MDA_object[[4]], 
                              UGA_subcounties_tidy_subset = subcounties_MDA_object[[5]], 
                              UGA_subcounties_tidy = subcounties_MDA_object[[1]], 
                              districts_2001 = districts_2001, year = 2004)

risk_fact_subcounties_object[[1]]
risk_fact_subcounties_object[[2]]

# repeat analysis but exclude any datapoints in sub-counties located in water bodies (i.e. risk score here not counted)
risk_fact_subcounties_nowater_object <- average_risk_subcounties_func2(data_to_join = risk_fact_subcounties_object[[6]],
                                                                       data_to_join2 = risk_fact_subcounties_object[[7]],
                                                                       RF_data_plotting = overlay_2001[[3]], 
                                                                       UGA_subcounties_tidy = subcounties_MDA_object[[1]], 
                                                                       scnames = subcounties_MDA_object[[4]],
                                                                       year = 2004)

risk_fact_subcounties_nowater_object[[1]]
risk_fact_subcounties_nowater_object[[2]]
risk_fact_subcounties_nowater_object[[3]]
risk_fact_subcounties_nowater_object[[4]]
risk_fact_subcounties_nowater_object[[5]]

# ============================ #
# for 2012 MDA sub-county data #

# first: need to call & produce risk factor overlay 
sanitation_2011 <- sanitation
poverty_lowest40_2011 <- poverty_lowest40

overlay_2011 <- plotting_overlays_func(risk_factor1 = sanitation_2011[[3]], risk_factor2 = pig_populationFAO[[1]], 
                                       risk_factor3 = poverty_lowest40_2011[[3]],
                                       admin_processed = districts_2001, admin = districts_2001, year = "2011-2015") 

# second: calculate avg. risk by sub-county & plot
risk_fact_subcounties_object <- average_risk_subcounties_func(RF_data = overlay_2011[[3]], subcounties_2010 = subcounties_2010_sp, 
                                                              subcounty_MDA_data = subcounties_MDA_object[[1]], scnames = subcounties_MDA_object[[4]], 
                                                              UGA_subcounties_tidy_subset = subcounties_MDA_object[[5]], 
                                                              UGA_subcounties_tidy = subcounties_MDA_object[[1]], 
                                                              districts_2001 = districts_2001, year = 2012)

risk_fact_subcounties_object[[1]] # risk factor score for all sub-counties
risk_fact_subcounties_object[[2]] # average risk factor score across sub-counties for each district
risk_fact_subcounties_object[[3]] # plot of sub-counties over risk factor map (sub-county labels)
risk_fact_subcounties_object[[4]] # plot of sub-counties over risk factor map (avg. score across sub-counties per district)

# repeat analysis but exclude any datapoints in sub-counties located in water bodies (i.e. risk score here not counted)
risk_fact_subcounties_nowater_object <- average_risk_subcounties_func2(data_to_join = risk_fact_subcounties_object[[6]],
                                                                       data_to_join2 = risk_fact_subcounties_object[[7]],
                                                                       RF_data_plotting = overlay_2011[[3]], 
                                                                       UGA_subcounties_tidy = subcounties_MDA_object[[1]], 
                                                                       scnames = subcounties_MDA_object[[4]],
                                                                       year = 2012)

risk_fact_subcounties_nowater_object[[1]]
risk_fact_subcounties_nowater_object[[2]]
risk_fact_subcounties_nowater_object[[3]]
risk_fact_subcounties_nowater_object[[4]]
risk_fact_subcounties_nowater_object[[5]]
