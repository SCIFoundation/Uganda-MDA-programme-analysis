# ================================================================================================================================================ #
#                           MASTER: analysing avg. risk score at sub-county levels based on where PCC studies conducted                            #

#=======================#                                                                                             
# Initiatie sub-scripts #                                                                                             
source('libraries.R')
source('Mapping_coverage.R')
source('Risk_factor_plotting_funcs.R')

#=====================#
#   Data to load      #
districts_2001 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp") # need this for 2006
Uganda_dist <- rgdal::readOGR("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp")

# sub-county shape files up to 2010
subcounties_2010 <- rgdal::readOGR("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2010 sub-counties/Uganda_Subcounties2010.shp") # need this for 2006
subcounties_2010_sp <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2010 sub-counties/Uganda_Subcounties2010.shp") # need this for 2006

# sub-county shape files up to 2010 - 2019
subcounties_2019 <- rgdal::readOGR("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/subcounties_2019/subcounties_2019.shp")
subcounties_2019_sp <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/subcounties_2019/subcounties_2019.shp")

# risk factor spatial data #
load("~/Uganda-MDA-programme-analysis/Data/pig_populationFAO.rdata") # Pig density map (only one available from FAO/GLW 2007 data)

# 2011 - 2015 #
load("~/Uganda-MDA-programme-analysis/Data/sanitation2011.rdata") # 2011 sanitation distribution spatial file
load("~/Uganda-MDA-programme-analysis/Data/poverty_lowest40_2011.rdata") # 2011 poverty distribution spatial file

# 2016 - 2019 #
load("~/Uganda-MDA-programme-analysis/Data/sanitation2016.rdata") # 2011 sanitation distribution spatial file
load("~/Uganda-MDA-programme-analysis/Data/poverty_lowest40_2016.rdata") # 2011 poverty distribution spatial file


# ============================#
# 1) produce base national map

national_map <- Plot_nationalmap_Uganda_func()
#national_map

# ================================================# 
# 2) map sub-counties & obtain sub-county names   #
#=================================================#

# A) for 2001 to 2012 data
UGA_subcounty_object_2004_2012 <- UGA_subcounties_boundaries_function(subcounty_shape_file = subcounties_2010, 
                                                                      district_map = districts_2001, national_map_input = national_map)

UGA_subcounty_object_2004_2012[[1]]

# extract sub-county names for 2003-2009 & 2010-2020 #
subcounties_name_20062010_object <- subcounty_name0412_func(shape_file = UGA_subcounty_object_2004_2012[[2]]) 


#subcounties_2006_names <- subcounties_name_20062010_object[[1]] # for 2003 - 2009 sub-counties 

subcounties_2010_names <- subcounties_name_20062010_object[[2]] # for 2010 sub-counties onwards

# B) for 2013-2019 data
UGA_subcounty_object_2019 <- UGA_subcounties_boundaries_function(subcounty_shape_file = subcounties_2019, 
                                                                 district_map = districts_2001, national_map_input = national_map)

UGA_subcounty_object_2019[[1]]

# extract sub-county names for 2013-2019 #
subcounties_name_2019_object <- subcounty_name19_func(shape_file = UGA_subcounty_object_2019[[2]]) 


subcounties_2019_names <- subcounties_name_2019_object[[1]] # for 2003 - 2009 sub-counties 

#write.csv(x=subcounties_2019_names, "sub_county_list2019.csv")

#=====================================================================================================#
# Extract sub-counties with PCC studies to i) work out average risk & ii) plot over PCC risk factors  #
#=====================================================================================================#

# for 2011 PCC studies #
SC_names_2011 <- subcounties_name_func3(shape_file = subcounties_2010, year = 2011) 

subcounties_PCCstudies_object_2011 <- subcounty_PCCstudies_processing_plotting_func(sc_names = SC_names_2011, 
                                                                      UGA_subcounties_tidy = UGA_subcounty_object_2004_2012[[3]], 
                                                                      district_2001 = district_2001, national_map_input = national_map, 
                                                                      PCC_survey_year = 2011)

subcounties_PCCstudies_object_2011[[3]]

# for 2015 PCC studies #
SC_names_2015 <- subcounties_name_func3(shape_file = subcounties_2019, year = 2015) 

subcounties_PCCstudies_object_2015 <- subcounty_PCCstudies_processing_plotting_func(sc_names = SC_names_2015, 
                                                                      UGA_subcounties_tidy = UGA_subcounty_object_2019[[3]], 
                                                                      district_2001 = district_2001, national_map_input = national_map, 
                                                                      PCC_survey_year = 2015)

subcounties_PCCstudies_object_2015[[3]]

# for 2019 PCC studies #
SC_names_2019 <- subcounties_name_func3(shape_file = subcounties_2019, year = 2019) 

subcounties_PCCstudies_object_2019 <- subcounty_PCCstudies_processing_plotting_func(sc_names = SC_names_2019, 
                                                                      UGA_subcounties_tidy = UGA_subcounty_object_2019[[3]], 
                                                                      district_2001 = district_2001, national_map_input = national_map, 
                                                                      PCC_survey_year = 2019)

subcounties_PCCstudies_object_2019[[3]]

#====================================================================================#
#               Average risk score per sub-county (where PCC studies)                #
#====================================================================================#

# ============================ #
# for 2011 PCC sub-county data #

# first: need to call & produce risk factor overlay 
sanitation_2011 <- sanitation
poverty_lowest40_2011 <- poverty_lowest40

overlay_2011 <- plotting_overlays_func(risk_factor1 = sanitation_2011[[3]], risk_factor2 = pig_populationFAO[[1]], 
                                       risk_factor3 = poverty_lowest40_2011[[3]],
                                       admin_processed = districts_2001, admin = districts_2001, year = "2011-2015") 

# second: calculate avg. risk by sub-county & plot
risk_fact_subcounties_object_2011 <- average_risk_subcounties_func3(RF_data = overlay_2011[[3]], subcounties = subcounties_2010_sp, 
                                                                   subcounty_PCCstudies_data = subcounties_PCCstudies_object_2011[[1]], 
                                                                   scnames = subcounties_PCCstudies_object_2011[[4]], 
                                                                   UGA_subcounties_tidy_subset = subcounties_PCCstudies_object_2011[[5]], 
                                                                   UGA_subcounties_tidy = subcounties_PCCstudies_object_2011[[1]], 
                                                                   UGA_districts_tidy_subset = subcounties_PCCstudies_object_2011[[6]],
                                                                   districts_2001 = districts_2001, year = 2011)

risk_fact_subcounties_object_2011[[1]] # risk factor score for all sub-counties
risk_fact_subcounties_object_2011[[2]] # average risk factor score across sub-counties for each district
risk_fact_subcounties_object_2011[[3]] # plot of sub-counties over risk factor map (sub-county labels)
risk_fact_subcounties_object_2011[[4]] # plot of sub-counties over risk factor map (avg. score across sub-counties per district)

# repeat analysis but exclude any datapoints in sub-counties located in water bodies (i.e. risk score here not counted)
risk_fact_subcounties_nowater_object_2011 <- average_risk_subcounties_func4(data_to_join = risk_fact_subcounties_object_2011[[6]],
                                                                            data_to_join2 = risk_fact_subcounties_object_2011[[7]],
                                                                            RF_data_plotting = overlay_2011[[3]], 
                                                                            UGA_subcounties_tidy_subset = subcounties_PCCstudies_object_2011[[5]], 
                                                                            scnames = subcounties_PCCstudies_object_2011[[4]],
                                                                            year = 2011)

risk_fact_subcounties_nowater_object_2011[[1]]
risk_fact_subcounties_nowater_object_2011[[2]] # map
risk_fact_subcounties_nowater_object_2011[[3]]
risk_fact_subcounties_nowater_object_2011[[4]] # map
risk_fact_subcounties_nowater_object_2011[[5]]


# ============================ #
# for 2015 PCC sub-county data #

# first: need to call & produce risk factor overlay 

sanitation_2011 <- sanitation
poverty_lowest40_2011 <- poverty_lowest40

overlay_2011 <- plotting_overlays_func(risk_factor1 = sanitation_2011[[3]], risk_factor2 = pig_populationFAO[[1]],
                                       risk_factor3 = poverty_lowest40_2011[[3]],
                                       admin_processed = districts_2001, admin = districts_2001, year = "2011-2015")

# second: calculate avg. risk by sub-county & plot

risk_fact_subcounties_object_2015 <- average_risk_subcounties_func3(RF_data = overlay_2011[[3]], subcounties = subcounties_2019_sp, 
                                                                   subcounty_PCCstudies_data = subcounties_PCCstudies_object_2015[[1]], 
                                                                   scnames = subcounties_PCCstudies_object_2015[[4]], 
                                                                   UGA_subcounties_tidy_subset = subcounties_PCCstudies_object_2015[[5]], 
                                                                   UGA_subcounties_tidy = subcounties_PCCstudies_object_2015[[1]],
                                                                   UGA_districts_tidy_subset = subcounties_PCCstudies_object_2015[[6]],
                                                                   districts_2001 = districts_2001, year = 2015)

risk_fact_subcounties_object_2015[[1]] # risk factor score for all sub-counties
risk_fact_subcounties_object_2015[[2]] # average risk factor score across sub-counties for each district
risk_fact_subcounties_object_2015[[3]] # plot of sub-counties over risk factor map (sub-county labels)
risk_fact_subcounties_object_2015[[4]] # plot of sub-counties over risk factor map (avg. score across sub-counties per district)

risk_fact_subcounties_object_2015[[9]]

# repeat analysis but exclude any datapoints in sub-counties located in water bodies (i.e. risk score here not counted)
risk_fact_subcounties_nowater_object_2015 <- average_risk_subcounties_func4(data_to_join = risk_fact_subcounties_object_2015[[6]],
                                                                            data_to_join2 = risk_fact_subcounties_object_2015[[7]],
                                                                            RF_data_plotting = overlay_2011[[3]], 
                                                                            UGA_subcounties_tidy_subset = subcounties_PCCstudies_object_2015[[5]], 
                                                                            scnames = subcounties_PCCstudies_object_2015[[4]],
                                                                            year = 2015)

risk_fact_subcounties_nowater_object_2015[[1]]
risk_fact_subcounties_nowater_object_2015[[2]] # map
risk_fact_subcounties_nowater_object_2015[[3]]
risk_fact_subcounties_nowater_object_2015[[4]] # map
risk_fact_subcounties_nowater_object_2015[[5]]

# ============================ #
# for 2019 PCC sub-county data #

# first: need to call & produce risk factor overlay 


overlay_2016 <- plotting_overlays_func(risk_factor1 = sanitation_2016[[3]], risk_factor2 = pig_populationFAO[[1]],
                                       risk_factor3 = poverty_lowest40_2016[[3]],
                                       admin_processed = districts_2001, admin = districts_2001, year = "2016-2019")

# second: calculate avg. risk by sub-county & plot

risk_fact_subcounties_object_2019 <- average_risk_subcounties_func3(RF_data = overlay_2016[[3]], subcounties = subcounties_2019_sp, 
                                                                   subcounty_PCCstudies_data = subcounties_PCCstudies_object_2019[[1]], 
                                                                   scnames = subcounties_PCCstudies_object_2019[[4]], 
                                                                   UGA_subcounties_tidy_subset = subcounties_PCCstudies_object_2019[[5]], 
                                                                   UGA_subcounties_tidy = subcounties_PCCstudies_object_2019[[1]],
                                                                   UGA_districts_tidy_subset = subcounties_PCCstudies_object_2019[[6]],
                                                                   districts_2001 = districts_2001, year = 2019)

risk_fact_subcounties_object_2019[[1]] # risk factor score for all sub-counties
risk_fact_subcounties_object_2019[[2]] # average risk factor score across sub-counties for each district
risk_fact_subcounties_object_2019[[3]] # plot of sub-counties over risk factor map (sub-county labels)
risk_fact_subcounties_object_2019[[4]] # plot of sub-counties over risk factor map (avg. score across sub-counties per district)

risk_fact_subcounties_object_2019[[9]]

# repeat analysis but exclude any datapoints in sub-counties located in water bodies (i.e. risk score here not counted)
risk_fact_subcounties_nowater_object_2019 <- average_risk_subcounties_func4(data_to_join = risk_fact_subcounties_object_2019[[6]],
                                                                            data_to_join2 = risk_fact_subcounties_object_2019[[7]],
                                                                            RF_data_plotting = overlay_2016[[3]], 
                                                                            UGA_subcounties_tidy_subset = subcounties_PCCstudies_object_2019[[5]], 
                                                                            scnames = subcounties_PCCstudies_object_2019[[4]],
                                                                            year = 2019)

risk_fact_subcounties_nowater_object_2019[[1]]
risk_fact_subcounties_nowater_object_2019[[2]] # map
risk_fact_subcounties_nowater_object_2019[[3]]
risk_fact_subcounties_nowater_object_2019[[4]] # map
risk_fact_subcounties_nowater_object_2019[[5]]

