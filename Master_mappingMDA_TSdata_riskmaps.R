#===================================================================================================================================#
#                      Analysing Uganda TS data/ risk factor data in context of district-level MDA history                          #
#===================================================================================================================================#

rm(list = ls())

#=======================#                                                                                             
# Initiatie sub-scripts #                             
source('libraries.R')
source('Risk_factor_plotting_funcs.R')
source('Plotting_UGA_MDA_TSdata.R')

#=============================================#
#       Load data files                       #
#=============================================#

# 1) data for TS data x year when MDA first started
UGA_MDA_PCCprev_data <- read.csv("~/Uganda-MDA-programme-analysis/Data/PCC_prev_MDAyr_data2.csv") 
view(UGA_MDA_PCCprev_data)

# 2) risk factor data: these maps and spatial files have been generated through another R project:
# https://github.com/SCIFoundation/Uganda_porcine_cysticercosis_risk_mapping
districts_2001 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp") # need this for 2006
subcounties_2002 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/Ug_Subcounty-boundaries2002/Ug_Subcounty-boundaries2002.shp") # need this for 2006
Uganda_dist <- rgdal::readOGR("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp")
load("~/Uganda-MDA-programme-analysis/Data/pig_populationFAO.rdata") # Pig density map (only one available from FAO/GLW 2007 data)

# 2011 risk factor spatial data#
load("~/Uganda-MDA-programme-analysis/Data/sanitation2011.rdata") # 2011 sanitation distribution spatial file
load("~/Uganda-MDA-programme-analysis/Data/poverty_lowest40_2011.rdata") # 2011 poverty distribution spatial file

# 2001 risk factor spatial data#
load("~/Uganda-MDA-programme-analysis/Data/sanitation2001.rdata") # 2011 sanitation distribution spatial file
load("~/Uganda-MDA-programme-analysis/Data/poverty_lowest40_2001.rdata") # 2011 poverty distribution spatial file

# 2006 risk factor spatial data#
load("~/Uganda-MDA-programme-analysis/Data/sanitation2006.rdata") # 2011 sanitation distribution spatial file
load("~/Uganda-MDA-programme-analysis/Data/poverty_lowest40_2006.rdata") # 2011 poverty distribution spatial file

# 2016 risk factor spatial data#
load("~/Uganda-MDA-programme-analysis/Data/sanitation2016.rdata") # 2011 sanitation distribution spatial file
load("~/Uganda-MDA-programme-analysis/Data/poverty_lowest40_2016.rdata") # 2011 poverty distribution spatial file

UGA_water <- readShapePoly('~/Uganda-MDA-programme-analysis/Data/Uganda shape files/water bodies/fh022bz4757.shp')

# sub-district MDA mapping (add in each time)


# ==========================================================#
# 1) Process & plot UGA TS data with year MDA first started #

Output <- Processing_plotting_UGA_MDA_TSdata_func(data = UGA_MDA_PCCprev_data)

Output[[1]]

#==========================================================#
# 2)      Plot risk factor maps x 2002 disticts            #

#=====================================#
#               2011                  #

#=====================================#
#  2a) plot risk factor map    #

sanitation_2011 <- sanitation
poverty_lowest40_2011 <- poverty_lowest40

overlay_2011 <- plotting_overlays_func(risk_factor1 = sanitation_2011[[3]], risk_factor2 = pig_populationFAO[[1]], 
                                       risk_factor3 = poverty_lowest40_2011[[3]],
                                        admin_processed = districts_2001, admin = districts_2001, year = "2011") 
overlay_2011[[2]]

#=================================================#
# 2b) Calculating average risk score per district #

PCC_average_riskscores_distr_2011_df <- calculate_risk_scores_districts_func(data = overlay_2011[[3]], year = "2011")

Avg.risk_newdistr_2011 <- PCC_average_riskscores_distr_2011_df[[1]]
Avg.risk_originaldistr_2011 <- PCC_average_riskscores_distr_2011_df[[2]]

#View(Avg.risk_newdistr_2011) # average PCC risk scores for all new districts
#View(Avg.risk_originaldistr_2011) # average PCC risk scores for original districts (pre-2003)

#write.csv(Avg.risk_originaldistr_2011, file="Avg.risk_originaldistr_2011.csv")

#=============================================== #
#   Make district centorids &  risk zones labels #

avg.risk.map_2011 <- plot_UGA_avg.risk.zones_func(Uganda_dist = Uganda_dist, 
                                                  risk_overlay = Avg.risk_originaldistr_2011, 
                                                  risk_map = overlay_2011[[2]])

plot(avg.risk.map_2011[[1]]) # mean risk scores not useful
plot(avg.risk.map_2011[[2]]) # therefore only plot modal risk scores

#======================================================================#
# 2c) Calculating average risk score per district (minus water bodies) #

PCC_average_riskscores_distr_2011_nowater_df <- calculate_risk_scores_districts_func2(data = overlay_2011[[3]], year = "2011",
                                                                              data_to_join = PCC_average_riskscores_distr_2011_df[[3]])

Avg.risk_newdistr_nowater_2011 <- PCC_average_riskscores_distr_2011_nowater_df[[1]]
Avg.risk_originaldistr_nowater_2011 <- PCC_average_riskscores_distr_2011_nowater_df[[2]]

#write.csv(Avg.risk_originaldistr_nowater_2011, file="Avg.risk_originaldistr_nowater_2011.csv")

#=============================================== #
#   Make district centorids &  risk zones labels #

avg.risk.map.water_2011 <- plot_UGA_avg.risk.zones_func2(Uganda_dist = Uganda_dist, 
                                                  risk_overlay = Avg.risk_originaldistr_nowater_2011, 
                                                  risk_map = overlay_2011[[2]])

plot(avg.risk.map.water_2011[[1]]) # mean risk scores not useful
plot(avg.risk.map.water_2011[[2]]) # therefore only plot modal risk scores

#=====================================#
#               2001                  #

#=====================================#
#  2a) plot risk factor map    #

# sanitation_2001
# poverty_lowest40_2001

overlay_2001 <- plotting_overlays_func(risk_factor1 = sanitation_2001[[3]], risk_factor2 = pig_populationFAO[[1]], 
                                       risk_factor3 = poverty_lowest40_2001[[3]],
                                       admin_processed = districts_2001, admin = districts_2001, year = "2001") 
overlay_2001[[2]]

#=================================================#
# 2b) Calculating average risk score per district #

PCC_average_riskscores_distr_2001_df <- calculate_risk_scores_districts_func(data = overlay_2001[[3]], year = "2001")

Avg.risk_newdistr_2001 <- PCC_average_riskscores_distr_2001_df[[1]]
Avg.risk_originaldistr_2001 <- PCC_average_riskscores_distr_2001_df[[2]]

#View(Avg.risk_newdistr_2001) # average PCC risk scores for all new districts
#View(Avg.risk_originaldistr_2001) # average PCC risk scores for original districts (pre-2003)

#write.csv(Avg.risk_originaldistr_2001, file="Avg.risk_originaldistr_2001.csv")

#=============================================== #
#   Make district centorids &  risk zones labels #

avg.risk.map_2001 <- plot_UGA_avg.risk.zones_func(Uganda_dist = Uganda_dist, 
                                                  risk_overlay = Avg.risk_originaldistr_2001, 
                                                  risk_map = overlay_2001[[2]])

plot(avg.risk.map_2001[[1]]) # mean risk scores not useful
plot(avg.risk.map_2001[[2]]) # therefore only plot modal risk scores

#======================================================================#
# 2c) Calculating average risk score per district (minus water bodies) #

PCC_average_riskscores_distr_2001_nowater_df <- calculate_risk_scores_districts_func2(data = overlay_2001[[3]], year = "2001",
                                                                                      data_to_join = PCC_average_riskscores_distr_2001_df[[3]])

Avg.risk_newdistr_nowater_2001 <- PCC_average_riskscores_distr_2001_nowater_df[[1]]
Avg.risk_originaldistr_nowater_2001 <- PCC_average_riskscores_distr_2001_nowater_df[[2]]

#write.csv(Avg.risk_originaldistr_nowater_2001, file="Avg.risk_originaldistr_nowater_2001.csv")

#=============================================== #
#   Make district centorids &  risk zones labels #

avg.risk.map.water_2001 <- plot_UGA_avg.risk.zones_func2(Uganda_dist = Uganda_dist, 
                                                         risk_overlay = Avg.risk_originaldistr_nowater_2001, 
                                                         risk_map = overlay_2001[[2]])

plot(avg.risk.map.water_2001[[1]]) # mean risk scores not useful
plot(avg.risk.map.water_2001[[2]]) # therefore only plot modal risk scores

#=====================================#
#               2006                  #

#=====================================#
#  2a) plot risk factor map    #

# sanitation_2001
# poverty_lowest40_2001

overlay_2006 <- plotting_overlays_func(risk_factor1 = sanitation_2006[[3]], risk_factor2 = pig_populationFAO[[1]], 
                                       risk_factor3 = poverty_lowest40_2006[[3]],
                                       admin_processed = districts_2001, admin = districts_2001, year = "2006") 
overlay_2006[[2]]

#=================================================#
# 2b) Calculating average risk score per district #

PCC_average_riskscores_distr_2006_df <- calculate_risk_scores_districts_func(data = overlay_2006[[3]], year = "2006")

Avg.risk_newdistr_2006 <- PCC_average_riskscores_distr_2006_df[[1]]
Avg.risk_originaldistr_2006 <- PCC_average_riskscores_distr_2006_df[[2]]

#View(Avg.risk_newdistr_2006) # average PCC risk scores for all new districts
#View(Avg.risk_originaldistr_2006) # average PCC risk scores for original districts (pre-2003)

#write.csv(Avg.risk_originaldistr_2006, file="Avg.risk_originaldistr_2006.csv")

#=============================================== #
#   Make district centorids &  risk zones labels #

avg.risk.map_2006 <- plot_UGA_avg.risk.zones_func(Uganda_dist = Uganda_dist, 
                                                  risk_overlay = Avg.risk_originaldistr_2006, 
                                                  risk_map = overlay_2006[[2]])

plot(avg.risk.map_2006[[1]]) # mean risk scores not useful
plot(avg.risk.map_2006[[2]]) # therefore only plot modal risk scores

#======================================================================#
# 2c) Calculating average risk score per district (minus water bodies) #

PCC_average_riskscores_distr_2006_nowater_df <- calculate_risk_scores_districts_func2(data = overlay_2006[[3]], year = "2006",
                                                                                      data_to_join = PCC_average_riskscores_distr_2006_df[[3]])

Avg.risk_newdistr_nowater_2006 <- PCC_average_riskscores_distr_2006_nowater_df[[1]]
Avg.risk_originaldistr_nowater_2006 <- PCC_average_riskscores_distr_2006_nowater_df[[2]]

#write.csv(Avg.risk_originaldistr_nowater_2006, file="Avg.risk_originaldistr_nowater_2006.csv")

#=============================================== #
#   Make district centorids &  risk zones labels #

avg.risk.map.water_2006 <- plot_UGA_avg.risk.zones_func2(Uganda_dist = Uganda_dist, 
                                                         risk_overlay = Avg.risk_originaldistr_nowater_2006, 
                                                         risk_map = overlay_2006[[2]])

plot(avg.risk.map.water_2006[[1]]) # mean risk scores not useful
plot(avg.risk.map.water_2006[[2]]) # therefore onl

#=====================================#
#               2016                  #

#=====================================#
#  2a) plot risk factor map    #

# sanitation_2001
# poverty_lowest40_2001

overlay_2016 <- plotting_overlays_func(risk_factor1 = sanitation_2016[[3]], risk_factor2 = pig_populationFAO[[1]], 
                                       risk_factor3 = poverty_lowest40_2016[[3]],
                                       admin_processed = districts_2001, admin = districts_2001, year = "2016") 
overlay_2016[[2]]

#=================================================#
# 2b) Calculating average risk score per district #

PCC_average_riskscores_distr_2016_df <- calculate_risk_scores_districts_func(data = overlay_2016[[3]], year = "2016")

Avg.risk_newdistr_2016 <- PCC_average_riskscores_distr_2016_df[[1]]
Avg.risk_originaldistr_2016 <- PCC_average_riskscores_distr_2016_df[[2]]

#View(Avg.risk_newdistr_2016) # average PCC risk scores for all new districts
#View(Avg.risk_originaldistr_2016) # average PCC risk scores for original districts (pre-2003)

#write.csv(Avg.risk_originaldistr_2016, file="Avg.risk_originaldistr_2016.csv")

#=============================================== #
#   Make district centorids &  risk zones labels #

avg.risk.map_2016 <- plot_UGA_avg.risk.zones_func(Uganda_dist = Uganda_dist, 
                                                  risk_overlay = Avg.risk_originaldistr_2016, 
                                                  risk_map = overlay_2016[[2]])

plot(avg.risk.map_2016[[1]]) # mean risk scores not useful
plot(avg.risk.map_2016[[2]]) # therefore only plot modal risk scores

#======================================================================#
# 2c) Calculating average risk score per district (minus water bodies) #

PCC_average_riskscores_distr_2016_nowater_df <- calculate_risk_scores_districts_func2(data = overlay_2016[[3]], year = "2016",
                                                                                      data_to_join = PCC_average_riskscores_distr_2016_df[[3]])

Avg.risk_newdistr_nowater_2016 <- PCC_average_riskscores_distr_2016_nowater_df[[1]]
Avg.risk_originaldistr_nowater_2016 <- PCC_average_riskscores_distr_2016_nowater_df[[2]]

#write.csv(Avg.risk_originaldistr_nowater_2016, file="Avg.risk_originaldistr_nowater_2016.csv")

#=============================================== #
#   Make district centorids &  risk zones labels #

avg.risk.map.water_2016 <- plot_UGA_avg.risk.zones_func2(Uganda_dist = Uganda_dist, 
                                                         risk_overlay = Avg.risk_originaldistr_nowater_2016, 
                                                         risk_map = overlay_2016[[2]])

plot(avg.risk.map.water_2016[[1]]) # mean risk scores not useful
plot(avg.risk.map.water_2016[[2]]) # therefore onl


#=====================================================================================================================================#
#                                    Mapping TS studies (district-level) with avg. risk                                               #
# ====================================================================================================================================#

# TS studies (district-level) on risk maps (minus water bodies)
TS_distlvlstudies_rismap_0205 <- plot_UGA_avg.risk.zones_func3(Uganda_dist = Uganda_dist, 
                                                  risk_overlay = Avg.risk_originaldistr_2001, 
                                                  risk_map = overlay_2001[[2]],
                                                  PCC_survey_years = "2002-2005",
                                                  TS_data = UGA_MDA_PCCprev_data)
TS_distlvlstudies_rismap_0205[[3]]
TS_distlvlstudies_rismap_0205[[4]]

# TS_distlvlstudies_rismap_0610 <- plot_UGA_avg.risk.zones_func3(Uganda_dist = Uganda_dist, 
#                                                         risk_overlay = Avg.risk_originaldistr_2001, 
#                                                         risk_map = overlay_2006[[2]],
#                                                         PCC_survey_years = "2006-2010",
#                                                         TS_data = UGA_MDA_PCCprev_data)
# 
# TS_distlvlstudies_rismap_0610[[3]]

TS_distlvlstudies_rismap_1115 <- plot_UGA_avg.risk.zones_func3(Uganda_dist = Uganda_dist, 
                                                               risk_overlay = Avg.risk_originaldistr_2011, 
                                                               risk_map = overlay_2011[[2]],
                                                               PCC_survey_years = "2011-2015",
                                                               TS_data = UGA_MDA_PCCprev_data)
TS_distlvlstudies_rismap_1115[[3]]

TS_distlvlstudies_rismap_1620 <- plot_UGA_avg.risk.zones_func3(Uganda_dist = Uganda_dist, 
                                                               risk_overlay = Avg.risk_originaldistr_2016, 
                                                               risk_map = overlay_2016[[2]],
                                                               PCC_survey_years = "2016-2020",
                                                               TS_data = UGA_MDA_PCCprev_data)


TS_distlvlstudies_rismap_1620[[3]]


# ============================================================ #
# TS studies (district-level) on risk maps (with water bodies) #
TS_distlvlstudies_rismap_nowater0205 <- plot_UGA_avg.risk.zones_func4(Uganda_dist = Uganda_dist, 
                                                                      risk_overlay = Avg.risk_originaldistr_nowater_2001, 
                                                                      risk_map = overlay_2001[[2]],
                                                                      PCC_survey_years = "2002-2005",
                                                                      TS_data = UGA_MDA_PCCprev_data, UGA_map = UGA_water)

TS_distlvlstudies_rismap_nowater0205[[3]]
TS_distlvlstudies_rismap_nowater0205[[4]] # final map to call with 


TS_distlvlstudies_rismap_nowater1115 <- plot_UGA_avg.risk.zones_func4(Uganda_dist = Uganda_dist, 
                                                                      risk_overlay = Avg.risk_originaldistr_nowater_2011, 
                                                                      risk_map = overlay_2011[[2]],
                                                                      PCC_survey_years = "2011-2015",
                                                                      TS_data = UGA_MDA_PCCprev_data, UGA_map = UGA_water)

TS_distlvlstudies_rismap_nowater1115[[3]]
TS_distlvlstudies_rismap_nowater1115[[4]]

TS_distlvlstudies_rismap_nowater1620 <- plot_UGA_avg.risk.zones_func4(Uganda_dist = Uganda_dist, 
                                                                      risk_overlay = Avg.risk_originaldistr_nowater_2016, 
                                                                      risk_map = overlay_2016[[2]],
                                                                      PCC_survey_years = "2016-2020",
                                                                      TS_data = UGA_MDA_PCCprev_data, UGA_map = UGA_water)

TS_distlvlstudies_rismap_nowater1620[[3]]
TS_distlvlstudies_rismap_nowater1620[[4]]


# #===================================================================================================================================#
# #                                               Mapping sub-district MDA locations                                                  #
# 
# 
# # for 2004 sub-district MDA data and 2001-2005 risk factor map
# matched_MDA_rf_0305 <- find_subdistrictMDA_location_match_riskzone_func(Sub_district_MDA_locations_df = Sub_district_MDA_locations_2001_2005_df,
#                                                  overlay = overlay_2001, avg.risk.map = avg.risk.map_2001)
# 
# matched_MDA_rf_0305[[1]]
# 
# matched_MDA_rf_0305[[2]]
# 
# matched_MDA_rf_0305[[3]]
# 
# matched_MDA_rf_0305[[4]]
# 
# 
# # for 2012 sub-district MDA data and 2011-2015 risk factor map
# matched_MDA_rf_1115 <- find_subdistrictMDA_location_match_riskzone_func(Sub_district_MDA_locations_df = Sub_district_MDA_locations_2011_2015_df,
#                                                                         overlay = overlay_2011, avg.risk.map = avg.risk.map_2011)
# 
# matched_MDA_rf_1115[[1]]
# 
# matched_MDA_rf_1115[[2]]
# 
# matched_MDA_rf_1115[[3]]
# 
# matched_MDA_rf_1115[[4]]