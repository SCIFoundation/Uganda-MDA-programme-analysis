#===================================================================================================================================#
#                      Analysing Uganda TS data/ risk factor data in context of district-level MDA history                          #
#===================================================================================================================================#

rm(list = ls())

#=============================================#
#       Load data files                       #
#=============================================#

UGA_MDA_PCCprev_data <- read.csv("~/Uganda-MDA-programme-analysis/Data/PCC_prev_MDAyr_data.csv") 
View(UGA_MDA_PCCprev_data)

#=======================#                                                                                             
# Initiatie sub-scripts #                             
source('libraries.R')
source('Risk_factor_plotting_funcs.R')

# ================================#
# data clean /adjust for plotting #

UGA_MDA_PCCprev_data$Type <- as.factor(UGA_MDA_PCCprev_data$Type)

UGA_MDA_PCCprev_data$District <- as.factor(UGA_MDA_PCCprev_data$District)

UGA_MDA_PCCprev_data$Upper <- as.numeric(UGA_MDA_PCCprev_data$Upper)

UGA_MDA_PCCprev_data$Type <- factor(UGA_MDA_PCCprev_data$Type, levels = c("Informed", "Apparent"))

# =================================#
#    plot TS data x district & MDA #

# ggplot(data = UGA_MDA_PCCprev_data, aes(group = Type))+
#   geom_rect(data = UGA_MDA_PCCprev_data, aes(x = NULL, y=NULL, xmin=2000, xmax=First_MDA_year, ymin=0, ymax=100), alpha=0.01, fill = "blue")+
#   geom_point(aes(x = Year_offset, y = Prevalence))+
#   geom_errorbar(aes(x = Year_offset, y = Prevalence, linetype = Type, ymin= Lower, ymax = Upper), 
#                 width= 1.25)+
#   #geom_vline(aes(xintercept = First_MDA_year), colour = "blue", alpha = 0.5)+
#   #xlim(2000,2020)+
#   scale_x_continuous(limits=c(2000,2020))+
#   scale_y_continuous(limits=c(0,100))+
#   facet_wrap(~District, scales = "free")+
#   theme_bw()+
#   theme(axis.line=element_line())+
#   labs(linetype='')+
#   xlab("Year") + 
#   ylab("Porcine cysticercosis prevalence")

UGA_MDA_PCCprev_data$First_MDA_year

p <- ggplot(data = UGA_MDA_PCCprev_data, aes(group = Type))+
  #geom_rect(subset(UGA_MDA_PCCprev_data, District="Apac"), aes(x = NULL, y=NULL, xmin=2000, xmax=2003, ymin=0, ymax=100), alpha=0.1, fill = "blue")+
  geom_point(aes(x = Year_offset, y = Prevalence))+
  geom_errorbar(aes(x = Year_offset, y = Prevalence, linetype = Type, ymin= Lower, ymax = Upper), 
                width= 1.25)+
  scale_x_continuous(limits=c(2000,2020))+
  scale_y_continuous(limits=c(0,100))+
  facet_wrap(~District, scales = "free")+
  #annotate("rect", xmin=2000, xmax=2003, ymin=0, ymax=100, fill="blue", alpha = .1)+
  theme_bw()+
  theme(axis.line=element_line())+
  labs(linetype='')+
  xlab("Year") + 
  ylab("Porcine cysticercosis prevalence")


# make pre-MDA (endemic) rectangles on TS PCC prevalence data by district plots

p1 <- p + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Apac", Type = NA), 
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p2 <- p1 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Arua", Type = NA), 
                   aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p3 <- p2 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Busia", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p4 <- p3 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Kaberamaido", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p5 <- p4 + geom_rect(data = data.frame(xmin=2000, xmax=2020, ymin=0, ymax=100, District = "Kampala", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p6 <- p5 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Kamuli", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p7 <- p6 + geom_rect(data = data.frame(xmin=2000, xmax=2013, ymin=0, ymax=100, District = "Katakwi", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p8 <- p7 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Kayunga", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p9 <- p8 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Kibaale", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p10 <- p9 + geom_rect(data = data.frame(xmin=2000, xmax=2013, ymin=0, ymax=100, District = "Kumi", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p11 <- p10 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Lira", Type = NA), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p12 <- p11 + geom_rect(data = data.frame(xmin=2000, xmax=2020, ymin=0, ymax=100, District = "Luwero", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p13 <- p12 + geom_rect(data = data.frame(xmin=2000, xmax=2007, ymin=0, ymax=100, District = "Masaka", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p14 <- p13 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Moyo", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p15 <- p14 + geom_rect(data = data.frame(xmin=2000, xmax=2007, ymin=0, ymax=100, District = "Mpigi", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p16 <- p15 + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, District = "Mubende", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p17 <- p16 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Mukono", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p18 <- p17 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Nakasongola", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p18 <- p17 + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, District = "Pallisa", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p19 <- p18 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Soroti", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

pFinal <- p19 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Wakiso", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

pFinal


#==========================================================#
#              Plot risk factor maps x 2002 disticts       #

#===========#
#  load     #
load("~/Uganda-MDA-programme-analysis/Data/pig_populationFAO.rdata") # only need this once

load("~/Uganda-MDA-programme-analysis/Data/sanitation2011.rdata")
load("~/Uganda-MDA-programme-analysis/Data/poverty_lowest40_2011.rdata") 



# 2001 #
sanitation2011 <- sanitation
poverty_lowest40_2011 <- poverty_lowest40

#===========#
#  plotting #

# admin <- readShapePoly('~/Uganda_porcine_cysticercosis_risk_mapping/data/admin-gdam/UGA_adm1.shp')
# admin_2011_processed <- processing_admin_data_func(admin = admin_2011) # process admin (district) spatial file

#districts_2001_processed <- processing_admin_data_func(admin = districts_2001) # process admin (district) spatial file
#district_map_0319 <- UGA_district_boundaries_function(shape_file = districts_2001, national_map_input = national_map) 
#district_map_0319[[1]]

#===========#
#   2001    #

districts_2001 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp") # need this for 2006

overlay_2011 <- plotting_overlays_func(risk_factor1 = sanitation2011[[3]], risk_factor2 = pig_populationFAO[[1]], risk_factor3 = poverty_lowest40_2011[[3]],
                                        admin_processed = districts_2001, admin = districts_2001, year = "2011") 
overlay_2011[[2]]


#========================================================================================================================================#
#                                  Calculating average risk score per district                                                          #

riskfact_2011_df <- overlay_2011[[3]]

riskfact_2011_df$long <- riskfact_2011_df$x
riskfact_2011_df$lat <- riskfact_2011_df$y

# Approach see: https://stackoverflow.com/questions/64229997/using-latitude-and-longitude-information-to-assign-locations-to-districts-in-afg

# get distirct co-ordinates to match 
UGA_districts_data <- rnaturalearth::ne_states(country = 'Uganda', 
                                               returnclass = 'sf') %>%
  select(name, name_en)

# make lat & lon dataframe into spatial object (sp)

xy <- riskfact_2011_df[,c(1,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data = riskfact_2011_df,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

spdf2 <- sf::st_as_sf(spdf) # convert to sp object

master2 <- sf::st_join(spdf2, UGA_districts_data) # join risk factor data and admin data

# test average risk score

master2$district_factor_test <- as.factor(master2$name)

sf::st_geometry(master2) <- NULL

master3 <- master2 %>% tidyr::drop_na(value)

# create risk scores per district

risk_score <- master3 %>% 
  group_by(district_factor_test) %>% 
  dplyr::summarise(n = n(), risk = sum(value))

mymode <- function(x) {
  t <- table(x)
  names(t)[ which.max(t) ]
}

risk_score <- master3 %>% 
  group_by(district_factor_test) %>% 
  dplyr::summarise(n = n(), risk = sum(value),
                   mode_risk = mymode(value))

risk_score$mean_risk <- risk_score$risk/risk_score$n

risk_score <- 
  risk_score %>%
  mutate(mean_risk_score = cut(mean_risk,
                               breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                               labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))

risk_score <- 
  risk_score %>%
  mutate(mode_risk_score = cut(as.numeric(mode_risk),
                               breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                               labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))


#write.csv(risk_score,'risk_score_2006.csv')

# ======================================================================= #
# recode newly created districts (since 2002) to original district names  #

risk_score$recoded_districts <- recode_factor(risk_score$district_factor_test, 
                                              "Abim" = "Kotido", "Adjumani" = "Adjumani", 
                                              "Agago" = "Pader", "Alebtong" = "Lira", "Amolatar" = "Lira",
                                              "Amudat" = "Nakapiripirit", "Amuria" = "Katakwi",
                                              "Amuru" = "Gulu", "Apac" = "Apac", "Arua" = "Arua", 
                                              "Budaka" = "Pallisa", "Bududa" = "Mbale", "Bugiri" = "Bugiri",
                                              "Buhweju" = "Bushenyi", "Buikwe" = "Mukono", "Bukedea" = "Kumi",
                                              "Bukomansimbi" = "Masaka", "Bukwa" = "Kapchorwa", 
                                              "Bulambuli" = "Sironko", "Buliisa" = "Masindi", 
                                              "Bundibugyo" = "Bundibugyo", "Bushenyi" = "Bushenyi",
                                              "Busia" = "Busia", "Butaleja" = "Tororo", "Butambala" = "Mpigi",
                                              "Buvuma" = "Mukono", "Buyende" = "Kamuli", "Dokolo" = "Lira",
                                              "Gomba" = "Mpigi", "Gulu" = "Gulu", "Hoima" = "Hoima",
                                              "Ibanda" = "Mbarara", "Iganga" = "Iganga", "Isingiro" = "Mbarara",
                                              "Jinja" = "Jinja", "Kaabong" = "Kotido", "Kabale" = "Kabale",
                                              "Kabarole" = "Kabarole", "Kaberamaido" = "Kaberamaido",
                                              "Kalangala" = "Kalangala", "Kaliro" = "Kamuli", "Kalungu" = "Masaka",
                                              "Kampala" = "Kampala", "Kamuli" = "Kamuli", "Kamwenge" = "Kamwenge",
                                              "Kanungu" = "Kanungu", "Kapchorwa" = "Kapchorwa", "Kasese" = "Kasese",
                                              "Katakwi" = "Katakwi", "Kayunga" = "Kayunga", "Kibale" = "Kibale",
                                              "Kiboga" = "Kiboga", "Kibuku" = "Pallisa", "Kiryandongo" = "Masindi",
                                              "Kisoro" = "Kisoro", "Kitgum" = "Kitgum", "Koboko" = "Arua",
                                              "Kole" = "Apac", "Kotido" = "Kotido", "Kumi" = "Kumi", "Kween" = "Kapchorwa",
                                              "Kyankwanzi" = "Kiboga", "Kyegegwa" = "Kyenjojo", "Kyenjojo" = "Kyenjojo",
                                              "Lamwo" = "Kitgum", "Lira" = "Lira", "Luuka" = "Iganga", "Luweero" = "Luweero",
                                              "Lwengo" = "Masaka", "Lyantonde" = "Rakai", "Manafwa" = "Mbale", 
                                              "Maracha" = "Arua", "Masaka" = "Masaka", "Masindi" = "Masindi", "Mayuge" = "Mayuge",
                                              "Mbale" = "Mbale", "Mbarara" = "Mbarara", "Mitooma" = "Bushenyi",
                                              "Mityana" = "Mubende", "Moroto" = "Moroto", "Moyo" = "Moyo", "Mpigi" = "Mpigi",
                                              "Mubende" = "Mubende", "Mukono" = "Mukono", "Nakapiripirit" = "Nakapiripirit",
                                              "Nakaseke" = "Luweero", "Nakasongola" = "Nakasongola", "Namayingo" = "Bugiri",
                                              "Namutumba" = "Iganga", "Napak" = "Moroto", "Nebbi" = "Nebbi", "Ngora" = "Kumi",
                                              "Ntoroko" = "Bundibugyo", "Ntungamo" = "Ntungamo", "Nwoya" = "Gulu", 
                                              "Otuke" = "Lira", "Oyam" = "Apac", "Pader" = "Pader", "Pallisa" = "Pallisa",
                                              "Rakai" = "Rakai", "Rubirizi" = "Bushenyi", "Rukungiri" = "Rukungiri", 
                                              "Sembabule" = "Sembabule", "Serere" = "Soroti", "Sheema" = "Bushenyi",
                                              "Sironko" = "Sironko", "Soroti" = "Soroti", "Tororo" = "Tororo",
                                              "Wakiso" = "Wakiso", "Yumbe" = "Yumbe", "Zombo" = "Nebbi")
# ================================= #
# create risk scores per district   #


master3_olddistr <- master3 

master3_olddistr$recoded_districts <- recode_factor(master3_olddistr$district_factor_test, 
                                              "Abim" = "Kotido", "Adjumani" = "Adjumani", 
                                              "Agago" = "Pader", "Alebtong" = "Lira", "Amolatar" = "Lira",
                                              "Amudat" = "Nakapiripirit", "Amuria" = "Katakwi",
                                              "Amuru" = "Gulu", "Apac" = "Apac", "Arua" = "Arua", 
                                              "Budaka" = "Pallisa", "Bududa" = "Mbale", "Bugiri" = "Bugiri",
                                              "Buhweju" = "Bushenyi", "Buikwe" = "Mukono", "Bukedea" = "Kumi",
                                              "Bukomansimbi" = "Masaka", "Bukwa" = "Kapchorwa", 
                                              "Bulambuli" = "Sironko", "Buliisa" = "Masindi", 
                                              "Bundibugyo" = "Bundibugyo", "Bushenyi" = "Bushenyi",
                                              "Busia" = "Busia", "Butaleja" = "Tororo", "Butambala" = "Mpigi",
                                              "Buvuma" = "Mukono", "Buyende" = "Kamuli", "Dokolo" = "Lira",
                                              "Gomba" = "Mpigi", "Gulu" = "Gulu", "Hoima" = "Hoima",
                                              "Ibanda" = "Mbarara", "Iganga" = "Iganga", "Isingiro" = "Mbarara",
                                              "Jinja" = "Jinja", "Kaabong" = "Kotido", "Kabale" = "Kabale",
                                              "Kabarole" = "Kabarole", "Kaberamaido" = "Kaberamaido",
                                              "Kalangala" = "Kalangala", "Kaliro" = "Kamuli", "Kalungu" = "Masaka",
                                              "Kampala" = "Kampala", "Kamuli" = "Kamuli", "Kamwenge" = "Kamwenge",
                                              "Kanungu" = "Kanungu", "Kapchorwa" = "Kapchorwa", "Kasese" = "Kasese",
                                              "Katakwi" = "Katakwi", "Kayunga" = "Kayunga", "Kibale" = "Kibale",
                                              "Kiboga" = "Kiboga", "Kibuku" = "Pallisa", "Kiryandongo" = "Masindi",
                                              "Kisoro" = "Kisoro", "Kitgum" = "Kitgum", "Koboko" = "Arua",
                                              "Kole" = "Apac", "Kotido" = "Kotido", "Kumi" = "Kumi", "Kween" = "Kapchorwa",
                                              "Kyankwanzi" = "Kiboga", "Kyegegwa" = "Kyenjojo", "Kyenjojo" = "Kyenjojo",
                                              "Lamwo" = "Kitgum", "Lira" = "Lira", "Luuka" = "Iganga", "Luweero" = "Luweero",
                                              "Lwengo" = "Masaka", "Lyantonde" = "Rakai", "Manafwa" = "Mbale", 
                                              "Maracha" = "Arua", "Masaka" = "Masaka", "Masindi" = "Masindi", "Mayuge" = "Mayuge",
                                              "Mbale" = "Mbale", "Mbarara" = "Mbarara", "Mitooma" = "Bushenyi",
                                              "Mityana" = "Mubende", "Moroto" = "Moroto", "Moyo" = "Moyo", "Mpigi" = "Mpigi",
                                              "Mubende" = "Mubende", "Mukono" = "Mukono", "Nakapiripirit" = "Nakapiripirit",
                                              "Nakaseke" = "Luweero", "Nakasongola" = "Nakasongola", "Namayingo" = "Bugiri",
                                              "Namutumba" = "Iganga", "Napak" = "Moroto", "Nebbi" = "Nebbi", "Ngora" = "Kumi",
                                              "Ntoroko" = "Bundibugyo", "Ntungamo" = "Ntungamo", "Nwoya" = "Gulu", 
                                              "Otuke" = "Lira", "Oyam" = "Apac", "Pader" = "Pader", "Pallisa" = "Pallisa",
                                              "Rakai" = "Rakai", "Rubirizi" = "Bushenyi", "Rukungiri" = "Rukungiri", 
                                              "Sembabule" = "Sembabule", "Serere" = "Soroti", "Sheema" = "Bushenyi",
                                              "Sironko" = "Sironko", "Soroti" = "Soroti", "Tororo" = "Tororo",
                                              "Wakiso" = "Wakiso", "Yumbe" = "Yumbe", "Zombo" = "Nebbi")
#levels(master3_olddistr$recoded_districts)

risk_score2 <- master3_olddistr %>% 
  group_by(recoded_districts) %>% 
  dplyr::summarise(n = n(), risk = sum(value))

risk_score2 <- master3_olddistr %>% 
  group_by(recoded_districts) %>% 
  dplyr::summarise(n = n(), risk = sum(value),
                   mode_risk = mymode(value))

risk_score2$mean_risk <- risk_score2$risk/risk_score2$n

risk_score2 <- 
  risk_score2 %>%
  mutate(mean_risk_score = cut(mean_risk,
                               breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                               labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))

risk_score2 <- 
  risk_score2 %>%
  mutate(mode_risk_score = cut(as.numeric(mode_risk),
                               breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                               labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))

View(risk_score2)

risk_score2a <- slice(risk_score2, 1:(n() - 1))  

View(risk_score2a)

risk_score2a <- as.data.frame(risk_score2a)

risk_score2a$recoded_districts_chr <- as.character(risk_score2a$recoded_districts) #need factor col as character to re-arrange alphabetically

risk_score2a <- risk_score2a  %>% arrange(recoded_districts_chr)

#=============================================== #
#   Make district centorids &  risk zones labels #

Uganda_dist <- rgdal::readOGR("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp")

# to plot #
Uganda_dist_latlon <- sp::spTransform(Uganda_dist, CRS("+proj=longlat +datum=WGS84"))
plot(Uganda_dist_latlon, axes=TRUE)
points(x=31.76828, y=3.22909063, col = "red", pch=1, bg = "red")

# to get centroids for each district #
Uganda_dist_centroids <- coordinates(sp::spTransform(Uganda_dist, CRS("+proj=longlat +datum=WGS84")))

Uganda_dist_centroids_df <- as.data.frame(Uganda_dist_centroids)

vector_dist <- as.character(Uganda_dist_latlon$DISTRICT) # make a vector of district names to supply a column with these

Uganda_dist_centroids_df$district <- vector_dist # add this vector to the centroids dataframe

Uganda_dist_centroids_df <- 
  Uganda_dist_centroids_df %>% 
  dplyr::rename(
    lon = V1,
    lat = V2
  )

Uganda_dist_centroids_df 

Uganda_2011_master <- cbind(Uganda_dist_centroids_df, risk_score2a)

Uganda_2011_master <- within(Uganda_2011_master,  label <- paste(recoded_districts_chr,mean_risk_score,mode_risk_score, sep="; ")) # make a column for a label

View(Uganda_2011_master)


labels_to_remove <- c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule")


Uganda_2011_master$label[Uganda_2011_master$recoded_districts %in% c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule")] <- NA

#=============================================== #
#   plot labels on risk map                      #

Risk_map_2011 <- overlay_2011[[2]]

Risk_map_2011 +
  geom_text_repel(data = Uganda_2011_master, aes(x = lon, y = lat, label = label), 
                  fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) 

Risk_map_2011 +
  geom_text_repel(data = Uganda_2011_master, aes(lon, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4, family = 'Avenir', segment.color = "#333333")


# 
# 
# 
# Uganda_2011_master %>%
#   dplyr::mutate(X_nudge = dplyr::case_when(recoded_districts == 'Kotido' ~ 2
#                                            ,recoded_districts == 'Adjumani' ~ .5
#                                            ,recoded_districts == 'Pader' ~ 2
#                                            ,recoded_districts == 'Lira' ~ 0
#                                            ,recoded_districts == 'Nakapiripirit' ~ 1.3
#                                            #,recoded_districts == 'Katakwi' ~ 0
#                                            #,recoded_districts == 'Gulu' ~ 0
#                                            #,recoded_districts == 'Apac' ~ 0
#                                            ,recoded_districts == 'Arua' ~ -10
#                                            ,recoded_districts == 'Pallisa' ~ 3
#                                            ,recoded_districts == 'Yumbe' ~ -10
#                                            ,TRUE ~ 0)
#                 ,y_nudge = dplyr::case_when(recoded_districts == 'Kotido' ~ 1
#                                             ,recoded_districts == 'Adjumani' ~ 3
#                                             ,recoded_districts == 'Pader' ~ 2
#                                             ,recoded_districts == 'Lira' ~ .25
#                                             ,recoded_districts == 'Nakapiripirit' ~ .1
#                                             #,recoded_districts == 'Katakwi' ~ 0
#                                             #,recoded_districts == 'Gulu' ~ 0
#                                             #,recoded_districts == 'Apac' ~ 0
#                                             ,recoded_districts == 'Arua' ~ 0
#                                             ,recoded_districts == 'Pallisa' ~ 0.25
#                                             ,recoded_districts == 'Yumbe' ~ 0
#                                             ,TRUE ~ 0)
#   ) -> Uganda_2011_master
# 
# 
# 
# 
# 
# Risk_map_2011 +
# geom_text_repel(data = Uganda_2011_master
#                 ,aes(x = lon
#                      ,y = lat
#                      ,label = label
#                 )
#                 ,family = 'Avenir'
#                 ,nudge_x = Uganda_2011_master$x_nudge
#                 ,nudge_y = Uganda_2011_master$y_nudge
#                 ,segment.color = "#333333"
# )
# 
# 
# 
# 
# 
# 
# port_data %>% 
#   mutate(x_nudge = case_when( location == 'Port Brownsville, Texas' ~ 1.3
#                               ,location == 'Port Isabel, Texas' ~ 1.3
#                               ,location == 'Port Mansfield, Texas' ~ 1.5
#                               ,location == 'Port Corpus Christi, Texas' ~ 1.5
#                               ,location == 'Port Lavaca, Texas' ~ -1
#                               ,location == 'Port Freeport, Texas' ~ 1
#                               #,location == 'Port of Texas City, Texas' ~ 0
#                               ,location == 'Texas City, Texas' ~ -1
#                               ,location == 'Port Galveston, Texas' ~ 1
#                               ,location == 'Port Houston, Texas' ~ -1.5
#                               ,location == 'Port Sabine Pass, Texas' ~ .5
#                               ,location == 'Port Arthur, Texas' ~ 1
#                               ,location == 'Port Beaumont, Texas' ~ -.6
#                               ,location == 'Port of Orange, Texas' ~ 1.6
#                               ,TRUE ~ 0)
#          ,y_nudge = case_when( location == 'Port Brownsville, Texas' ~ -1
#                                ,location == 'Port Isabel, Texas' ~ 0
#                                ,location == 'Port Mansfield, Texas' ~ .2
#                                ,location == 'Port Corpus Christi, Texas' ~ 0
#                                ,location == 'Port Lavaca, Texas' ~ .5
#                                ,location == 'Port Freeport, Texas' ~ -.5
#                                ,location == 'Texas City, Texas' ~ 0
#                                ,location == 'Port Galveston, Texas' ~ -.5
#                                ,location == 'Port Houston, Texas' ~ .8
#                                ,location == 'Port Sabine Pass, Texas' ~ -.5
#                                ,location == 'Port Arthur, Texas' ~ .1
#                                ,location == 'Port Beaumont, Texas' ~ .6
#                                ,location == 'Port of Orange, Texas' ~ .5
#                                ,TRUE ~ 0)
#   ) -> port_data