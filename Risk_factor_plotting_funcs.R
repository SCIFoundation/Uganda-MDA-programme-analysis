#================================================#
# function for plotting raster layer in ggplot2  #
# # see: https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r 

gplot_data <- function(x, maxpixels = 50000)  {
  
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  
  ## Extract values
  
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  
  names(dat) <- c('value', 'variable')
  
  dat <- tibble::as_tibble(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}



#============================================================================#
# function for plotting overlay of risk factors (combined risk factor score) #

plotting_overlays_func <- function(risk_factor1, risk_factor2, risk_factor3, admin_processed, admin, year){
  
  # set-up plot breakpoints in base R
  breakpoints <- c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12)
  
  breakpoints2 <- c('all low', 'A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')
  
  colors <- c( "gray90", "gold", "steelblue1", "red", "springgreen", 'orange', 'plum1', "brown")
  
  # combined metric with 3 risk factors (e.g. rf1 + (rf2 * 1.01) + (rf3 * 1.1))
  
  # * A proportion of households bad sanitation high =1 is low = 0  
  # * B proportion of is high =1.1 is low =0 
  # * C proportion of poor 40% is high = 1.01 is low zero 
  
  over <- risk_factor1 + risk_factor2 * 1.01 + risk_factor3 * 1.1
  over <- raster::mask(over , admin)
  
  # plot in base r 
  plot(over , breaks = breakpoints, col = colors, legend = FALSE)
  legend("bottomleft", inset = .02, title = "classes", legend = breakpoints2
         , fill = colors, horiz = F, cex = 0.8)
  plot(admin, add = T)
  
  # plot risk map in ggplot2 #
  riskfact_df <- gplot_data(over)
  
  riskfact_df <- 
    riskfact_df %>%
    mutate(risk_fact_bins = cut(value,
                                breaks = breakpoints, right = FALSE,
                                labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  # continuous risk factor plot #
  a <- ggplot() +
    
    geom_tile(data = riskfact_df, 
              aes(x = x, y = y, fill = value)) +
    geom_polygon(data=admin_processed, aes(x=long, y=lat, group=group), color="black", alpha=0) +
    scale_fill_gradient("classes",
                        low = 'yellow', high = 'blue',
                        na.value = NA) +
    coord_equal() +
    theme_bw()
  
  # FINAL plot: discrete risk factor plot # 
  # discrete color gradient from continous color gradient in ggplot: https://stackoverflow.com/questions/64762377/create-r-ggplot2-discrete-colour-palette-for-gradient-map-with-continuous-values
  # convert cont to discrete : https://www.r-bloggers.com/2020/09/how-to-convert-continuous-variables-into-categorical-by-creating-bins/
  
  value_rf <- c("grey90", "gold", "darkorchid1", "red1", "springgreen", "darkorange1", "pink1", "tan4")
  
  b <- ggplot() +
    geom_tile(data = riskfact_df, 
              aes(x = x, y = y, fill = risk_fact_bins)) +
    geom_polygon(data=admin_processed, aes(x=long, y=lat, group=group), color="black", alpha=0) +
    scale_fill_manual(name = "Class",
                      values = value_rf, 
                      na.value = NA, na.translate = FALSE)+
    coord_equal() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE)
    #annotate("text", label = year, x = 29.75, y = 3.8, size = 7, colour = "black")
  
  return(list(a, b, riskfact_df))
  
  
}


# ===================================== #
# process admin (district) spatial file #
processing_admin_data_func <- function(admin) {
  
  admin$NAME_1 <- as.character(admin$NAME_1) # make character variable for district names
  
  admin$label <- NA 
  
  # admin$label[(admin$NAME_1 == 'Lira'|admin$NAME_1 == 'Masaka'|admin$NAME_1 == 'Mukono'|admin$NAME_1 == 'Kamuli'|
  #                admin$NAME_1 =='Hoima'|admin$NAME_1 =='Moyo'|admin$NAME_1 == 'Kumi'|admin$NAME_1 == 'Apac'|
  #                admin$NAME_1 =='Kaberamaido'|admin$NAME_1 == 'Soroti'|admin$NAME_1 == 'Kayunga')] <- 
  #   admin$NAME_1[(admin$NAME_1 == 'Lira'|admin$NAME_1 == 'Masaka'|admin$NAME_1 == 'Mukono'|admin$NAME_1 == 'Kamuli'|
  #                   admin$NAME_1 == 'Hoima'|admin$NAME_1 == 'Moyo'|admin$NAME_1 == 'Kumi'|admin$NAME_1 == 'Apac'|
  #                   admin$NAME_1 =='Kaberamaido'|admin$NAME_1 == 'Soroti'|admin$NAME_1 == 'Kayunga')]
  
  admin2 <- fortify(admin)
  
  return(admin)
}

#==============================================================#
#      Function: calculating avg. risk scores                  #

calculate_risk_scores_districts_func <- function(data, year) {

  # Approach see: https://stackoverflow.com/questions/64229997/using-latitude-and-longitude-information-to-assign-locations-to-districts-in-afg

  # extract lat and longitutdes
  data$long <- data$x
  data$lat <- data$y
  
  # get distirct co-ordinates to match 
  
  UGA_districts_data <- rnaturalearth::ne_states(country = 'Uganda', 
                                                returnclass = 'sf') %>%
     dplyr::select(name, name_en)
  
  UGA_districts_data <- rnaturalearth::ne_states(country = 'Uganda', 
                                                 returnclass = 'sf') 

# make lat & lon dataframe into spatial object (sp)
xy <- data[,c(1,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data = data,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

spdf2 <- sf::st_as_sf(spdf) # convert to sp object

master2 <- sf::st_join(spdf2, UGA_districts_data) # join risk factor data and admin data

# test average risk score
master2$district_factor_test <- as.factor(master2$name)

master_to_save <- master2

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
# recode newly created districts (since 2002) to ORIGINAL district names  #
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
# sort districts alphabetically
risk_score <- as.data.frame(risk_score)

risk_score$recoded_districts_chr <- as.character(risk_score$recoded_districts) #need factor col as character to re-arrange alphabetically

risk_score <- risk_score  %>% arrange(recoded_districts_chr)

# add year factor
if(year == "2011"){
  risk_score$year <- "2011-2015"
}

if(year == "2001"){
  risk_score$year <- "2001-2005"
}

if(year == "2006"){
  risk_score$year <- "2006-2010"
}

if(year == "2016"){
  risk_score$year <- "2016-2020"
}

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
                   median_risk = median(value),
                   mode_risk = mymode(value))

risk_score2$mean_risk <- risk_score2$risk/risk_score2$n

risk_score2 <- 
  risk_score2 %>%
  mutate(mean_risk_score = cut(mean_risk,
                               breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                               labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))

risk_score2 <- 
  risk_score2 %>%
  mutate(median_risk_score = cut(median_risk,
                               breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                               labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))

# mode provides best average description with large samples (i.e. all RF values accross a district)
risk_score2 <- 
  risk_score2 %>%
  mutate(mode_risk_score = cut(as.numeric(mode_risk),
                               breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                               labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))

# further editing 
risk_score2a <- slice(risk_score2, 1:(n() - 1))  # remove final row (NA)

# sort districts alphabetically
risk_score2a <- as.data.frame(risk_score2a)

risk_score2a$recoded_districts_chr <- as.character(risk_score2a$recoded_districts) #need factor col as character to re-arrange alphabetically

risk_score2a <- risk_score2a  %>% arrange(recoded_districts_chr)

if(year == "2011"){
  risk_score2a$year <- "2011-2015"
}

if(year == "2001"){
  risk_score2a$year <- "2001-2005"
}

if(year == "2006"){
  risk_score2a$year <- "2006-2010"
}

if(year == "2016"){
  risk_score2a$year <- "2016-2020"
}

return(list(risk_score, risk_score2a, master_to_save))

}


#=========================================================================#
#      Function: calculating avg. risk scores - minus water bodies        #

calculate_risk_scores_districts_func2 <- function(data, year, data_to_join) {
  
  # Approach see: https://stackoverflow.com/questions/64229997/using-latitude-and-longitude-information-to-assign-locations-to-districts-in-afg
  #data<- overlay_2001[[3]]
  
  # # extract lat and longitutdes
  # data$long <- data$x
  # data$lat <- data$y
  # 
  # # get distirct co-ordinates to match 
  # 
  # UGA_districts_data <- rnaturalearth::ne_states(country = 'Uganda', 
  #                                                returnclass = 'sf') %>%
  #   dplyr::select(name, name_en)
  # 
  # UGA_districts_data <- rnaturalearth::ne_states(country = 'Uganda', 
  #                                                returnclass = 'sf') 
  # 
  # # make lat & lon dataframe into spatial object (sp)
  # xy <- data[,c(1,2)]
  # 
  # spdf <- SpatialPointsDataFrame(coords = xy, data = data,
  #                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  # 
  # spdf2 <- sf::st_as_sf(spdf) # convert to sp object
  # 
  # master2 <- sf::st_join(spdf2, UGA_districts_data) # join risk factor data and admin data
  # 
  # # test average risk score
  # master2$district_factor_test <- as.factor(master2$name)
  # 
  # sf::st_geometry(master2) <- NULL
  # 
  # master3 <- master2 %>% tidyr::drop_na(value)
  
  # ==========================================================================#
  # need to locate water bodies and remove co-ordinates from analysis for UGA #
  
  URL <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_lakes.zip"
  
  fil <- basename(URL)
  if (!file.exists(fil)) download.file(URL, fil)
  fils <- unzip(fil)
  lakes <- readOGR(grep("shp$", fils, value=TRUE), "ne_50m_lakes",
                   stringsAsFactors=FALSE, verbose=FALSE)
  
  lakes_sf <- sf::st_as_sf(lakes) # make sf spatial object for joining
  
  lakes_sf2  <- lakes_sf  %>% dplyr::select(name, name_en) # just select name columns
  
  # =============================================================================#
  # 2) calculate score (minus water bodies) for all sub-counties within dsitrict #
  
  master_lakes_district <- sf::st_join(data_to_join, lakes_sf2) # join the two sf objects
  
  master_lakes_district_filtered <- master_lakes_district %>% filter(!name.y %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward")) # removes any observation including lakes
  
  # test average risk score
  #master_lakes_district_filtered$district_factor_test <- as.factor(master_lakes_district_filtered$DISTRICT)
  
  sf::st_geometry(master_lakes_district_filtered) <- NULL
  
  master_lakes_district_filtered2 <- master_lakes_district_filtered %>% tidyr::drop_na(value)
  
  # if(year == 2011){
  #   master_lakes_district_filtered2 <- master_lakes_district_filtered2 %>% tidyr::drop_na(SNAME_2010)
  # }
  # 
  # if(year == 2015 || year == 2019){
  #   master_lakes_district_filtered2 <- master_lakes_district_filtered2 %>% tidyr::drop_na(Subcounty)
  # }
  # 

  # create risk scores per district
  risk_score_dist_nolakes <- master_lakes_district_filtered2 %>% 
    group_by(district_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value))
  
  mymode <- function(x) {
    t <- table(x)
    names(t)[ which.max(t) ]
  }
  
  risk_score_dist_nolakes <- master_lakes_district_filtered2 %>% 
    group_by(district_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value),
                     mode_risk = mymode(value))
  
  risk_score_dist_nolakes$mean_risk <- risk_score_dist_nolakes$risk/risk_score_dist_nolakes$n
  
  risk_score_dist_nolakes <- 
    risk_score_dist_nolakes %>%
    mutate(mean_risk_score = cut(mean_risk,
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  risk_score_dist_nolakes <- 
    risk_score_dist_nolakes %>%
    mutate(mode_risk_score = cut(as.numeric(mode_risk),
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  #write.csv(risk_score,'risk_score_2006.csv')
  
  # ======================================================================= #
  # recode newly created districts (since 2002) to ORIGINAL district names  #
  risk_score_dist_nolakes$recoded_districts <- recode_factor(risk_score_dist_nolakes$district_factor_test, 
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
  # sort districts alphabetically
  risk_score_dist_nolakes <- as.data.frame(risk_score_dist_nolakes)
  
  risk_score_dist_nolakes$recoded_districts_chr <- as.character(risk_score_dist_nolakes$recoded_districts) #need factor col as character to re-arrange alphabetically
  
  risk_score_dist_nolakes <- risk_score_dist_nolakes  %>% arrange(recoded_districts_chr)
  
  # add year factor
  if(year == "2011"){
    risk_score_dist_nolakes$year <- "2011-2015"
  }
  
  if(year == "2001"){
    risk_score_dist_nolakes$year <- "2001-2005"
  }
  
  if(year == "2006"){
    risk_score_dist_nolakes$year <- "2006-2010"
  }
  
  if(year == "2016"){
    risk_score_dist_nolakes$year <- "2016-2020"
  }
  
  # ================================= #
  # create risk scores per district   #
  master_lakes_district_filtered2_olddistr <- master_lakes_district_filtered2
  
  master_lakes_district_filtered2_olddistr$recoded_districts <- recode_factor(master_lakes_district_filtered2_olddistr$district_factor_test, 
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
  risk_score_dist_nolakes2 <- master_lakes_district_filtered2_olddistr %>% 
    group_by(recoded_districts) %>% 
    dplyr::summarise(n = n(), risk = sum(value))
  
  risk_score_dist_nolakes2 <- master_lakes_district_filtered2_olddistr %>% 
    group_by(recoded_districts) %>% 
    dplyr::summarise(n = n(), risk = sum(value),
                     median_risk = median(value),
                     mode_risk = mymode(value))
  
  risk_score_dist_nolakes2$mean_risk <- risk_score_dist_nolakes2$risk/risk_score_dist_nolakes2$n
  
  risk_score_dist_nolakes2 <- 
    risk_score_dist_nolakes2 %>%
    mutate(mean_risk_score = cut(mean_risk,
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  risk_score_dist_nolakes2 <- 
    risk_score_dist_nolakes2 %>%
    mutate(median_risk_score = cut(median_risk,
                                   breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                   labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  # mode provides best average description with large samples (i.e. all RF values accross a district)
  risk_score_dist_nolakes2 <- 
    risk_score_dist_nolakes2 %>%
    mutate(mode_risk_score = cut(as.numeric(mode_risk),
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  # further editing 
  risk_score_dist_nolakes2a <- slice(risk_score_dist_nolakes2, 1:(n() - 1))  # remove final row (NA)
  
  # sort districts alphabetically
  risk_score_dist_nolakes2a <- as.data.frame(risk_score_dist_nolakes2a)
  
  risk_score_dist_nolakes2a$recoded_districts_chr <- as.character(risk_score_dist_nolakes2a$recoded_districts) #need factor col as character to re-arrange alphabetically
  
  risk_score_dist_nolakes2a <- risk_score_dist_nolakes2a  %>% arrange(recoded_districts_chr)
  
  if(year == "2011"){
    risk_score_dist_nolakes2a$year <- "2011-2015"
  }
  
  if(year == "2001"){
    risk_score_dist_nolakes2a$year <- "2001-2005"
  }
  
  if(year == "2006"){
    risk_score_dist_nolakes2a$year <- "2006-2010"
  }
  
  if(year == "2016"){
    risk_score_dist_nolakes2a$year <- "2016-2020"
  }
  
  return(list(risk_score_dist_nolakes, risk_score_dist_nolakes2a))
  
}


#==============================================================#
#      Function: Plotting average risk scores on UGA map       #


plot_UGA_avg.risk.zones_func <- function(Uganda_dist, risk_overlay, risk_map){

  # transform co-ordinates and test plot #
Uganda_dist_latlon <- sp::spTransform(Uganda_dist, CRS("+proj=longlat +datum=WGS84"))
plot(Uganda_dist_latlon, axes=TRUE)
points(x=31.76828, y=3.22909063, col = "red", pch=1, bg = "red")

# to get centroids for each district #
Uganda_dist_centroids <- coordinates(sp::spTransform(Uganda_dist, CRS("+proj=longlat +datum=WGS84"))) # trasnform spatial obj to lat/lon data

Uganda_dist_centroids_df <- as.data.frame(Uganda_dist_centroids)

vector_dist <- as.character(Uganda_dist_latlon$DISTRICT) # make a vector of district names to supply a column with these

Uganda_dist_centroids_df$district <- vector_dist # add this vector to the centroids dataframe

Uganda_dist_centroids_df <- 
  Uganda_dist_centroids_df %>% 
  dplyr::rename(
    lon = V1,
    lat = V2
  )

Uganda_master <- cbind(Uganda_dist_centroids_df, risk_overlay)

Uganda_master <- within(Uganda_master,  label <- paste(recoded_districts_chr, mean_risk_score, mode_risk_score, sep="; ")) # make a column for a label
Uganda_master <- within(Uganda_master,  label2 <- paste(recoded_districts_chr, mode_risk_score, sep="; ")) # make a column for a label

labels_to_remove <- c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule") # districts where no MDA has occured

# remove these districts (make NA in label)
Uganda_master$label[Uganda_master$recoded_districts %in% c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule")] <- NA
Uganda_master$label2[Uganda_master$recoded_districts %in% c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule")] <- NA

#=============================================== #
#   plot labels on risk map                      #

risk_map1 <- 
  risk_map +
  ggrepel::geom_text_repel(data = Uganda_master, aes(lon, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4, family = 'Avenir', segment.color = "#333333")

risk_map2 <- 
  risk_map +
  ggrepel::geom_text_repel(data = Uganda_master, aes(lon, lat, label = label2), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")


#===================================================================#
#   plot labels (only where TS studies occured at district-level)   #

return(list(risk_map1, risk_map2))

} 

#=================================================================================#
#      Function: Plotting average risk scores on UGA map - minus water bodies     #


plot_UGA_avg.risk.zones_func2 <- function(Uganda_dist, risk_overlay, risk_map){
  
  # transform co-ordinates and test plot #
  Uganda_dist_latlon <- sp::spTransform(Uganda_dist, CRS("+proj=longlat +datum=WGS84"))
  plot(Uganda_dist_latlon, axes=TRUE)
  points(x=31.76828, y=3.22909063, col = "red", pch=1, bg = "red")
  
  # to get centroids for each district #
  Uganda_dist_centroids <- coordinates(sp::spTransform(Uganda_dist, CRS("+proj=longlat +datum=WGS84"))) # trasnform spatial obj to lat/lon data
  
  Uganda_dist_centroids_df <- as.data.frame(Uganda_dist_centroids)
  
  vector_dist <- as.character(Uganda_dist_latlon$DISTRICT) # make a vector of district names to supply a column with these
  
  Uganda_dist_centroids_df$district <- vector_dist # add this vector to the centroids dataframe
  
  Uganda_dist_centroids_df <- 
    Uganda_dist_centroids_df %>% 
    dplyr::rename(
      lon = V1,
      lat = V2
    )
  
  Uganda_master <- cbind(Uganda_dist_centroids_df, risk_overlay)
  
  Uganda_master <- within(Uganda_master,  label <- paste(recoded_districts_chr, mean_risk_score, mode_risk_score, sep="; ")) # make a column for a label
  Uganda_master <- within(Uganda_master,  label2 <- paste(recoded_districts_chr, mode_risk_score, sep="; ")) # make a column for a label
  
  
  labels_to_remove <- c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule") # districts where no MDA has occured
  
  # remove these districts (make NA in label)
  Uganda_master$label[Uganda_master$recoded_districts %in% c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule")] <- NA
  Uganda_master$label2[Uganda_master$recoded_districts %in% c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule")] <- NA
  
  # ==========================================================================#
  # need to locate water bodies and remove co-ordinates from analysis for UGA #
  
  URL <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_lakes.zip"
  
  fil <- basename(URL)
  if (!file.exists(fil)) download.file(URL, fil)
  fils <- unzip(fil)
  lakes <- readOGR(grep("shp$", fils, value=TRUE), "ne_50m_lakes",
                   stringsAsFactors=FALSE, verbose=FALSE)
  
  lakes_sf <- sf::st_as_sf(lakes) # make sf spatial object for joining
  
  # for plotting the lakes # 
  lakes_UGA <- lakes_sf %>% dplyr::select(name, name_en)
  
  lakes_UGA <- lakes_UGA %>% filter(name %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward"))
  #=============================================== #
  #   plot labels on risk map                      #
  
  risk_map1 <- 
    risk_map +
    geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue", alpha = 0.8) + 
    ggrepel::geom_text_repel(data = Uganda_master, aes(lon, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4, family = 'Avenir', segment.color = "#333333")
  
  risk_map2 <- 
    risk_map +
    geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue", alpha = 0.8) + 
    ggrepel::geom_text_repel(data = Uganda_master, aes(lon, lat, label = label2), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")
  
  return(list(risk_map1, risk_map2))

} 


#====================================================================================================#
#      Function: Plotting average risk scores on UGA map  - for plotting TS studies (district-level) #


plot_UGA_avg.risk.zones_func3 <- function(Uganda_dist, risk_overlay, risk_map, PCC_survey_years, TS_data){
  
  # transform co-ordinates and test plot #
  Uganda_dist_latlon <- sp::spTransform(Uganda_dist, CRS("+proj=longlat +datum=WGS84"))
  plot(Uganda_dist_latlon, axes=TRUE)
  points(x=31.76828, y=3.22909063, col = "red", pch=1, bg = "red")
  
  # to get centroids for each district #
  Uganda_dist_centroids <- coordinates(sp::spTransform(Uganda_dist, CRS("+proj=longlat +datum=WGS84"))) # trasnform spatial obj to lat/lon data
  
  Uganda_dist_centroids_df <- as.data.frame(Uganda_dist_centroids)
  
  vector_dist <- as.character(Uganda_dist_latlon$DISTRICT) # make a vector of district names to supply a column with these
  
  Uganda_dist_centroids_df$district <- vector_dist # add this vector to the centroids dataframe
  
  Uganda_dist_centroids_df <- 
    Uganda_dist_centroids_df %>% 
    dplyr::rename(
      lon = V1,
      lat = V2
    )
  
  Uganda_master <- cbind(Uganda_dist_centroids_df, risk_overlay)
  
  Uganda_master <- within(Uganda_master,  label <- paste(recoded_districts_chr, mean_risk_score, mode_risk_score, sep="; ")) # make a column for a label
  Uganda_master <- within(Uganda_master,  label2 <- paste(recoded_districts_chr, mode_risk_score, sep="; ")) # make a column for a label
  
  Uganda_master2 <- Uganda_master
  
  labels_to_remove <- c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule") # districts where no MDA has occured
  
  # remove these districts (make NA in label)
  Uganda_master$label[Uganda_master$recoded_districts %in% c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule")] <- NA
  Uganda_master$label2[Uganda_master$recoded_districts %in% c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule")] <- NA
  
  #=============================================== #
  #   plot labels on risk map                      #
  
  risk_map1 <- 
    risk_map +
    ggrepel::geom_text_repel(data = Uganda_master, aes(lon, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4, family = 'Avenir', segment.color = "#333333")
  
  risk_map2 <- 
    risk_map +
    ggrepel::geom_text_repel(data = Uganda_master, aes(lon, lat, label = label2), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")
  
  
  #===================================================================#
  #   plot labels (only where TS studies occured at district-level)   #
  
  
  if(PCC_survey_years == "2002-2005"){
    Uganda_master2$dist_to_plot[Uganda_master2$recoded_districts %in% c("Lira", "Kamuli")] <- "plot"
    Uganda_master2 <- subset(Uganda_master2, dist_to_plot == "plot")
  }
  
  # if(PCC_survey_years == "2006-2010"){
  #   Uganda_master2$dist_to_plot[Uganda_master2$recoded_districts %in% c("Masaka", "Lira", "Kibale",
  #                                                                       "Kayunga", "Kamuli", "Kaberamaido",
  #                                                                       "Busia", "Arua", "Apac")] <- "plot"
  #   Uganda_master2 <- subset(Uganda_master2, dist_to_plot == "plot")
  # }
  
  if(PCC_survey_years == "2011-2015"){
    Uganda_master2$dist_to_plot[Uganda_master2$recoded_districts %in% c("Masaka", "Kamuli", "Mukono", "Soroti",
                                                                        "Lira", "Moyo", "Kibale", "Kayunga", 
                                                                        "Kaberamaido", "Busia", "Arua", "Apac")] <- "plot"
    Uganda_master2 <- subset(Uganda_master2, dist_to_plot == "plot")
    
    Uganda_master2$lat <- ifelse(Uganda_master2$recoded_districts == "Mukono", 0.180205, Uganda_master2$lat)
  }
  
  if(PCC_survey_years == "2016-2020"){

    Uganda_master2$dist_to_plot[Uganda_master2$recoded_districts %in% c("Kumi", "Apac", "Kampala", "Kamuli",
                                                                        "Katakwi", "Kayunga", "Lira", "Luwero",
                                                                        "Masaka", "Mpigi", "Mubende", "Mukono",
                                                                        "Nakasongola", "Pallisa", "Soroti", "Wakiso")] <- "plot"
    Uganda_master2 <- subset(Uganda_master2, dist_to_plot == "plot")
    
    Uganda_master2$lat <- ifelse(Uganda_master2$recoded_districts == "Mukono", 0.180205, Uganda_master2$lat)
  }
  
  
  
  # now match centroid longitude & latitude data to TS prev data for each district
  Uganda_dist_centroids_df$district <- str_to_title(Uganda_dist_centroids_df$district) # make district names in centroid data first letter capital case
  
  if(PCC_survey_years == "2002-2005"){
  TS_data_subset <- subset(TS_data, Year < 2006) # subset studies before 2006 for this period
  }
  
  if(PCC_survey_years == "2006-2010"){
    TS_data_subset <- subset(TS_data, Year < 2011) # subset studies before 2012 for this period
    TS_data_subset <- subset(TS_data_subset, Year > 2005) # after 2005
  }
  
  if(PCC_survey_years == "2011-2015"){
    TS_data_subset <- subset(TS_data, Year < 2016) # subset studies before 2012 for this period
    TS_data_subset <- subset(TS_data_subset, Year > 2010) # after 2005
  }
  
  if(PCC_survey_years == "2016-2020"){
    TS_data_subset <- subset(TS_data, Year < 2020) # subset studies before 2012 for this period
    TS_data_subset <- subset(TS_data_subset, Year > 2015) # after 2005
  }
  
  to_match <- as.character(unique(TS_data_subset$District)) # character vector to match with districts in centroid data

  Uganda_dist_centroids_df$district_to_select <- ifelse(Uganda_dist_centroids_df$district %in% to_match, "yes","no") # make column stating whether match in centroid data (i.e. yes have TS data for this district)
  
  Uganda_dist_centroids_df_subset <- subset(Uganda_dist_centroids_df, district_to_select == "yes") # subset centorid data on districts with TS data
  
  Uganda_dist_centroids_df_subset <- Uganda_dist_centroids_df_subset %>% 
    rename(
      District = district,
    ) # rename so can use full join
  
  TS_data_coords_df <- dplyr::full_join(Uganda_dist_centroids_df_subset, TS_data_subset, by = "District") # join centroid + TS dataframes
  
  TS_data_coords_df$lat <- ifelse(TS_data_coords_df$District == "Mukono", 0.180205, TS_data_coords_df$lat)
  
  risk_map3 <- 
    risk_map +
    ggrepel::geom_text_repel(data = Uganda_master2, aes(lon, lat, label = label2), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
    geom_point(data = TS_data_coords_df, aes(x= lon, y = lat, size = Prevalence, shape = Type), fill = "yellow", colour = "black", alpha = 0.75)+
    scale_size_continuous(name="PCC prevalence",
                          breaks = c(5, 10, 15, 20, 30, 40, 50, 60, 90),
                          limits = c(0,94))+
    scale_shape_manual(name = "Adjusted or not",
                       values=c(21,24))
  
  return(list(risk_map1, risk_map2, risk_map3, TS_data_coords_df))
  
} 


#===========================================================================================================================#
#      Function: Plotting average risk scores on UGA map - minus water bodies  - for plotting TS studies (district-level)   #


plot_UGA_avg.risk.zones_func4 <- function(Uganda_dist, risk_overlay, risk_map, PCC_survey_years, TS_data, UGA_map){
  
  # transform co-ordinates and test plot #
  Uganda_dist_latlon <- sp::spTransform(Uganda_dist, CRS("+proj=longlat +datum=WGS84"))
  plot(Uganda_dist_latlon, axes=TRUE)
  points(x=31.76828, y=3.22909063, col = "red", pch=1, bg = "red")
  
  # to get centroids for each district #
  Uganda_dist_centroids <- coordinates(sp::spTransform(Uganda_dist, CRS("+proj=longlat +datum=WGS84"))) # trasnform spatial obj to lat/lon data
  
  Uganda_dist_centroids_df <- as.data.frame(Uganda_dist_centroids)
  
  vector_dist <- as.character(Uganda_dist_latlon$DISTRICT) # make a vector of district names to supply a column with these
  
  Uganda_dist_centroids_df$district <- vector_dist # add this vector to the centroids dataframe
  
  Uganda_dist_centroids_df <- 
    Uganda_dist_centroids_df %>% 
    dplyr::rename(
      lon = V1,
      lat = V2
    )
  
  Uganda_dist_centroids_all <- Uganda_dist_centroids_df
  
  Uganda_master <- cbind(Uganda_dist_centroids_df, risk_overlay)
  
  Uganda_master <- within(Uganda_master,  label <- paste(recoded_districts_chr, mean_risk_score, mode_risk_score, sep="; ")) # make a column for a label
  Uganda_master <- within(Uganda_master,  label2 <- paste(recoded_districts_chr, mode_risk_score, sep="; ")) # make a column for a label
  
  Uganda_master2 <- Uganda_master
  
  labels_to_remove <- c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule") # districts where no MDA has occured
  
  # remove these districts (make NA in label)
  Uganda_master$label[Uganda_master$recoded_districts %in% c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule")] <- NA
  Uganda_master$label2[Uganda_master$recoded_districts %in% c("Kampala","Kiboga","Kotido", "Kyenjojo", "Luweero", "Moroto","Ntungamo", "Sembabule")] <- NA
  
  # ==========================================================================#
  # need to locate water bodies and remove co-ordinates from analysis for UGA #
  
  URL <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_lakes.zip"
  
  fil <- basename(URL)
  if (!file.exists(fil)) download.file(URL, fil)
  fils <- unzip(fil)
  lakes <- readOGR(grep("shp$", fils, value=TRUE), "ne_50m_lakes",
                   stringsAsFactors=FALSE, verbose=FALSE)
  
  lakes_sf <- sf::st_as_sf(lakes) # make sf spatial object for joining
  
  # for plotting the lakes # 
  lakes_UGA <- lakes_sf %>% dplyr::select(name, name_en)
  
  lakes_UGA <- lakes_UGA %>% filter(name %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward"))
  #=============================================== #
  #   plot labels on risk map                      #
  
  risk_map1 <- 
    risk_map +
    geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue", alpha = 0.8) + 
    ggrepel::geom_text_repel(data = Uganda_master, aes(lon, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4, family = 'Avenir', segment.color = "#333333")
  
  risk_map2 <- 
    risk_map +
    geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue", alpha = 0.8) + 
    ggrepel::geom_text_repel(data = Uganda_master, aes(lon, lat, label = label2), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")
  
  #===================================================================#
  #   plot labels (only where TS studies occured at district-level)   #
  
  
  if(PCC_survey_years == "2002-2005"){
    Uganda_master2$dist_to_plot[Uganda_master2$recoded_districts %in% c("Lira", "Kamuli")] <- "plot"
    Uganda_master2 <- subset(Uganda_master2, dist_to_plot == "plot")
  }
  
  # if(PCC_survey_years == "2006-2010"){
  #   Uganda_master2$dist_to_plot[Uganda_master2$recoded_districts %in% c("Masaka", "Lira", "Kibale",
  #                                                                       "Kayunga", "Kamuli", "Kaberamaido",
  #                                                                       "Busia", "Arua", "Apac")] <- "plot"
  #   Uganda_master2 <- subset(Uganda_master2, dist_to_plot == "plot")
  # }
  
  if(PCC_survey_years == "2011-2015"){
    Uganda_master2$dist_to_plot[Uganda_master2$recoded_districts %in% c("Masaka", "Kamuli", "Mukono", "Soroti",
                                                                        "Lira", "Moyo", "Kibale", "Kayunga", 
                                                                        "Kaberamaido", "Busia", "Arua", "Apac", "Soroti")] <- "plot"
    Uganda_master2 <- subset(Uganda_master2, dist_to_plot == "plot")
    
    Uganda_master2$lat <- ifelse(Uganda_master2$recoded_districts == "Mukono", 0.180205, Uganda_master2$lat)
  }
  
  if(PCC_survey_years == "2016-2020"){
    
    Uganda_master2$dist_to_plot[Uganda_master2$recoded_districts %in% c("Kumi", "Apac", "Kampala", "Kamuli",
                                                                        "Katakwi", "Kayunga", "Lira", "Luweero",
                                                                        "Masaka", "Mpigi", "Mubende", "Mukono",
                                                                        "Nakasongola", "Pallisa", "Soroti", "Wakiso",
                                                                        "Gulu")] <- "plot"
    Uganda_master2 <- subset(Uganda_master2, dist_to_plot == "plot")
    
    Uganda_master2$lat <- ifelse(Uganda_master2$recoded_districts == "Mukono", 0.180205, Uganda_master2$lat)
  }
  
  
  
  # now match centroid longitude & latitude data to TS prev data for each district
  Uganda_dist_centroids_df$district <- str_to_title(Uganda_dist_centroids_df$district) # make district names in centroid data first letter capital case
  
  if(PCC_survey_years == "2002-2005"){
    TS_data_subset <- subset(TS_data, Year < 2006) # subset studies before 2006 for this period
  }
  
  if(PCC_survey_years == "2006-2010"){
    TS_data_subset <- subset(TS_data, Year < 2011) # subset studies before 2012 for this period
    TS_data_subset <- subset(TS_data_subset, Year > 2005) # after 2005
  }
  
  if(PCC_survey_years == "2011-2015"){
    TS_data_subset <- subset(TS_data, Year < 2016) # subset studies before 2012 for this period
    TS_data_subset <- subset(TS_data_subset, Year > 2010) # after 2005
  }
  
  if(PCC_survey_years == "2016-2020"){
    TS_data_subset <- subset(TS_data, Year < 2020) # subset studies before 2012 for this period
    TS_data_subset <- subset(TS_data_subset, Year > 2015) # after 2005
  }
  
  to_match <- as.character(unique(TS_data_subset$Old_districts)) # character vector to match with districts in centroid data
  
  Uganda_dist_centroids_df$district_to_select <- ifelse(Uganda_dist_centroids_df$district %in% to_match, "yes","no") # make column stating whether match in centroid data (i.e. yes have TS data for this district)
  
  Uganda_dist_centroids_df_subset <- subset(Uganda_dist_centroids_df, district_to_select == "yes") # subset centorid data on districts with TS data
  
  Uganda_dist_centroids_df_subset <- Uganda_dist_centroids_df_subset %>% 
    rename(
      District = district,
    ) # rename so can use full join
  
  TS_data_coords_df <- dplyr::full_join(Uganda_dist_centroids_df_subset, TS_data_subset, by = "District") # join centroid + TS dataframes
  
  TS_data_coords_df$lat <- ifelse(TS_data_coords_df$District == "Mukono", 0.243376, TS_data_coords_df$lat)
  
  risk_map3 <- 
    risk_map +
    geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue", alpha = 0.8) +
    ggrepel::geom_text_repel(data = Uganda_master2, aes(lon, lat, label = label2), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
    geom_point(data = TS_data_coords_df, aes(x= lon, y = lat, size = Prevalence, shape = Type), 
               fill = "yellow", colour = "black", alpha = 0.75, position = position_jitter(h=0.1,w=0.11))+
    scale_size_continuous(name="PCC prevalence",
                          breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 90),
                          limits = c(0,94),
                          range = c(1.75,6))+
    scale_shape_manual(name = "Adjusted or not",
                       values=c(21,24,25))
  
  #========================================================#
  #  Geographic outline of Uganda - to capture isalnds etc #
  # https://maps.princeton.edu/catalog/stanford-fh022bz4757 
  UGAmap <- fortify(UGA_map)
  
  risk_map4 <- 
    risk_map +
    #geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue", alpha = 0.8) +
    geom_polygon(data = UGAmap, aes(x = long, y = lat, group = group), colour="blue", fill="blue", alpha=0.2)+
    ggrepel::geom_text_repel(data = Uganda_master2, aes(lon, lat, label = label2), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
    geom_point(data = TS_data_coords_df, aes(x= lon1, y = lat1, size = Prevalence, shape = Type, colour = SC_risk, stroke = stroke), 
               fill = "yellow", alpha = 0.75)+
    scale_size_continuous(name="PCC prevalence",
                          breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 90),
                          limits = c(0,94),
                          range = c(1.75,6))+
    scale_shape_manual(name = "Adjusted or not",
                       values=c(21,24,25), na.translate = F)+
    scale_color_manual(name = "District-level prevalence or sub-county
available (colour and thickness around point)",
                       values = c("black","red4"), na.translate = F)
  #coord_sf(xlim=c(29.2,35.05),ylim=c(-1.5439,4.3), expand = FALSE)
  
  
  return(list(risk_map1, risk_map2, risk_map3, risk_map4, TS_data_coords_df, Uganda_dist_centroids_all))
  
  
} 

#=========================================================================================================#
#    function to i) overlay sub-district locations of MDAs & 
# ii) find subdistrict locations that most closely match a lat/lon datapoint with an attached risk factor

find_subdistrictMDA_location_match_riskzone_func <- function(Sub_district_MDA_locations_df, overlay, avg.risk.map){
  
  # overlay 2004 MDA sub-district location data on 2001-2005 PCC risk map
  plot_overlay <- overlay[[2]] + geom_point(data=Sub_district_MDA_locations_df, 
                                 aes(x=Lon, y=Lat), colour="Black", 
                                 fill="Purple",pch=21, size=5, alpha=I(0.8))
  
  
  plot_overlay2 <- avg.risk.map[[2]] + geom_point(data=Sub_district_MDA_locations_df, 
                                      aes(x=Lon, y=Lat), colour="Black", 
                                      fill="White",pch=21, size=5, alpha=I(0.9))
  
  # extract lat, lon and risk factor values to match
  
  Risk_factor_extracted_df <- data.frame(lat = overlay[[3]]$y, lon = overlay[[3]]$x, RF_value = overlay[[3]]$value,
                                              RF_cat = overlay[[3]]$risk_fact_bins)
  
  #==================================#
  #  Now match closest lat/lons      #
  
  # solution from: https://stackoverflow.com/questions/45625041/matching-data-frames-based-on-shortest-geographic-distance 
  
  # Create ID for my_df_1 and my_df_2 based on row id
  # This step is not required, just help me to better distinguish each point
  df_1 <- Sub_district_MDA_locations_df %>% mutate(ID1 = row.names(.))
  df_2 <- Risk_factor_extracted_df %>% mutate(ID2 = row.names(.))
  
  # Create spatial point data frame
  df_1_sp <- df_1
  coordinates(df_1_sp) <- ~Lon + Lat
  
  df_2_sp <- df_2
  coordinates(df_2_sp) <- ~lon + lat
  
  # Convert to simple feature
  df_1_sf <- sf::st_as_sf(df_1_sp)
  df_2_sf <- sf::st_as_sf(df_2_sp)
  
  # Set projection based on the epsg code
  sf::st_crs(df_1_sf) <- 4326
  sf::st_crs(df_2_sf) <- 4326
  
  # Calculate the distance
  m_dist <- sf::st_distance(df_1_sf, df_2_sf)
  
  # Filter for the nearest
  near_index <- apply(m_dist, 1, order)[1, ]
  
  # Based on the index in near_index to select the rows in df_2
  # Combine with df_1
  df_final <- cbind(df_1, df_2[near_index, ])
  
  df_final
  
  #===================================================================================================================#
  # Where there are >1 MDA locations for a given district, estimate average risk scaore across sub-district MDA sites #
  
  mode <- function(codes){
    which.max(tabulate(codes))
  }
  
  df_final$District <- as.factor(df_final$District)
  
  df_final2 <- dplyr::group_by(df_final, District) %>% dplyr::summarize(mean_RF_value = mean(RF_value),
                                                                        median_RF_value = median(RF_value),
                                                                        mode_RF_value = mode(RF_value))

  df_final2 <- as.data.frame(df_final2)
  
  df_final2  <- 
    df_final2 %>%
    dplyr::mutate(mean_risk_score_subdistrict = cut(mean_RF_value,
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  # better representation with small sample size (or still biased?)
  df_final2  <- 
    df_final2 %>%
    dplyr::mutate(median_risk_score_subdistrict = cut(median_RF_value,
                                                    breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                                    labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  # mode doesnt make much sense with small sample sizes
  df_final2  <- 
    df_final2 %>%
    dplyr::mutate(mode_risk_score_subdistrict = cut(mode_RF_value,
                                                    breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                                    labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  
  return(list(plot_overlay, plot_overlay2, df_final, df_final2))
  
}


#==============================================================================#
#        Average risk factor per sub-county (area) recieving MDA function      #


average_risk_subcounties_func <- function(RF_data, subcounties, subcounty_MDA_data, scnames, UGA_subcounties_tidy_subset, 
                                          UGA_subcounties_tidy, districts_2001, UGA_districts_tidy_subset, year){

  RF_data_plotting <- RF_data # maintain original risk factor overlay for plotting later
  
  #==================================================================================#
  # combine spatial objects: risk factor values across UGA & sub-county spatial data #
  
  if(year == 2004){
    data <- overlay_2001[[3]]
  }
  
  if(year == 2012 || year == 2015){
    data <- overlay_2011[[3]]
  }
  
  if(year == 2016 || year == 2018){
    data <- overlay_2016[[3]]
  }
  
  # extract lat and longitutdes
  RF_data$long <- data$x
  RF_data$lat <- data$y
  
  # make lat & lon dataframe into spatial object (sp)
  xy <- RF_data[,c(1,2)]
  
  spdf <- SpatialPointsDataFrame(coords = xy, data = data,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  spdf2 <- sf::st_as_sf(spdf) # convert to sp object
  
  #=================================#
  # do the same for sub-county data #

  subcounty_MDA_data <- UGA_subcounties_tidy_subset
  
  if(year == 2004){
  # make into an sf (spatial) object for joining
  spdf_sc2 <- sfheaders::sf_polygon(
    obj = subcounty_MDA_data
    , x = "long"
    , y = "lat"
    , polygon_id = "SNAME_2006"
    )
  }
  
  if(year == 2012){
    # make into an sf (spatial) object for joining
    spdf_sc2 <- sfheaders::sf_polygon(
      obj = subcounty_MDA_data
      , x = "long"
      , y = "lat"
      , polygon_id = "SNAME_2010"
    )
  }
  
  
  if(year == 2015 || year == 2016 || year == 2018){
    # make into an sf (spatial) object for joining

    subcounty_MDA_data <- with(subcounty_MDA_data,  subcounty_MDA_data[order(Subcounty) , ])
    
    spdf_sc2 <- sfheaders::sf_polygon(
      obj = subcounty_MDA_data
      , x = "long"
      , y = "lat"
      , polygon_id = "Subcounty"
    )
  }
  
  # if(year == 2015){
  #   # make into an sf (spatial) object for joining
  #   subcounty_MDA_data <- transform(subcounty_MDA_data,                         
  #                                   polygon_id = as.numeric(factor(Subcounty))) # Create ID by group
  #   
  #   subcounty_MDA_data$linestring_id <- subcounty_MDA_data$polygon_id
  #   
  #   subcounty_MDA_data$x <- subcounty_MDA_data$long
  #   subcounty_MDA_data$y <- subcounty_MDA_data$lat
  # 
  #   spdf_sc2 <- sfheaders::sf_poly(subcounty_MDA_data, keep = TRUE)
  # }

  # if(year == 2015){
  #   spdf_sc2 <- sf::st_as_sf(subcounty_MDA_data)
  # }
  
  
  sf::st_crs( spdf_sc2 ) <- 4326 # WGS84 (EPSG: 4326)
  
 
  #=================================#
  
  master2 <- sf::st_join(spdf2, spdf_sc2) # join risk factor data and admin data
  
  master_lake <- sf::st_join(spdf2, spdf_sc2) # join risk factor data and admin data - this is for the lake data later
  
  # test average risk score
  if(year == 2004){
  master2$subcounty_factor_test <- as.factor(master2$SNAME_2006)
  }
  
  if(year == 2012){
    master2$subcounty_factor_test <- as.factor(master2$SNAME_2010)
  }
  
  if(year == 2015 || year == 2016 || year == 2018){
    master2$subcounty_factor_test <- as.factor(master2$Subcounty)
  }
  
  sf::st_geometry(master2) <- NULL
  
  master3 <- master2 %>% tidyr::drop_na(value)
  
  # ================================#
  # create risk scores per district #
  risk_score <- master3 %>% 
    group_by(subcounty_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value))
  
  mymode <- function(x) {
    t <- table(x)
    names(t)[ which.max(t) ]
    }
  
  risk_score <- master3 %>% 
    group_by(subcounty_factor_test) %>% 
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
  
  #====================================================================================#
  # WHERE >1 sub-county per district, calculate average risk score across sub-counties #
  
    # make original districts (2001) as sf sptail dataframe to merge
  spdf_dist <- sf::st_as_sf(districts_2001)
  spdf_dist <- sf::st_set_crs(spdf_dist, 4326)
  
  spdf_tojoin <- sf::st_as_sf(master_lake)
  spdf_tojoin <- sf::st_set_crs(spdf_tojoin, 4326)
  
  master_dist <- sf::st_join(spdf_tojoin, spdf_dist)
  
  # test average risk score
  
  master_dist$district_factor_test <- as.factor(master_dist$DISTRICT)
  
  master_dist_tosave <- master_dist
  
  sf::st_geometry(master_dist) <- NULL
  
  master_dist2 <- master_dist %>% tidyr::drop_na(value)
  
  if(year == 2004){
    master_dist2 <- master_dist2 %>% tidyr::drop_na(SNAME_2006)
  }
  
  if(year == 2012){
    master_dist2 <- master_dist2 %>% tidyr::drop_na(SNAME_2010)
  }
  
  if(year == 2015 || year == 2016 || year == 2018){
    master_dist2 <- master_dist2 %>% tidyr::drop_na(Subcounty)
  }
  
  # ================================#
  # create risk scores per district #
  risk_score_dist <- master_dist2 %>% 
    group_by(district_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value))
  
  risk_score_dist <- master_dist2 %>% 
    group_by(district_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value),
                     mode_risk = mymode(value))
  
  risk_score_dist$mean_risk <- risk_score_dist$risk/risk_score_dist$n
  
  risk_score_dist <- 
    risk_score_dist %>%
    mutate(mean_risk_score = cut(mean_risk,
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  risk_score_dist <- 
    risk_score_dist %>%
    mutate(mode_risk_score = cut(as.numeric(mode_risk),
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))

  #=========================================#
  #          PLOTTING                       #

value_rf <- c("grey90", "gold", "darkorchid1", "red1", "springgreen", "darkorange1", "pink1", "tan4")

# make labels (sub-counties with MDA) for plotting
risk_score_dist2 <- na.omit(risk_score_dist)

  dnames <- aggregate(cbind(x, y) ~ DISTRICT, data=master_dist2, FUN=mean)
  dnames$label <- dnames$DISTRICT
  dnames$risk_factor <- risk_score_dist2$mode_risk_score
  dnames <- within(dnames,  label1 <- paste(DISTRICT, risk_factor, sep="; "))
  dnames <- dnames %>% dplyr::rename(long = x, lat = y)

if(year == 2004){
plot1 <- ggplot() +
  geom_tile(data = RF_data_plotting, 
            aes(x = x, y = y, fill = risk_fact_bins)) +
  geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
  geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.2, fill=NA, alpha=NA)+
  scale_fill_manual(name = "Class",
                    values = value_rf, 
                    na.value = NA, na.translate = FALSE)+
  scale_colour_manual(values=c("black",NA), guide=FALSE)+
  coord_equal() +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  cowplot::panel_border(remove = TRUE) +
  ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")

plot2 <- ggplot() +
  geom_tile(data = RF_data_plotting, 
            aes(x = x, y = y, fill = risk_fact_bins)) +
  geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
  geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.2, fill=NA, alpha=NA)+
  scale_fill_manual(name = "Class",
                    values = value_rf, 
                    na.value = NA, na.translate = FALSE)+
  scale_colour_manual(values=c("black",NA), guide=FALSE)+
  coord_equal() +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  cowplot::panel_border(remove = TRUE) +
  ggrepel::geom_text_repel(data = dnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")


plot3 <- ggplot() +
  geom_tile(data = RF_data_plotting, 
            aes(x = x, y = y, fill = risk_fact_bins)) +
  geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
  geom_polygon(data= UGA_districts_tidy_subset, aes(x = long, y = lat, group = group), colour= "grey45", size = 1, alpha=0.25, fill=NA)+
  geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.2, fill=NA, alpha=NA, linetype = "longdash")+
  scale_fill_manual(name = "Class",
                    values = value_rf, 
                    na.value = NA, na.translate = FALSE)+
  scale_colour_manual(values=c("black",NA), guide=FALSE)+
  coord_equal() +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  cowplot::panel_border(remove = TRUE) +
  ggrepel::geom_text_repel(data = dnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")


}

if(year == 2012 || year == 2015 || year == 2016 || year == 2018){
  plot1 <- ggplot() +
    geom_tile(data = RF_data_plotting, 
              aes(x = x, y = y, fill = risk_fact_bins)) +
    geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
    geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.2, fill=NA, alpha=NA)+
    scale_fill_manual(name = "Class",
                      values = value_rf, 
                      na.value = NA, na.translate = FALSE)+
    scale_colour_manual(values=c("black",NA), guide=FALSE)+
    coord_equal() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
    ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 2, family = 'Avenir', segment.color = "#333333", fontface = "bold")
  
  plot2 <- ggplot() +
    geom_tile(data = RF_data_plotting, 
              aes(x = x, y = y, fill = risk_fact_bins)) +
    geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
    geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.2, fill=NA, alpha=NA)+
    scale_fill_manual(name = "Class",
                      values = value_rf, 
                      na.value = NA, na.translate = FALSE)+
    scale_colour_manual(values=c("black",NA), guide=FALSE)+
    coord_equal() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
    ggrepel::geom_text_repel(data = dnames, aes(long, lat, label = label1), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")

  
  plot3 <- ggplot() +
    geom_tile(data = RF_data_plotting, 
              aes(x = x, y = y, fill = risk_fact_bins)) +
    geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
    geom_polygon(data= UGA_districts_tidy_subset, aes(x = long, y = lat, group = group), colour= "grey45", size = 1, alpha=0.25,fill=NA)+
    geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.2, fill=NA, alpha=NA, linetype = "longdash")+
    scale_fill_manual(name = "Class",
                      values = value_rf, 
                      na.value = NA, na.translate = FALSE)+
    scale_colour_manual(values=c("black",NA), guide=FALSE)+
    coord_equal() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
    ggrepel::geom_text_repel(data = dnames, aes(long, lat, label = label1), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")
  
  
  }

return(list(risk_score, risk_score_dist, plot1, plot2, master2, master_lake, master_dist_tosave, dnames, plot3))

}


#=============================================================================================#
#    Risk factor analysis for sub-counties, exlcuding data points in water bodies: function   #

average_risk_subcounties_func2 <- function(data_to_join, data_to_join2, RF_data_plotting, UGA_subcounties_tidy, scnames, year){ 
  
  # ==========================================================================#
  
  # need to locate water bodies and remove co-ordinates from analysis for UGA #
  
  URL <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_lakes.zip"
  
  fil <- basename(URL)
  if (!file.exists(fil)) download.file(URL, fil)
  fils <- unzip(fil)
  lakes <- readOGR(grep("shp$", fils, value=TRUE), "ne_50m_lakes",
                  stringsAsFactors=FALSE, verbose=FALSE)

  lakes_sf <- sf::st_as_sf(lakes) # make sf spatial object for joining
  
  lakes_sf2  <- lakes_sf  %>% dplyr::select(name, name_en) # just select name columns
  
  # ============================================================#
  # 1) calculate score (minus water bodies) for each sub-county #
  
  master_lakes <- sf::st_join(data_to_join, lakes_sf2) # join the two sf objects
  
  master_lakes_filtered <- master_lakes %>% filter(!name %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward")) # removes any observation including lakes
  
  # test average risk score
  if(year == 2004){
  master_lakes_filtered$subcounty_factor_test <- as.factor(master_lakes_filtered$SNAME_2006)
  }
  
  if(year == 2012){
    master_lakes_filtered$subcounty_factor_test <- as.factor(master_lakes_filtered$SNAME_2010)
  }
  
  if(year == 2015 || year == 2016 || year == 2018){
    master_lakes_filtered$subcounty_factor_test <- as.factor(master_lakes_filtered$Subcounty)
  }
  
  sf::st_geometry(master_lakes_filtered) <- NULL
  
  master_lakes_filtered2 <- master_lakes_filtered %>% tidyr::drop_na(value)
  
  # create risk scores per sub-county
  
  risk_score_nolakes <- master_lakes_filtered2 %>% 
    group_by(subcounty_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value))
  
   mymode <- function(x) {
    t <- table(x)
    names(t)[ which.max(t) ]
  }
  
  risk_score_nolakes <- master_lakes_filtered2 %>% 
    group_by(subcounty_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value),
                   mode_risk = mymode(value))
  
  risk_score_nolakes$mean_risk <- risk_score_nolakes$risk/risk_score_nolakes$n
  
  risk_score_nolakes <- 
    risk_score_nolakes %>%
    mutate(mean_risk_score = cut(mean_risk,
                               breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                               labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  risk_score_nolakes <- 
    risk_score_nolakes %>%
    mutate(mode_risk_score = cut(as.numeric(mode_risk),
                               breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                               labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))

  # =============================================================================#
  # 2) calculate score (minus water bodies) for all sub-counties within dsitrict #
  
  master_lakes_district <- sf::st_join(data_to_join2, lakes_sf2) # join the two sf objects
  
  master_lakes_district_filtered <- master_lakes_district %>% filter(!name %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward")) # removes any observation including lakes
  
  # test average risk score
  #master_lakes_district_filtered$district_factor_test <- as.factor(master_lakes_district_filtered$DISTRICT)
  
  sf::st_geometry(master_lakes_district_filtered) <- NULL
  
  master_lakes_district_filtered2 <- master_lakes_district_filtered %>% tidyr::drop_na(value)
  
  if(year == 2004){
    master_lakes_district_filtered2 <- master_lakes_district_filtered2 %>% tidyr::drop_na(SNAME_2006)
  }
  
  if(year == 2012){
  master_lakes_district_filtered2 <- master_lakes_district_filtered2 %>% tidyr::drop_na(SNAME_2010)
  }
  
  if(year == 2015 || year == 2016 || year == 2018){
    master_lakes_district_filtered2 <- master_lakes_district_filtered2 %>% tidyr::drop_na(Subcounty)
  }
  
  # create risk scores per sub-county
  
  risk_score_district_nolakes <- master_lakes_district_filtered2 %>% 
    group_by(district_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value))
  
 
  risk_score_district_nolakes <- master_lakes_district_filtered2  %>% 
    group_by(district_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value),
                     mode_risk = mymode(value))
  
  risk_score_district_nolakes$mean_risk <- risk_score_district_nolakes$risk/risk_score_district_nolakes$n
  
  risk_score_district_nolakes <- 
    risk_score_district_nolakes %>%
    mutate(mean_risk_score = cut(mean_risk,
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  risk_score_district_nolakes <- 
    risk_score_district_nolakes %>%
    mutate(mode_risk_score = cut(as.numeric(mode_risk),
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  
  # for plotting the lakes # 
  lakes_UGA <- lakes_sf %>% dplyr::select(name, name_en)
  
  lakes_UGA <- lakes_UGA %>% filter(name %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward"))
  
  value_rf <- c("grey90", "gold", "darkorchid1", "red1", "springgreen", "darkorange1", "pink1", "tan4")
  
  if(year == 2004){
  plot1 <- ggplot() +
    geom_tile(data = RF_data_plotting, 
              aes(x = x, y = y, fill = risk_fact_bins)) +
    geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
    geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue") + 
    geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.1, fill=NA, alpha=NA)+
    scale_fill_manual(name = "Class",
                    values = value_rf, 
                    na.value = NA, na.translate = FALSE)+
    scale_colour_manual(values=c("black",NA), guide=FALSE)+
    coord_equal() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
  ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
  coord_sf(xlim = c(29.83, 35.1), ylim = c(-1.5, 4.32))
  }
  
  if(year == 2012 || year == 2015 || year == 2016 || year == 2018){
    plot1 <- ggplot() +
      geom_tile(data = RF_data_plotting, 
                aes(x = x, y = y, fill = risk_fact_bins)) +
      geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
      geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue") + 
      geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.1, fill=NA, alpha=NA)+
      scale_fill_manual(name = "Class",
                        values = value_rf, 
                        na.value = NA, na.translate = FALSE)+
      scale_colour_manual(values=c("black",NA), guide=FALSE)+
      coord_equal() +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.background = element_blank()) +
      cowplot::panel_border(remove = TRUE) +
      ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 2.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
      coord_sf(xlim = c(29.83, 35.1), ylim = c(-1.5, 4.32))
  }
  
  # make NEW labels (sub-counties across districts with MDA) for plotting
  risk_score_nolakes_dist2 <- na.omit(risk_score_district_nolakes)
  
  dnames2 <- aggregate(cbind(x, y) ~ DISTRICT, data=master_lakes_district_filtered2, FUN=mean)
  dnames2$label <- dnames2$DISTRICT
  dnames2$risk_factor <- risk_score_nolakes_dist2$mode_risk_score
  dnames2 <- within(dnames2,  label1 <- paste(DISTRICT, risk_factor, sep="; "))
  dnames2 <- dnames2 %>% dplyr::rename(long = x, lat = y)
  
  plot2 <- ggplot() +
    geom_tile(data = RF_data_plotting, 
              aes(x = x, y = y, fill = risk_fact_bins)) +
    geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
    geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue") + 
    geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.1, fill=NA, alpha=NA)+
    scale_fill_manual(name = "Class",
                      values = value_rf, 
                      na.value = NA, na.translate = FALSE)+
    scale_colour_manual(values=c("black",NA), guide=FALSE)+
    coord_equal() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
    ggrepel::geom_text_repel(data = dnames2, aes(long, lat, label = label1), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
    coord_sf(xlim = c(29.83, 35.1), ylim = c(-1.5, 4.32))
  

return(list(risk_score_nolakes, plot1, risk_score_district_nolakes, plot2, dnames2))

}



#======================================================================================================================#
#    Average risk score function for sub-counties/average across sub-counties for those with PCC studies               #
#======================================================================================================================#


#==============================================================================#
#        Average risk factor per sub-county (area) recieving MDA function      #


average_risk_subcounties_func3 <- function(RF_data, subcounties, subcounty_PCCstudies_data, scnames, UGA_subcounties_tidy_subset, 
                                          UGA_subcounties_tidy, districts_2001, UGA_districts_tidy_subset, year,
                                          Sub_county_TSdata){
  
  RF_data_plotting <- RF_data # maintain original risk factor overlay for plotting later
  
  #==================================================================================#
  # combine spatial objects: risk factor values across UGA & sub-county spatial data #
  
  # if(year == 2004){
  #   data <- overlay_2001[[3]]
  # }
  
  if(year == 2011 || year == 2013 || year == 2015){
    data <- overlay_2011[[3]]
  }
  
  if(year == 2019){
    data <- overlay_2016[[3]]
  }
  
  # extract lat and longitutdes
  RF_data$long <- data$x
  RF_data$lat <- data$y
  
  # make lat & lon dataframe into spatial object (sp)
  xy <- RF_data[,c(1,2)]
  
  spdf <- SpatialPointsDataFrame(coords = xy, data = data,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  spdf2 <- sf::st_as_sf(spdf) # convert to sp object
  
  #=================================#
  # do the same for sub-county data #
  
  subcounty_PCCstudies_data <- UGA_subcounties_tidy_subset
  
  if(year == 2011){
    # make into an sf (spatial) object for joining
    spdf_sc2 <- sfheaders::sf_polygon(
      obj = subcounty_PCCstudies_data
      , x = "long"
      , y = "lat"
      , polygon_id = "SNAME_2010"
    )
  }
  
  
  if(year == 2013 || year == 2015 || year == 2019){
    # make into an sf (spatial) object for joining
    
    subcounty_PCCstudies_data <- with(subcounty_PCCstudies_data,  subcounty_PCCstudies_data[order(Subcounty) , ])
    
    spdf_sc2 <- sfheaders::sf_polygon(
      obj = subcounty_PCCstudies_data
      , x = "long"
      , y = "lat"
      , polygon_id = "Subcounty"
    )
  }
  
 sf::st_crs( spdf_sc2 ) <- 4326 # WGS84 (EPSG: 4326)
  
  
  #=================================#
  
  master2 <- sf::st_join(spdf2, spdf_sc2) # join risk factor data and admin data
  
  master_lake <- sf::st_join(spdf2, spdf_sc2) # join risk factor data and admin data - this is for the lake data later
  
  # test average risk score
 
  if(year == 2011){
    master2$subcounty_factor_test <- as.factor(master2$SNAME_2010)
  }
  
  if(year == 2013 || year == 2015 || year == 2019){
    master2$subcounty_factor_test <- as.factor(master2$Subcounty)
  }
  
  sf::st_geometry(master2) <- NULL
  
  master3 <- master2 %>% tidyr::drop_na(value)
  
  # ================================#
  # create risk scores per district #
  risk_score <- master3 %>% 
    group_by(subcounty_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value))
  
  mymode <- function(x) {
    t <- table(x)
    names(t)[ which.max(t) ]
  }
  
  risk_score <- master3 %>% 
    group_by(subcounty_factor_test) %>% 
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
  
  #====================================================================================#
  # WHERE >1 sub-county per district, calculate average risk score across sub-counties #
  
  # make original districts (2001) as sf sptail dataframe to merge
  spdf_dist <- sf::st_as_sf(districts_2001)
  spdf_dist <- sf::st_set_crs(spdf_dist, 4326)
  
  spdf_tojoin <- sf::st_as_sf(master_lake)
  spdf_tojoin <- sf::st_set_crs(spdf_tojoin, 4326)
  
  master_dist <- sf::st_join(spdf_tojoin, spdf_dist)
  
  # test average risk score
  
  master_dist$district_factor_test <- as.factor(master_dist$DISTRICT)
  
  master_dist_tosave <- master_dist
  
  sf::st_geometry(master_dist) <- NULL
  
  master_dist2 <- master_dist %>% tidyr::drop_na(value)
  
  if(year == 2011){
    master_dist2 <- master_dist2 %>% tidyr::drop_na(SNAME_2010)
  }
  
  if(year == 2013 || year == 2015 || year == 2019){
    master_dist2 <- master_dist2 %>% tidyr::drop_na(Subcounty)
  }
  
  # ================================#
  # create risk scores per district #
  risk_score_dist <- master_dist2 %>% 
    group_by(district_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value))
  
  risk_score_dist <- master_dist2 %>% 
    group_by(district_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value),
                     mode_risk = mymode(value))
  
  risk_score_dist$mean_risk <- risk_score_dist$risk/risk_score_dist$n
  
  risk_score_dist <- 
    risk_score_dist %>%
    mutate(mean_risk_score = cut(mean_risk,
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  risk_score_dist <- 
    risk_score_dist %>%
    mutate(mode_risk_score = cut(as.numeric(mode_risk),
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  #=========================================#
  #          PLOTTING                       #
  
  value_rf <- c("grey90", "gold", "darkorchid1", "red1", "springgreen", "darkorange1", "pink1", "tan4")
  
  # make labels (sub-counties with MDA) for plotting (labels for subcounty risk score)
  risk_score2 <- na.omit(risk_score)
  
  scnames$risk_factor <- risk_score2$mode_risk_score
  scnames <- within(scnames,  label1 <- paste(label, risk_factor, sep="; "))

  # make labels (sub-counties with MDA) for plotting (labels for districts with aggregate sub-county risk score)
  risk_score_dist2 <- na.omit(risk_score_dist)
  
  dnames <- aggregate(cbind(x, y) ~ DISTRICT, data=master_dist2, FUN=mean)
  dnames$label <- dnames$DISTRICT
  dnames$risk_factor <- risk_score_dist2$mode_risk_score
  dnames <- within(dnames,  label1 <- paste(DISTRICT, risk_factor, sep="; "))
  dnames <- dnames %>% dplyr::rename(long = x, lat = y)
  
  if(year == 2011 || year == 2013 || year == 2015 || year == 2019){
    
    # subset Sub-county TS data depending on year
    Sub_county_TSdata$Prev_adjustment <- as.factor(Sub_county_TSdata$type)
    Sub_county_TSdata_subset <- subset(Sub_county_TSdata, study_year == year)

    plot1 <- ggplot() +
      geom_tile(data = RF_data_plotting, 
                aes(x = x, y = y, fill = risk_fact_bins)) +
      geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
      geom_polygon(data= UGA_subcounties_tidy_subset, aes(x = long, y = lat, group = group, colour= PCC_survey), size = 1.2, fill=NA, alpha=NA)+
      # geom_point(data = Sub_county_TSdata_subset, aes(x= long, y = lat, size = prevalence, shape = Prev_adjustment), shape=21, 
      #            fill = "yellow", colour = "black", alpha = 0.75)+
      scale_fill_manual(name = "Class",
                        values = value_rf, 
                        na.value = NA, na.translate = FALSE)+
      scale_colour_manual(values=c("black",NA), guide=FALSE)+
      coord_equal() +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.background = element_blank()) +
      cowplot::panel_border(remove = TRUE) +
      ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label1), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")
    
    type_chr <- as.character(Sub_county_TSdata_subset$type) # make character for legend (based on informed or apparent prev)
    range_toplot <- range(Sub_county_TSdata_subset$prevalence) # prev range to plot in legend for specific survey year
    
    plot1a <- plot1 +
      geom_point(data = Sub_county_TSdata_subset, aes(x= long, y = lat, size = prevalence), shape=21, 
                 fill = "yellow", colour = "black", alpha = 0.75)+
      scale_size_continuous(name=type_chr,
                          breaks = c(5, 10, 15, 20, 30, 40, 50, 60, 90),
                          # labels=c("0-10%", "10.01-25%", "25.01-40%",
                          #            "40.01-60%", ">60%"),
                          limits = c(0,92))
    
    
    plot2 <- ggplot() +
      geom_tile(data = RF_data_plotting, 
                aes(x = x, y = y, fill = risk_fact_bins)) +
      geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
      geom_polygon(data= UGA_subcounties_tidy_subset, aes(x = long, y = lat, group = group, colour= PCC_survey), size = 1.2, fill=NA, alpha=NA)+
      geom_point(data = Sub_county_TSdata_subset, aes(x= long, y = lat, size = prevalence, shape = Prev_adjustment), shape=21, 
                 fill = "yellow", colour = "black", alpha = 0.75)+
      scale_fill_manual(name = "Class",
                        values = value_rf, 
                        na.value = NA, na.translate = FALSE)+
      scale_colour_manual(values=c("black",NA), guide=FALSE)+
      coord_equal() +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.background = element_blank()) +
      cowplot::panel_border(remove = TRUE) +
      ggrepel::geom_text_repel(data = dnames, aes(long, lat, label = label1), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")
    
    
    plot2a <- plot2 +
      geom_point(data = Sub_county_TSdata_subset, aes(x= long, y = lat, size = prevalence), shape=21, 
                 fill = "yellow", colour = "black", alpha = 0.75)+
      scale_size_continuous(name=type_chr,
                            breaks = c(5, 10, 15, 20, 30, 40, 50, 60, 90),
                            # labels=c("0-10%", "10.01-25%", "25.01-40%",
                            #            "40.01-60%", ">60%"),
                            limits = c(0,92))
    
    plot3 <- ggplot() +
      geom_tile(data = RF_data_plotting, 
                aes(x = x, y = y, fill = risk_fact_bins)) +
      geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
      geom_polygon(data= UGA_districts_tidy_subset, aes(x = long, y = lat, group = group), colour= "grey45", size = 1, alpha=0.25,fill=NA)+
      geom_polygon(data= UGA_subcounties_tidy_subset, aes(x = long, y = lat, group = group, colour= PCC_survey), size = 1.2, fill=NA, alpha=NA, linetype = "longdash")+
      scale_fill_manual(name = "Class",
                        values = value_rf, 
                        na.value = NA, na.translate = FALSE)+
      scale_colour_manual(values=c("black",NA), guide=FALSE)+
      coord_equal() +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.background = element_blank()) +
      cowplot::panel_border(remove = TRUE) +
      ggrepel::geom_text_repel(data = dnames, aes(long, lat, label = label1), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")
    
    
  }
  
  return(list(risk_score, risk_score_dist, plot1a, plot2a, master2, master_lake, master_dist_tosave, dnames, plot3))
  
}


#=============================================================================================#
#    Risk factor analysis for sub-counties, exlcuding data points in water bodies: function   #

average_risk_subcounties_func4 <- function(data_to_join, data_to_join2, RF_data_plotting, UGA_subcounties_tidy_subset, scnames, year){ 
  
  # ==========================================================================#
  
  # need to locate water bodies and remove co-ordinates from analysis for UGA #
  
  URL <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_lakes.zip"
  
  fil <- basename(URL)
  if (!file.exists(fil)) download.file(URL, fil)
  fils <- unzip(fil)
  lakes <- readOGR(grep("shp$", fils, value=TRUE), "ne_50m_lakes",
                   stringsAsFactors=FALSE, verbose=FALSE)
  
  lakes_sf <- sf::st_as_sf(lakes) # make sf spatial object for joining
  
  lakes_sf2  <- lakes_sf  %>% dplyr::select(name, name_en) # just select name columns
  
  # ============================================================#
  # 1) calculate score (minus water bodies) for each sub-county #
  
  master_lakes <- sf::st_join(data_to_join, lakes_sf2) # join the two sf objects
  
  master_lakes_filtered <- master_lakes %>% filter(!name %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward")) # removes any observation including lakes
  
  # test average risk score
 if(year == 2011){
    master_lakes_filtered$subcounty_factor_test <- as.factor(master_lakes_filtered$SNAME_2010)
  }
  
  if(year == 2013 || year == 2015 || year == 2019){
    master_lakes_filtered$subcounty_factor_test <- as.factor(master_lakes_filtered$Subcounty)
  }
  
  sf::st_geometry(master_lakes_filtered) <- NULL
  
  master_lakes_filtered2 <- master_lakes_filtered %>% tidyr::drop_na(value)
  
  # create risk scores per sub-county
  
  risk_score_nolakes <- master_lakes_filtered2 %>% 
    group_by(subcounty_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value))
  
  mymode <- function(x) {
    t <- table(x)
    names(t)[ which.max(t) ]
  }
  
  risk_score_nolakes <- master_lakes_filtered2 %>% 
    group_by(subcounty_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value),
                     mode_risk = mymode(value))
  
  risk_score_nolakes$mean_risk <- risk_score_nolakes$risk/risk_score_nolakes$n
  
  risk_score_nolakes <- 
    risk_score_nolakes %>%
    mutate(mean_risk_score = cut(mean_risk,
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  risk_score_nolakes <- 
    risk_score_nolakes %>%
    mutate(mode_risk_score = cut(as.numeric(mode_risk),
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  # =============================================================================#
  # 2) calculate score (minus water bodies) for all sub-counties within dsitrict #
  
  master_lakes_district <- sf::st_join(data_to_join2, lakes_sf2) # join the two sf objects
  
  master_lakes_district_filtered <- master_lakes_district %>% filter(!name %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward")) # removes any observation including lakes
  
  # test average risk score
  #master_lakes_district_filtered$district_factor_test <- as.factor(master_lakes_district_filtered$DISTRICT)
  
  sf::st_geometry(master_lakes_district_filtered) <- NULL
  
  master_lakes_district_filtered2 <- master_lakes_district_filtered %>% tidyr::drop_na(value)
  
 if(year == 2011){
    master_lakes_district_filtered2 <- master_lakes_district_filtered2 %>% tidyr::drop_na(SNAME_2010)
  }
  
  if(year == 2013 || year == 2015 || year == 2019){
    master_lakes_district_filtered2 <- master_lakes_district_filtered2 %>% tidyr::drop_na(Subcounty)
  }
  
  # create risk scores per sub-county
  
  risk_score_district_nolakes <- master_lakes_district_filtered2 %>% 
    group_by(district_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value))
  
  
  risk_score_district_nolakes <- master_lakes_district_filtered2  %>% 
    group_by(district_factor_test) %>% 
    dplyr::summarise(n = n(), risk = sum(value),
                     mode_risk = mymode(value))
  
  risk_score_district_nolakes$mean_risk <- risk_score_district_nolakes$risk/risk_score_district_nolakes$n
  
  risk_score_district_nolakes <- 
    risk_score_district_nolakes %>%
    mutate(mean_risk_score = cut(mean_risk,
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  risk_score_district_nolakes <- 
    risk_score_district_nolakes %>%
    mutate(mode_risk_score = cut(as.numeric(mode_risk),
                                 breaks = c(0, 0.99, 1.0099, 1.099, 2.0099, 2.099, 2.1099, 3.1099, 3.12), right = FALSE,
                                 labels = c('all low','A' , 'B', 'C', 'AB', 'AC', 'BC', 'ABC')))
  
  
  # for plotting the lakes # 
  lakes_UGA <- lakes_sf %>% dplyr::select(name, name_en)
  
  lakes_UGA <- lakes_UGA %>% filter(name %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward"))
  
  value_rf <- c("grey90", "gold", "darkorchid1", "red1", "springgreen", "darkorange1", "pink1", "tan4")
  
  
  # subset Sub-county TS data depending on year
  Sub_county_TSdata$Prev_adjustment <- as.factor(Sub_county_TSdata$type)
  Sub_county_TSdata_subset <- subset(Sub_county_TSdata, study_year == year)
  
  type_chr <- as.character(Sub_county_TSdata_subset$type) # make character for legend (based on informed or apparent prev)
  range_toplot <- range(Sub_county_TSdata_subset$prevalence) # prev range to plot in legend for specific survey year
  
  
 if(year == 2011 || year == 2013 || year == 2015 || year == 2019){
   
   # make labels (sub-counties with MDA) for plotting (labels for subcounty risk score)
   risk_score_nolakes2 <- na.omit(risk_score_nolakes)
   
   scnames$risk_factor <- risk_score_nolakes2$mode_risk_score
   scnames <- within(scnames,  label1 <- paste(label, risk_factor, sep="; "))
   
    plot1 <- ggplot() +
      geom_tile(data = RF_data_plotting, 
                aes(x = x, y = y, fill = risk_fact_bins)) +
      geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
      geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue") + 
      geom_polygon(data= UGA_subcounties_tidy_subset, aes(x = long, y = lat, group = group, colour= PCC_survey), size = 1.1, fill=NA, alpha=NA)+
      scale_fill_manual(name = "Class",
                        values = value_rf, 
                        na.value = NA, na.translate = FALSE)+
      scale_colour_manual(values=c("black",NA), guide=FALSE)+
      coord_equal() +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.background = element_blank()) +
      cowplot::panel_border(remove = TRUE) +
      ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label1), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
      coord_sf(xlim = c(29.83, 35.1), ylim = c(-1.5, 4.32))
    
    plot1a <- plot1 +
      geom_point(data = Sub_county_TSdata_subset, aes(x= long, y = lat, size = prevalence), shape=21, 
                 fill = "yellow", colour = "black", alpha = 0.75)+
      scale_size_continuous(name=type_chr,
                            breaks = c(5, 10, 15, 20, 30, 40, 50, 60, 90),
                            limits = c(0,92))
      #scale_size(limits = c(0,92)) 
  }
  
  # make NEW labels (sub-counties across districts with MDA) for plotting
  risk_score_nolakes_dist2 <- na.omit(risk_score_district_nolakes)
  
  dnames2 <- aggregate(cbind(x, y) ~ DISTRICT, data=master_lakes_district_filtered2, FUN=mean)
  dnames2$label <- dnames2$DISTRICT
  dnames2$risk_factor <- risk_score_nolakes_dist2$mode_risk_score
  dnames2 <- within(dnames2,  label1 <- paste(DISTRICT, risk_factor, sep="; "))
  dnames2 <- dnames2 %>% dplyr::rename(long = x, lat = y)
  
  plot2 <- ggplot() +
    geom_tile(data = RF_data_plotting, 
              aes(x = x, y = y, fill = risk_fact_bins)) +
    geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
    geom_sf(data = lakes_UGA, colour = alpha("blue",0.8), fill = "blue") + 
    geom_polygon(data= UGA_subcounties_tidy_subset, aes(x = long, y = lat, group = group, colour= PCC_survey), size = 1.1, fill=NA, alpha=NA)+
    scale_fill_manual(name = "Class",
                      values = value_rf, 
                      na.value = NA, na.translate = FALSE)+
    scale_colour_manual(values=c("black",NA), guide=FALSE)+
    coord_equal() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
    ggrepel::geom_text_repel(data = dnames2, aes(long, lat, label = label1), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
    coord_sf(xlim = c(29.83, 35.1), ylim = c(-1.5, 4.32))
  
  plot2a <- plot2 +
    geom_point(data = Sub_county_TSdata_subset, aes(x= long, y = lat, size = prevalence), shape=21, 
               fill = "yellow", colour = "black", alpha = 0.75)+
    scale_size_continuous(name=type_chr,
                          breaks = c(5, 10, 15, 20, 30, 40, 50, 60, 90),
                          # labels=c("0-10%", "10.01-25%", "25.01-40%",
                          #            "40.01-60%", ">60%"),
                          limits = c(0,92))
  
  
  return(list(risk_score_nolakes, plot1a, risk_score_district_nolakes, plot2a, dnames2))
  
}