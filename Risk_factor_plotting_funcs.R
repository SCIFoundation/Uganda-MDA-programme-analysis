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
  
  value_rf <- c("grey90", "gold", "royalblue1", "red1", "springgreen", "darkorange1", "pink1", "tan4")
  
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
    cowplot::panel_border(remove = TRUE) +
    annotate("text", label = year, x = 29.75, y = 3.8, size = 7, colour = "black")
  
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
data<- overlay_2001[[3]]
  
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

return(list(risk_score, risk_score2a))

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

return(list(risk_map1, risk_map2))

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


average_risk_subcounties_func <- function(RF_data, subcounties_2010, subcounty_MDA_data, scnames, UGA_subcounties_tidy_subset, 
                                          UGA_subcounties_tidy, districts_2001, year){

  RF_data_plotting <- RF_data # maintain original risk factor overlay for plotting later
  
  #==================================================================================#
  # combine spatial objects: risk factor values across UGA & sub-county spatial data #
  
  if(year == 2004){
    data <- overlay_2001[[3]]
  }
  
  if(year == 2012){
    data <- overlay_2011[[3]]
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



}

if(year == 2012){
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
}

return(list(risk_score, risk_score_dist, plot1, plot2, master2, master_lake, master_dist_tosave, dnames))

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
  
  if(year == 2012){
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