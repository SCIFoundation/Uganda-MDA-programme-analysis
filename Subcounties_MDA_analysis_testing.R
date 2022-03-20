source('libraries.R')

#subcounties_2010 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2010 sub-counties/Uganda_Subcounties2010.shp") # need this for 2006

subcounties_2010 <- rgdal::readOGR("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2010 sub-counties/Uganda_Subcounties2010.shp") # need this for 2006

Uganda_dist <- rgdal::readOGR("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2001 district/uganda_district_2001_1.shp")

# ============================#
# 1) produce base national map

national_map <- Plot_nationalmap_Uganda_func()
national_map


# ===========================# 
# 2) map sub-counties        #

UGA_subcounties_boundaries_function <- function(subcounty_shape_file, district_map, national_map_input){

  # need to trasnform UGA sub-county data to WGS84 lat/lon co-ordinates first 
  
# class(Uganda_dist)
# plot(Uganda_dist,
#      main = "Uganda sitricts 2001",
#      border = "gray40")
# 
# crs(Uganda_dist)
# 
# crs(subcounties_2010)

subcounties_2010_WGS84 <- spTransform(subcounties_2010,
                              crs(Uganda_dist))

subcounties_plot <- ggplot() +
  geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
  geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
  geom_polygon(data = subcounties_2010_WGS84, aes(x = long, y = lat, group = group), colour = "blue", alpha = 0.75, fill = NA)+
  coord_equal(ratio = 1) # plot district boundaries


#View(subcounties_2010_WGS84)

# turn re-projected sub-county data into dataframe to work with 

subcounties_tidy <- tidy(subcounties_2010_WGS84) # turn into a dataframe with tidy func

# make dataframe object with variables (district name) for mapping #
  
subcounties_2010_WGS84$id <- row.names(subcounties_2010_WGS84) # include row ids in spatial polygon object
  
UGA_subcounties_tidy <- left_join(subcounties_tidy, subcounties_2010_WGS84@data) # join variables from spatial polygon into dataframe

return(list(subcounties_plot, subcounties_2010_WGS84, UGA_subcounties_tidy))

}



UGA_subcounty_object <- UGA_subcounties_boundaries_function(subcounty_shape_file = subcounties_2010, district_map = districts_2001, national_map_input = national_map)

UGA_subcounty_object[[1]]

View(UGA_subcounty_object[[2]])

View(UGA_subcounty_object[[3]])
UGA_subcounties_tidy <- UGA_subcounty_object[[3]] # for merging with presence of MDA data below

#============================================#

subcounty_name_func <- function(shape_file){
  
  #UGA_dist_MDA_names <- data.frame(Dist_name = sort(shape_file@data$dname_2006))
  
  UGA_dist_subcounty_MDA_names_2006 <- data.frame(Dist_name = shape_file@data$DNAME_2006,
                                   Subcounty_name = shape_file@data$SNAME_2006) 
  
  UGA_dist_subcounty_MDA_names_2006 <- with(UGA_dist_subcounty_MDA_names_2006,  UGA_dist_subcounty_MDA_names_2006[order(Dist_name) , ])
  
  
  #UGA_subcounty_MDA_names_2006 <- data.frame(Subcounty_name = sort(shape_file@data$SNAME_2006)) 
  
  UGA_dist_subcounty_MDA_names_2010 <- data.frame(Dist_name = shape_file@data$DNAME_2010,
                                                  Subcounty_name = shape_file@data$SNAME_2010) 
  
  UGA_dist_subcounty_MDA_names_2010 <- with(UGA_dist_subcounty_MDA_names_2010,  UGA_dist_subcounty_MDA_names_2010[order(Dist_name) , ])
  
  
  
  
 #UGA_subcounty_MDA_names_2010 <- data.frame(Subcounty_name = sort(shape_file@data$SNAME_2010)) 
  
  return(list(UGA_dist_subcounty_MDA_names_2006, UGA_dist_subcounty_MDA_names_2010))
  
}



subcounties_name_20062010_object <- subcounty_name_func(shape_file = UGA_subcounty_object[[2]])

# for 2003 - 2009 sub-counties 
subcounties_2006_names <- subcounties_name_20062010_object[[1]]
View(subcounties_2006_names)

# rename one subcounty as found twice in both MASAKA and MAYUGE (only want to consider MALONGO in MAYUGE)
subcounties_2006_names$Subcounty_name <- ifelse(subcounties_2006_names$Dist_name == "MAYUGE" & subcounties_2006_names$Subcounty_name == "MALONGO", 
                                                "MALONGO1", subcounties_2006_names$Subcounty_name)

# for 2010 sub-counties onwards
subcounties_2010_names <- subcounties_name_20062010_object[[2]]
View(subcounties_2010_names)

#============================================================================================================================================#
#                   EXTRACT SUB-COUNTIES WIT MDA TO I) WORK OUT AVG RISK and 2) PLOT OVER PCC RISK FACTOR MAPS                               #

#====================================#
#        2004                        #

# Sub_district_MDA_locations_2001_2005_df <- read.csv("") 
View(Sub_district_MDA_locations_2001_2005_df)


# get subcounties names variable (not unique)
subcounties_name_func <- function(shape_file){
  
  #UGA_dist_MDA_names <- data.frame(Dist_name = sort(shape_file@data$dname_2006))
  
  UGA_SC_MDA_names <- data.frame(Subcounty_name = shape_file@data$SNAME_2006,
                                 District_name = shape_file@data$DNAME_2006) 
  
  UGA_SC_MDA_names <- with(UGA_SC_MDA_names,  UGA_SC_MDA_names[order(District_name) , ])
  
  
  return(UGA_SC_MDA_names)
  
}

SC_names_2006 <- subcounties_name_func(shape_file = subcounties_2010) 

SC_names_2006$Subcounty_name <- ifelse(SC_names_2006$District_name == "MAYUGE" & SC_names_2006$Subcounty_name == "MALONGO", 
                                                "MALONGO1", SC_names_2006$Subcounty_name)
# isolate subcounties & districts with MDA

MDA_subcounties <-
  c("DZAIPI", "AKOKORO", "RHINO CAMP", "BANDA", "LUNYO", "KYANGWALI", "BUSERUKA", "KABWOYA", "KIGOROBYA",
    "MASESE/WALUKUBA", "GALIRAAYA", "MPEEFU", "MUNTU", "BIISO", "BULIISA", "MALONGO1", "DUFILE", "NGOGWE",
    "LWAMPANGA", "PAKWACH", "DIVISION A", "KANARA") # vector of subcounties with MDA in 2003
# Notes on 2003 subcounties: BIISO & BULIISA sub-counties found in Buliisa, rather than in Masindi, and MUNTU found in Amolotar (not Lira)
# MALONGO1 also renamed from MALONGO in MAYUGE as there are 2 MALONGO sub-counties (one if MASAKA), so need to make unique

# make MDA yes or no variable for sub counties with MDA

UGA_SC_MDA_names <- SC_names_2006 # copy variable (dist names) : UGA_dist_MDA_names <- district_names 

UGA_SC_MDA_names$MDA <- ifelse(SC_names_2006$Subcounty_name %in% MDA_subcounties, "MDA","none") # code whether MDA or not

UGA_SC_MDA_names <- UGA_SC_MDA_names  %>% rename(SNAME_2006 = Subcounty_name, DNAME_2006 = District_name) # rename column

UGA_subcounties_tidy$SNAME_2006 <- ifelse(UGA_subcounties_tidy$DNAME_2006 == "MAYUGE" & UGA_subcounties_tidy$SNAME_2006 == "MALONGO", 
                                       "MALONGO1", UGA_subcounties_tidy$SNAME_2006) # must also change any duplicate sub-counties with MDA in this object

UGA_subcounties_tidy <- left_join(UGA_subcounties_tidy, UGA_SC_MDA_names) # join boundary data to MDA presence data

UGA_subcounties_tidy$MDA <- as.factor(UGA_subcounties_tidy$MDA) # make MDA presence a factor

MDA.SC.col <- c("purple2", NA) # to colour MDA districts
MDA.SC.vec <- MDA.SC.col[UGA_subcounties_tidy$MDA] # specify colour for each polygon

UGA_subcounties_tidy$MDA_colour <- MDA.SC.vec # new column for fill in ggplot depending on MDA

UGA_subcounties_tidy$label <- ifelse(UGA_subcounties_tidy$MDA == "MDA", 
                                     UGA_subcounties_tidy$SNAME_2006, NA)

#alpha.MDA.col <- c(0.6, 0.01) # alpha for gpplot depending on MDA fill

#alpha.MDA.vec <- alpha.MDA.col[UGA_districts_tidy$MDA] # vector depending on MDA

#UGA_subcounties_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon

# PLOT:
  plot1 <- ggplot() +
    geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
    #geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
    geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 0.1, fill=NA, alpha=NA)+
  coord_equal(ratio = 1)+
  #geom_point(data = UGA_TS_studies, aes(x=long, y=lat, size=Informed.prev, fill=sample.size, shape=Production.setting), colour="black", stroke=1.2, inherit.aes = FALSE)+
  #scale_fill_brewer("Sample size", palette = "YlOrRd",aesthetics = "fill")+
  #scale_size_discrete("Informed prevalence (%)")+
  #scale_shape_manual(values=c(24,25,22))+
  scale_colour_manual(values=c("purple2",NA), guide=FALSE)+
  #labs(title="2003")+
  theme_void()+
  theme(
    plot.title = element_text(color="black", size=16, face="bold.italic"))+
  guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend

  # make label & plot
  UGA_subcounties_tidy_subset <- subset(UGA_subcounties_tidy, MDA=="MDA")   #subset just for NYS
  scnames <- aggregate(cbind(long, lat) ~ SNAME_2006, data=UGA_subcounties_tidy_subset, FUN=mean)
  scnames$label <- scnames$SNAME_2006
  
  plot1 +
     ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")
   
  
  
  #==========#
  # DISTRICT #
  district_map_0319 <- UGA_district_boundaries_function(shape_file = districts_2001, national_map_input = national_map) 
  district_map_0319[[1]]
  #district_map_0319[[2]] # this is for below (i.e. distirct_map)
  
  # repeat for districts to highlight & check sub-counties (for each district) for 2004#
  MDA_districts <-
  c("APAC", "MOYO", "ADJUMANI", "ARUA", "NEBBI", "LIRA", "NAKASONGOLA", "MASINDI", "HOIMA", "BUGIRI",
    "BUSIA", "KAYUNGA", "JINJA", "MUKONO", "WAKISO", "MAYUGE", "BUNDIBUGYO", "KIBAALE") # vector of districts with MDA in 2003
  
  
  #length(MDA_districts)
  
  district_names_0319 <- district_name_func(shape_file = districts_2001) # using original districts throughout 2003-2019
  
  UGA_dist_MDA_names <- district_names_0319 # copy variable (dist names)
  
  UGA_dist_MDA_names$MDA <- ifelse(district_names_0319$Dist_name %in% MDA_districts, "MDA","none") # code whether MDA or not
  
  UGA_dist_MDA_names <- UGA_dist_MDA_names  %>% rename(DISTRICT = Dist_name) # rename column
  
  UGA_districts_tidy <- left_join(district_map_0319[[2]], UGA_dist_MDA_names) # join boundary data to MDA presence data
  
  UGA_districts_tidy$MDA <- as.factor(UGA_districts_tidy$MDA) # make MDA presence a factor
  
  MDA.dist.col <- c("blue",NA) # to colour MDA districts
  MDA.dist.vec <- MDA.dist.col[UGA_districts_tidy$MDA] # specify colour for each polygon
  
  UGA_districts_tidy$MDA_colour <- MDA.dist.vec # new column for fill in ggplot depending on MDA
  
  # alpha.MDA.col <- c(0.6, 0.01) # alpha for gpplot depending on MDA fill
  # 
  # alpha.MDA.vec <- alpha.MDA.col[UGA_districts_tidy$MDA] # vector depending on MDA
  # 
  # UGA_districts_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon
  
  # PLOT:
  ggplot() +
    geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
    geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
     geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 0.75, fill=NA, alpha=NA)+
     geom_polygon(data= UGA_districts_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1, fill=NA, alpha=0.5)+
    coord_equal(ratio = 1)+
    scale_colour_manual(values=c("blue","purple2",NA), guide=FALSE)+
    ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
   #labs(title="2003")+
    theme_void()+
    theme(
      plot.title = element_text(color="black", size=16, face="bold.italic"))+
    guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
  

  
  #====================================================================================#
  #               Average risk score per sub-county                                    #
  
  #PCC_average_riskscores_subcounty_2001_df <- calculate_risk_scores_districts_func(data = overlay_2001[[3]], year = "2001")
  
  #===================================#
  data <- overlay_2001[[3]]
  
  # extract lat and longitutdes
  data$long <- data$x
  data$lat <- data$y
  
  # get distirct co-ordinates to match 
  
  # UGA_districts_data <- rnaturalearth::ne_states(country = 'Uganda', 
  #                                                returnclass = 'sf') %>%
  #   select(name, name_en)
  # 
 
  # make lat & lon dataframe into spatial object (sp)
  xy <- data[,c(1,2)]
  
    spdf <- SpatialPointsDataFrame(coords = xy, data = data,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  spdf2 <- sf::st_as_sf(spdf) # convert to sp object
  
  sf::st_is_valid(spdf2)
  
  #mapview::mapview(spdf2) 
  #=================================#
  # do the same for sub-county data #
  subcounties_2010 <- readShapePoly("~/Uganda-MDA-programme-analysis/Data/Uganda shape files/2010 sub-counties/Uganda_Subcounties2010.shp") # need this for 2006
  
  data2 <- UGA_subcounties_tidy_subset
  
  # #data2 <- raster::mask(subcounties_2010)
  # 
  # data2$x <- data2$long
  # data2$y <- data2$lat
  # 
  # xy2 <- data2[,c(21,22)]
  # 
  # spdf_sc <- SpatialPointsDataFrame(coords = xy2, data = data2,
  #                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  # 
  # spdf_sc2 <- sf::st_as_sf(spdf_sc,
  #                          coords = c("long", "lat"),
  #                          crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #master2 <- sf::st_join(spdf2, UGA_districts_data) # join risk factor data and admin data

  
  spdf_sc2 <- sfheaders::sf_polygon(
    obj = data2
    , x = "long"
    , y = "lat"
    , polygon_id = "SNAME_2006"
  )
  
  sf::st_crs( spdf_sc2 ) <- 4326 # WGS84 (EPSG: 4326)
  
  #sf::st_is_valid(spdf_sc2)
  #mapview::mapview(spdf_sc2)
  #=================================#
  master2 <- sf::st_join(spdf2, spdf_sc2) # join risk factor data and admin data
  
  master2a <- sf::st_join(spdf2, spdf_sc2) # join risk factor data and admin data - this is for the lake data later
  
  #master2 <- tigris::geo_join(spdf, data2, "lat", "lat")
  # master2 <- dplyr::inner_join(data, data2, by="lat")
  
  # data_frame_test <- data.frame(master2$long, master2$lat, master2$value, master2$variable, master2$risk_fact_bins,
  #                               master2$group, master2$id, master2$DNAME_2006, master2$CNAME_2006,
  #                               master2$SNAME_2006)
  
  # test average risk score
  master2$subcounty_factor_test <- as.factor(master2$SNAME_2006)
  
  sf::st_geometry(master2) <- NULL
  
  master3 <- master2 %>% tidyr::drop_na(value)
  
  # create risk scores per district
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
  
 
  View(risk_score)
  
  value_rf <- c("grey90", "gold", "darkorchid1", "red1", "springgreen", "darkorange1", "pink1", "tan4")
  
  ggplot() +
    geom_tile(data = overlay_2001[[3]], 
              aes(x = x, y = y, fill = risk_fact_bins)) +
    #geom_polygon(data=admin_processed, aes(x=long, y=lat, group=group), color="black", alpha=0) +
    geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
    geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.75, fill=NA, alpha=NA)+
    #geom_polygon(data= UGA_districts_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1, fill=NA, alpha=0.5)+
    scale_fill_manual(name = "Class",
                      values = value_rf, 
                      na.value = NA, na.translate = FALSE)+
    scale_colour_manual(values=c("purple2",NA), guide=FALSE)+
    coord_equal() +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.background = element_blank()) +
    cowplot::panel_border(remove = TRUE) +
    #annotate("text", label = year, x = 29.75, y = 3.8, size = 7, colour = "black")+
    ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")
  
  # ==========================================================================#
  # need to locate water bodies and remove co-ordinates from analysis for UGA #
  
  URL <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_lakes.zip"
  
  fil <- basename(URL)
  if (!file.exists(fil)) download.file(URL, fil)
  fils <- unzip(fil)
  lakes1 <- readOGR(grep("shp$", fils, value=TRUE), "ne_50m_lakes",
                   stringsAsFactors=FALSE, verbose=FALSE)
  
  #lake_albert <- lakes$label == "Lake Albert"
  
  plot(lakes1)
  
  lakes2 <- sf::st_as_sf(lakes1)
  
  lakes3 <- lakes2 %>% dplyr::select(name, name_en)
  
  master_lakes <- sf::st_join(master2a, lakes3)
  
  ggplot(data = lakes2) +
    geom_sf(data = lakes2, fill = "gray80") + 
    coord_sf(xlim = c(29.0, 35.1), ylim = c(-1.5, 4.32)) # map coordinates in utm
  
  lakes3 <- lakes2 %>% dplyr::select(name, name_en)
  
  master_lakes <- sf::st_join(master2a, lakes3)
  
  
  master_lakes_filtered <- master_lakes %>% filter(!name %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward")) # removes any observation including lakes
  
  # test average risk score
  master_lakes_filtered$subcounty_factor_test <- as.factor(master_lakes_filtered$SNAME_2006)
  
  sf::st_geometry(master_lakes_filtered) <- NULL
  
  master_lakes_filtered2 <- master_lakes_filtered %>% tidyr::drop_na(value)
  
  # create risk scores per district
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
  
  
  View(risk_score_nolakes)
  
  lakes_UGA <- lakes2 %>% dplyr::select(name, name_en)
  
  lakes_UGA <- lakes_UGA %>% filter(name %in% c("Lake Victoria", "Lake Albert","Lake Kyoga","Lake Edward"))
  
  value_rf <- c("grey90", "gold", "darkorchid1", "red1", "springgreen", "darkorange1", "pink1", "tan4")
  
  
  ggplot() +
    geom_tile(data = overlay_2001[[3]], 
              aes(x = x, y = y, fill = risk_fact_bins)) +
    #geom_polygon(data=admin_processed, aes(x=long, y=lat, group=group), color="black", alpha=0) +
    geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "grey45", alpha = 1, fill = NA)+
    #geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1.75, fill=NA, alpha=NA)+
    #geom_polygon(data= UGA_districts_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 1, fill=NA, alpha=0.5)+
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
    #annotate("text", label = year, x = 29.75, y = 3.8, size = 7, colour = "black")+
    ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
   coord_sf(xlim = c(29.83, 35.1), ylim = c(-1.5, 4.32))
 
    
  # test.points <- data.frame(lon = c(1.658854,1.547387,1.137820,1.173587),
  #                           lat = c(31.127233,31.258073,30.782171,30.616079))
  # 
  # coordinates(test.points) <- ~lon+lat
  # proj4string(test.points) <- CRS(proj4string(lakes2))
  # 
  # over(test.points, lakes2)
  
  
  # ggplot() +
  #   geom_tile(data = lakes, 
  #             aes(x = x, y = y, fill = risk_fact_bins)) +
  #   #geom_polygon(data=admin_processed, aes(x=long, y=lat, group=group), color="black", alpha=0) +
  #   geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)
  # 
  # 
  # lakes <- readShapePoly("C:/Users/MattDixon/OneDrive - SCI Foundation/Documents/Uganda-MDA-programme-analysis/Data/Uganda shape files/Uganda lakes/ne_50m_lakes.shp")
  # 
  # plot(lakes)
  