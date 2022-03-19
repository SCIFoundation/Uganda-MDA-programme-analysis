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
  
