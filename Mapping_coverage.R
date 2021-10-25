#===================================================================================================================#
#=====================================  Mapping MDA Coverage across Uganda =========================================#

#=============================================#
#==== produce country map function  ==========#
Plot_nationalmap_Uganda_func <- function() {

w2hr <- map_data("world2Hires")

UGA <- w2hr[w2hr$region=="Uganda",] # obtain Uganda country outline

ggplot() + 
  geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") + 
  coord_equal() # plot Uganda country outline

return(UGA)

}

#=========================================================================================#
#    Plotting district boundaries (2003-2009) - create Dataframe object (from shape file) # 

# Plot district layer 2003-2009 (2006 admin data) #
# source: https://earthworks.stanford.edu/catalog/stanford-vg894mz3698


UGA_district_boundaries_0309_function <- function(shape_file, national_map_input){ 
  
  glimpse(shape_file) # check shape file
  
  districts2006_tidy <- tidy(shape_file) # turn into a dataframe with tidy func
  
  ggplot() +
  geom_polygon(data = national_map_input, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
  geom_polygon(data = districts2006_tidy, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
  coord_equal(ratio = 1) # plot district boundaries
  
  # make dataframe object with variables (district name) for mapping #
  
  districts_2006$id <- row.names(districts_2006) # include row ids in spatial polygon object
  
  UGA_districts_tidy_0309 <- left_join(districts2006_tidy, districts_2006@data) # join variables from spatial polygon into dataframe
  
  return(UGA_districts_tidy_0309)
  
  }

#========================================================================================#
#============ function to get district names (2003-2009) ================================#

district_name_func <- function(shape_file = districts_2006){
  
  UGA_dist_MDA_names_2003_2009 <- data.frame(Dist_name = sort(shape_file@data$dname_2006)) 
  
  return(UGA_dist_MDA_names_2003_2009)

}


#==================================================================================================#
# function to get dataframe to plot different coverages (based on different methods) for each year #



district_MDA_coverage_mapping0309_dataframe_func <- function(data1, data2, data3, data4, 
                                                           district_names_0309, district_map_0309,
                                                           year_input){
  

# rename district names within coverage dataframes (to match shp dataframe object) #

as.character(unique(unlist(data1$District))) # view names of districts in cov dataframe
as.character(unique(unlist(data2$District))) # view names of districts in cov dataframe
as.character(unique(unlist(data3$District))) # view names of districts in cov dataframe
as.character(unique(unlist(data4$District))) # view names of districts in cov dataframe

data1$District_factor <- as.factor(data1$District) # create new factor variable
data2$District_factor <- as.factor(data2$District) # create new factor variable
data3$District_factor <- as.factor(data3$District) # create new factor variable
data4$District_factor <- as.factor(data4$District) # create new factor variable

District_factor_col <- as.data.frame(data1[ , 10]) # select this column

names(District_factor_col)[1] <- "District_factor" # rename col

District_factor_col <- District_factor_col %>%
  mutate(
    District_factor = recode(District_factor, 'Abim' = 'ABIM', 'Adjumani'= 'ADJUMANI', 'Agogo' = 'AGAGO',
                             'Amolatar' = 'AMOLATAR', 'Amudat' = 'AMUDAT', 'Amuria' = 'AMURIA', 'Amuru' = 'AMURU',
                             'Apaca' = 'APAC', 'Aruaa' = 'ARUA', 'Budaka' = 'BUDAKA', 'Bududa' = 'BUDUDA', 'Bugiri' = 'BUGIRI',
                             'Bukeda' = 'BUKEDEA', 'Bukwo' = 'BUKWO', 'Bulilsa' = 'BULIISA', 'Bundibugyoa' = 'BUNDIBUGYO',
                             'Bushenyi' = 'BUSHENYI', 'Busia' = 'BUSIA', 'Butaleja' = 'BUTALEJA', 'Dokolo' = 'DOKOLO',
                             'Gulu' = 'GULU', 'Hoima' = 'HOIMA', 'Ibanda' = 'IBANDA', 'Iganga' = 'IGANGA', 'Isingiro' = 'ISINGIRO',
                             'Jinja' = 'JINJA', 'Kaabong' = 'KAABONG', 'Kabale' = 'KABALE', 'Kabarole' = 'KABAROLE',
                             'Kaberamaido' = 'KABERAMAIDO', 'Kalangala' = 'KALANGALA', 'Kaliro' = 'KALIRO', 'Kampala' = 'KAMPALA',
                             'Kamuli' = 'KAMULI', 'Kamwenge' = 'KAMWENGE', 'Kanungu' = 'KANUNGU', 'Kapchorwa' = 'KAPCHORWA',
                             'Kasese' = 'KASESE', 'Katakwi' = 'KATAKWI', 'Kayunga' = 'KAYUNGA', 'Kibaale' = 'KIBAALE',
                             'Kiboga' = 'KIBOGA', 'Kiruhura' = 'KIRUHURA', 'Kisoro' = 'KISORO', 'Kitgum' = 'KITGUM',
                             'Koboko' = 'KOBOKO', 'Kotido' = 'KOTIDO', 'Kumi' = 'KUMI', 'Kyenjojo' = 'KYENJOJO', 'Lamwo' = 'LAMWO',
                             'Lira' = 'LIRA', 'Luwero' = 'LUWERO', 'Lyantonde' = 'LYANTONDE', 'Manafwa' = 'MANAFWA',
                             'Maracha-Terego (or Nyadri)' = 'MARACHA (NYADRI)', 'Masaka' = 'MASAKA', 'Masindi' = 'MASINDI',
                             'Mayuge' = 'MAYUGE', 'Mbale' = 'MBALE', 'Mbarara' = 'MBARARA', 'Mityana' = 'MITYANA', 'Moroto' = 'MOROTO',
                             'Moyo' = 'MOYO', 'Mpigi' = 'MPIGI', 'Mubende' = 'MUBENDE', 'Mukono' = 'MUKONO',
                             'Nakapiripirit' = 'NAKAPIRIPIRIT', 'Nakaseke' = 'NAKASEKE', 'Nakasongola' = 'NAKASONGOLA',
                             'Namutumba' = 'NAMUTUMBA', 'Napak' = 'NAPAK', 'Nebbi' = 'NEBBI', 'Ntungamo' = 'NTUNGAMO',
                             'Nwoya (from Amuru)' = 'NWOYA', 'Oyam' = 'OYAM', 'Pader' = 'PADER', 'Pallisa ' = 'PALLISA',
                             'Rakai' = 'RAKAI', 'Rukungiri' = 'RUKUNGIRI', 'Sironko' = 'SIRONKO', 'Soroti' = 'SOROTI',
                             'SSembabule'='SSEMBABULE','Tororo' = 'TORORO',
                             'Wakiso' = 'WAKISO','Yumbe' = 'YUMBE'))

# levels(District_factor_col$District_factor) # check

data1$District_factor <- District_factor_col$District_factor

data2$District_factor <- District_factor_col$District_factor

data3$District_factor <- District_factor_col$District_factor

data4$District_factor <- District_factor_col$District_factor


#==============================================================#
#   Mapping Presence of MDA: 2003-2009 treatment year          #


if(year_input == 2003 || year_input == 2004){

  MDA_districts <- c("APAC","MOYO","ADJUMANI","ARUA","NEBBI","LIRA","NAKASONGOLA","MASINDI","HOIMA",
            "BUGIRI", "BUSIA", "KAYUNGA", "JINJA", "MUKONO", "WAKISO", "MAYUGE", "BUNDIBUGYO","KIBAALE") # vector of districts with MDA in 2003
  } 
if(year_input == 2005 || year_input == 2006) {

    MDA_districts <- c("APAC","MOYO","ADJUMANI","YUMBE","ARUA","NEBBI","LIRA","KABERAMAIDO","SOROTI","NAKASONGOLA",
                  "MASINDI","HOIMA","KAMULI","BUGIRI", "BUSIA", "KAYUNGA", "JINJA", "MUKONO", "WAKISO", "MAYUGE",
                  "KALANGALA","KABALE","KISORO","KANUNGU","RUKUNGIRI","BUNDIBUGYO","KIBAALE")
    } 

length(MDA_districts)

UGA_dist_MDA_names <- district_names_0309 # copy variable (dist names)

UGA_dist_MDA_names$MDA <- ifelse(district_names_0309$Dist_name %in% MDA_districts, "MDA","none") # code whether MDA or not

UGA_dist_MDA_names <- UGA_dist_MDA_names  %>% rename(dname_2006 = Dist_name) # rename column

UGA_districts_tidy <- left_join(district_map_0309, UGA_dist_MDA_names) # join boundary data to MDA presence data

UGA_districts_tidy$MDA <- as.factor(UGA_districts_tidy$MDA) # make MDA presence a factor

MDA.col <- c("purple2","lightgrey") # to colour MDA districts
MDA.vec <- MDA.col[UGA_districts_tidy$MDA] # specify colour for each polygon

UGA_districts_tidy$MDA_fill <- MDA.vec # new column for fill in ggplot depending on MDA

alpha.MDA.col <- c(0.6, 0.01) # alpha for gpplot depending on MDA fill

alpha.MDA.vec <- alpha.MDA.col[UGA_districts_tidy$MDA] # vector depending on MDA

UGA_districts_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon

# to plot: # 

# Map_03 <-
#   ggplot() +
#   geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
#   geom_polygon(data= UGA_districts_tidy_03, aes(x = long, y = lat, group = group), colour="black", size = 0.1, fill=MDA.vec, alpha=alpha.MDA.vec)+
#   coord_equal()+
#   #geom_point(data = UGA_TS_studies, aes(x=long, y=lat, size=Informed.prev, fill=sample.size, shape=Production.setting), colour="black", stroke=1.2, inherit.aes = FALSE)+
#   #scale_fill_brewer("Sample size", palette = "YlOrRd",aesthetics = "fill")+
#   #scale_size_discrete("Informed prevalence (%)")+
#   #scale_shape_manual(values=c(24,25,22))+
#   scale_colour_manual(values=c("black","purple2"), guide=FALSE)+
#   labs(title="2003")+
#   theme_void()+
#   theme(
#     plot.title = element_text(color="black", size=16, face="bold.italic"))+
#   guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend

#=========================================================#
#           Coverage of MDA: 2003-2009 treatment year          #

District_name0309_vec <-  c('ABIM', 'ADJUMANI', 'AGAGO', 'AMOLATAR', 'AMUDAT', 'AMURIA', 'AMURU', 'APAC', 'ARUA',
                            'BUDAKA', 'BUDUDA', 'BUGIRI', 'BUKEDEA', 'BUKWO', 'BULIISA', 'BUNDIBUGYO', 'BUSHENYI', 'BUSIA',
                            'BUTALEJA', 'DOKOLO', 'GULU', 'HOIMA', 'IBANDA', 'IGANGA', 'ISINGIRO', 'JINJA', 'KAABONG',
                            'KABALE', 'KABAROLE', 'KABERAMAIDO', 'KALANGALA', 'KALIRO', 'KAMPALA', 'KAMULI', 'KAMWENGE',
                            'KANUNGU', 'KAPCHORWA', 'KASESE', 'KATAKWI', 'KAYUNGA', 'KIBAALE', 'KIBOGA', 'KIRUHURA',
                            'KISORO', 'KITGUM', 'KOBOKO', 'KOTIDO', 'KUMI', 'KYENJOJO', 'LAMWO', 'LIRA', 'LUWERO',
                            'LYANTONDE', 'MANAFWA', 'MARACHA (NYADRI)', 'MASAKA', 'MASINDI','MAYUGE', 'MBALE',
                            'MBARARA', 'MITYANA', 'MOROTO', 'MOYO', 'MPIGI', 'MUBENDE', 'MUKONO', 'NAKAPIRIPIRIT',
                            'NAKASEKE', 'NAKASONGOLA', 'NAMUTUMBA', 'NAPAK', 'NEBBI', 'NTUNGAMO', 'NWOYA', 'OYAM', 'PADER',
                            'PALLISA', 'RAKAI', 'RUKUNGIRI', 'SIRONKO', 'SOROTI', 'SSEMBABULE', 'TORORO', 'WAKISO', 'YUMBE')

#===== Coverage 1 (total doses/toal targeted)==============#

dummy_dataset1 <- data1

dummy_dataset1 <- filter(dummy_dataset1, District_factor %in% District_name0309_vec) # filter so any renamed districts incldued

UGA_dist_MDAcov1_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003

UGA_dist_MDAcov1_names$dname_2006_chr <- as.character(UGA_dist_MDAcov1_names$dname_2006)

UGA_dist_MDAcov1_names <- UGA_dist_MDAcov1_names[order(UGA_dist_MDAcov1_names$dname_2006_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 

if(year_input == 2003 ){
  UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2003 # add coverage values to dataframe for mapping
  UGA_dist_MDAcov1_names$MDA_year <- as.factor("2003")
  }
if(year_input == 2004 ){
    UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2004
    UGA_dist_MDAcov1_names$MDA_year <- as.factor("2004")
    }
if(year_input == 2005 ){
      UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2005
      UGA_dist_MDAcov1_names$MDA_year <- as.factor("2005")
      } 
if(year_input == 2006 ){
      UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2006
      UGA_dist_MDAcov1_names$MDA_year <- as.factor("2006")
      }


UGA_districts_MDAcov1_tidy <- left_join(district_map_0309, UGA_dist_MDAcov1_names) # join boundary data to MDA presence data

UGA_districts_MDAcov1_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov1_tidy$MDA_cov) # make cov value a numeric variable

UGA_districts_MDAcov1_tidy$MDA <- as.factor(UGA_districts_MDAcov1_tidy$MDA) # make MDA presence a factor

UGA_districts_MDAcov1_tidy$Coverage_approach <- as.factor("denominator: total targeted")

alpha.MDA.col <- c(0.9, 0.01) # alpha for gpplot depending on MDA fill

alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov1_tidy$MDA] # vector depending on MDA

UGA_districts_MDAcov1_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon

# to plot

# Map_03_MDAcov1 <-
#   ggplot() +
#   geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
#   geom_polygon(data= UGA_districts_MDAcov1_tidy_03, aes(x = long, y = lat, group = group, fill=MDA_cov), colour="black", size = 0.1, alpha=alpha.MDA.vec)+
#   coord_equal()+
#   labs(title="2003")+
#   theme_void()+
#   scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis")+
#   theme(
#     plot.title = element_text(color="black", size=16, face="bold.italic"))
#guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend


#===== Coverage 2 (total doses/district pop)==============#
dummy_dataset2 <- data2

dummy_dataset2 <- filter(dummy_dataset2, District_factor %in% District_name0309_vec) # filter so any renamed districts incldued

UGA_dist_MDAcov2_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003

UGA_dist_MDAcov2_names$dname_2006_chr <- as.character(UGA_dist_MDAcov2_names$dname_2006)

UGA_dist_MDAcov2_names <- UGA_dist_MDAcov2_names[order(UGA_dist_MDAcov2_names$dname_2006_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 


if(year_input == 2003 ){
  UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2003 # add coverage values to dataframe for mapping
  UGA_dist_MDAcov2_names$MDA_year <- as.factor("2003")
  }
if(year_input == 2004 ){
    UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2004
    UGA_dist_MDAcov2_names$MDA_year <- as.factor("2004")
    }
if(year_input == 2005 ){
      UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2005
      UGA_dist_MDAcov2_names$MDA_year <- as.factor("2005")
    }
if(year_input == 2006 ){
  UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2006
  UGA_dist_MDAcov2_names$MDA_year <- as.factor("2006")
}

# UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2003 # add coverage values to dataframe for mapping

UGA_districts_MDAcov2_tidy <- left_join(district_map_0309, UGA_dist_MDAcov2_names) # join boundary data to MDA presence data

UGA_districts_MDAcov2_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov2_tidy$MDA_cov) # make cov value a numeric variable

UGA_districts_MDAcov2_tidy$MDA <- as.factor(UGA_districts_MDAcov2_tidy$MDA) # make MDA presence a factor

UGA_districts_MDAcov2_tidy$Coverage_approach <- as.factor("denominator: district population (constant growth)")

alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov2_tidy$MDA] # vector depending on MDA

UGA_districts_MDAcov2_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon

# Map_03_MDAcov2 <-
#   ggplot() +
#   geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
#   geom_polygon(data= UGA_districts_MDAcov2_tidy_03, aes(x = long, y = lat, group = group, fill=MDA_cov), colour="black", size = 0.1, alpha=alpha.MDA.vec)+
#   coord_equal()+
#   labs(title="2003")+
#   theme_void()+
#   scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis")+
#   theme(
#     plot.title = element_text(color="black", size=16, face="bold.italic"))
#guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend

#===== Coverage 3 (total doses/district pop - using SCIF data & pop growth )==============#

dummy_dataset3 <- data3

dummy_dataset3 <- filter(dummy_dataset3, District_factor %in% District_name0309_vec) # filter so any renamed districts incldued

UGA_dist_MDAcov3_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003

UGA_dist_MDAcov3_names$dname_2006_chr <- as.character(UGA_dist_MDAcov3_names$dname_2006)

UGA_dist_MDAcov3_names <- UGA_dist_MDAcov3_names[order(UGA_dist_MDAcov3_names$dname_2006_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 


if(year_input == 2003 ){
  UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2003 # add coverage values to dataframe for mapping
  UGA_dist_MDAcov3_names$MDA_year <- as.factor("2003")
  }
if(year_input == 2004 ){
    UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2004
    UGA_dist_MDAcov3_names$MDA_year <- as.factor("2004")
    }
if(year_input == 2005 ){
      UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2005
      UGA_dist_MDAcov3_names$MDA_year <- as.factor("2005")
      }
if(year_input == 2006 ){
  UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2006
  UGA_dist_MDAcov3_names$MDA_year <- as.factor("2006")
}

#UGA_dist_MDAcov3_names_03$MDA_cov <- dummy_dataset_2003c$Cov_2003 # add coverage values to dataframe for mapping

UGA_districts_MDAcov3_tidy <- left_join(district_map_0309, UGA_dist_MDAcov3_names) # join boundary data to MDA presence data

UGA_districts_MDAcov3_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov3_tidy$MDA_cov) # make cov value a numeric variable

UGA_districts_MDAcov3_tidy$MDA <- as.factor(UGA_districts_MDAcov3_tidy$MDA) # make MDA presence a factor

UGA_districts_MDAcov3_tidy$Coverage_approach <- as.factor("denominator: district population (SCIF numbers
& constant growth)")

alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov3_tidy$MDA] # vector depending on MDA

UGA_districts_MDAcov3_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon

# Map_03_MDAcov3 <-
#   ggplot() +
#   geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
#   geom_polygon(data= UGA_districts_MDAcov3_tidy_03, aes(x = long, y = lat, group = group, fill=MDA_cov), colour="black", size = 0.1, alpha=alpha.MDA.vec)+
#   coord_equal()+
#   labs(title="2003")+
#   theme_void()+
#   scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis")+
#   theme(
#     plot.title = element_text(color="black", size=16, face="bold.italic"))
#guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend


#===== Coverage 4 (total doses/largest targeted pop across years)==============#
dummy_dataset4 <- data4

dummy_dataset4 <- filter(dummy_dataset4, District_factor %in% District_name0309_vec) # filter so any renamed districts incldued

UGA_dist_MDAcov4_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003

UGA_dist_MDAcov4_names$dname_2006_chr <- as.character(UGA_dist_MDAcov4_names$dname_2006)

UGA_dist_MDAcov4_names <- UGA_dist_MDAcov4_names[order(UGA_dist_MDAcov4_names$dname_2006_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 


if(year_input == 2003 ){
  UGA_dist_MDAcov4_names$MDA_cov <- dummy_dataset4$Cov_2003 # add coverage values to dataframe for mapping
  UGA_dist_MDAcov4_names$MDA_year <- as.factor("2003")
  }
if(year_input == 2004 ){
    UGA_dist_MDAcov4_names$MDA_cov <- dummy_dataset4$Cov_2004
    UGA_dist_MDAcov4_names$MDA_year <- as.factor("2004")
    }
if(year_input == 2005 ){
      UGA_dist_MDAcov4_names$MDA_cov <- dummy_dataset4$Cov_2005
      UGA_dist_MDAcov4_names$MDA_year <- as.factor("2005")
      }
if(year_input == 2006 ){
  UGA_dist_MDAcov4_names$MDA_cov <- dummy_dataset4$Cov_2006
  UGA_dist_MDAcov4_names$MDA_year <- as.factor("2006")
}

#UGA_dist_MDAcov4_names_03$MDA_cov <- dummy_dataset_2003d$Cov_2003 # add coverage values to dataframe for mapping

UGA_districts_MDAcov4_tidy <- left_join(district_map_0309, UGA_dist_MDAcov4_names) # join boundary data to MDA presence data

UGA_districts_MDAcov4_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov4_tidy$MDA_cov) # make cov value a numeric variable

UGA_districts_MDAcov4_tidy$MDA <- as.factor(UGA_districts_MDAcov4_tidy$MDA) # make MDA presence a factor

UGA_districts_MDAcov4_tidy$Coverage_approach <- as.factor("denominator: largest targeted pop (across years)")

alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov4_tidy$MDA] # vector depending on MDA

UGA_districts_MDAcov4_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon

# Map_03_MDAcov4 <-
#   ggplot() +
#   geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
#   geom_polygon(data= UGA_districts_MDAcov4_tidy_03, aes(x = long, y = lat, group = group, fill=MDA_cov), colour="black", size = 0.1, alpha=alpha.MDA.vec)+
#   coord_equal()+
#   labs(title="2003")+
#   theme_void()+
#   scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis")+
#   theme(
#     plot.title = element_text(color="black", size=16, face="bold.italic"))
#guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend


#======================================================#
# Combined & plot different coverages                  #


# 2003-2009 MDA # 
UGA_districts_MDAcov_tidy <- rbind(UGA_districts_MDAcov1_tidy, UGA_districts_MDAcov2_tidy,
                                      UGA_districts_MDAcov3_tidy, UGA_districts_MDAcov4_tidy)


return(UGA_districts_MDAcov_tidy)

}


plot_UGA_MDA_func <- function(national_map, MDA_data){
  
  MDA_year_label <- as.character(unique(MDA_data$MDA_year)) # get year for plot title
  
  Map_0309_MDAcov <-
    ggplot() +
    geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
    geom_polygon(data= MDA_data, aes(x = long, y = lat, group = group, fill=MDA_cov, alpha = alpha.MDA.vec), colour="black", size = 0.1)+
    coord_equal()+
    labs(title=MDA_year_label)+
    facet_wrap(~Coverage_approach)+
    theme_void()+
    scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis")+
    scale_alpha_continuous(guide = "none") +
    theme(
    plot.title = element_text(color="black", size=16, face="bold.italic"))

return(Map_0309_MDAcov)

}







