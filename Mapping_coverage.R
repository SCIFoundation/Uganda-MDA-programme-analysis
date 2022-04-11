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


UGA_district_boundaries_function <- function(shape_file, national_map_input){ 
  
  glimpse(shape_file) # check shape file
  
  districts_tidy <- tidy(shape_file) # turn into a dataframe with tidy func
  
  district_plot <- 
    ggplot() +
    geom_polygon(data = national_map_input, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
    geom_polygon(data = districts_tidy, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
    coord_equal(ratio = 1) # plot district boundaries
  
  # make dataframe object with variables (district name) for mapping #
  
  shape_file$id <- row.names(shape_file) # include row ids in spatial polygon object
  
  UGA_districts_tidy <- left_join(districts_tidy, shape_file@data) # join variables from spatial polygon into dataframe
  
  return(list(district_plot, UGA_districts_tidy))
  
  }

# TO DO: make function for 2010 onwards or roll into this function?

#========================================================================================#
#============ function to get district names (2003-2009) ================================#

district_name_func <- function(shape_file){
  
  #UGA_dist_MDA_names <- data.frame(Dist_name = sort(shape_file@data$dname_2006))
  
  UGA_dist_MDA_names <- data.frame(Dist_name = sort(shape_file@data$DISTRICT)) 
  
  return(UGA_dist_MDA_names)

}

# TO DO: make function for 2010 onwards or roll into this function?


#==================================================================================================#
# function to get dataframe to plot different coverages (based on different methods) for each year #

# =============================================== #
# 1) mapping original district coverages function #
originaldistrict_MDA_coverage_mapping0319_dataframe_func <- function(data1, data2, data3, 
                                                                     district_names, district_map, year_input, age_target){
  
  temp_data1 <- cbind(district_names_0319, data1)
  temp_data1$Age <- as.factor(temp_data1$Age)
 
  temp_data2 <- cbind(district_names_0319, data2)
  temp_data2$Age <- as.factor(temp_data2$Age)
  
  temp_data3 <- cbind(district_names_0319, data3)
  temp_data3$Age <- as.factor(temp_data3$Age)
  
  # subset data based on age target # 
  if(age_target == "ALL") {
  temp_data1 <- subset(temp_data1, Age == "ALL")
  temp_data2 <- subset(temp_data2, Age == "ALL")
  temp_data3 <- subset(temp_data3, Age == "ALL")
  }
  if(age_target == "SAC") {
  temp_data2 <- subset(temp_data1, Age == "SAC")
  temp_data2 <- subset(temp_data2, Age == "SAC")
  temp_data3 <- subset(temp_data3, Age == "SAC")
  }
  
  if(age_target == "non-SAC") {
    temp_data2 <- subset(temp_data1, Age == "non-SAC")
    temp_data2 <- subset(temp_data2, Age == "non-SAC")
    temp_data3 <- subset(temp_data3, Age == "non-SAC")
  }
  
  #==============================================================#
  #   Mapping Presence of MDA: 2003-2009 treatment year          #
  
  selecting_MDA_districts_func <- function(year_input, age_target) {
    
    # 2003 - 2004 MDA years # 
    if (year_input == 2003 || year_input == 2004 && age_target == "ALL") {
      MDA_districts <-
        c("APAC", "MOYO", "ADJUMANI", "ARUA", "NEBBI", "LIRA", "NAKASONGOLA", "MASINDI", "HOIMA", "BUGIRI",
          "BUSIA", "KAYUNGA", "JINJA", "MUKONO", "WAKISO", "MAYUGE", "BUNDIBUGYO", "KIBAALE") # vector of districts with MDA in 2003
    }
    if (year_input == 2003 || year_input == 2004 && age_target == "SAC") {
      MDA_districts <-
        c("APAC", "MOYO", "ADJUMANI", "ARUA", "NEBBI", "LIRA", "NAKASONGOLA", "MASINDI", "HOIMA", "BUGIRI",
          "BUSIA", "KAYUNGA", "JINJA", "MUKONO", "WAKISO", "MAYUGE", "BUNDIBUGYO", "KIBAALE") # vector of districts with MDA in 2003
    }
    if (year_input == 2003 && age_target == "non-SAC") {
      MDA_districts <-
        c("APAC", "ARUA", "NEBBI", "LIRA", "NAKASONGOLA", "MASINDI", "BUGIRI","BUSIA",
          "KAYUNGA", "JINJA", "MUKONO", "WAKISO", "MAYUGE", "KIBAALE") # vector of districts with MDA in 2003
    }
    if (year_input == 2004 && age_target == "non-SAC") {
      MDA_districts <-
        c("APAC", "ARUA", "NEBBI", "LIRA", "NAKASONGOLA", "MASINDI", "BUGIRI","BUSIA",
          "KAYUNGA", "JINJA", "MUKONO", "MAYUGE", "KIBAALE", "HOIMA", "MOYO") # vector of districts with MDA in 2003
    }
    
    # 2005 - 2006 MDA years # 
    if (year_input == 2005 || year_input == 2006 && age_target == "ALL") {
      MDA_districts <-
        c("APAC","MOYO","ADJUMANI","YUMBE","ARUA","NEBBI","LIRA","KABERAMAIDO","SOROTI","NAKASONGOLA",
          "MASINDI","HOIMA","KAMULI","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO","MAYUGE",
          "KALANGALA","KABALE","KISORO","KANUNGU","RUKUNGIRI","BUNDIBUGYO","KIBAALE")
    }
    
    if (year_input == 2005 && age_target == "SAC") {
      MDA_districts <-
        c("APAC", "MOYO", "ADJUMANI","YUMBE","ARUA","NEBBI","LIRA","KABERAMAIDO","SOROTI",
          "NAKASONGOLA","MASINDI","HOIMA","KAMULI","BUGIRI","KAYUNGA","JINJA","MUKONO",
          "WAKISO","MAYUGE","KALANGALA","BUNDIBUGYO","KIBAALE")
    }
    
    if (year_input == 2006 && age_target == "SAC") {
      MDA_districts <-
        c("APAC","MOYO","ADJUMANI","ARUA","NEBBI","LIRA","KABERAMAIDO","SOROTI","NAKASONGOLA",
          "MASINDI","HOIMA","KAMULI","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","MAYUGE",
          "KALANGALA","BUNDIBUGYO","KIBAALE")
    }
    
    if (year_input == 2005 && age_target == "non-SAC") {
      MDA_districts <-
        c("APAC","MOYO","ADJUMANI","YUMBE","ARUA","NEBBI","LIRA","KABERAMAIDO","SOROTI","NAKASONGOLA",
          "MASINDI","HOIMA","KAMULI","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","MAYUGE","KALANGALA",
          "BUNDIBUGYO","KIBAALE")
    }
    if (year_input == 2006 && age_target == "non-SAC") {
      MDA_districts <-
        c("MOYO","LIRA","KABERAMAIDO","SOROTI","HOIMA","KAMULI","BUGIRI","BUSIA",
          "KAYUNGA","JINJA","MUKONO","WAKISO","MAYUGE","KALANGALA","BUNDIBUGYO")
    }
    
    # 2007 MDA year #
    if (year_input == 2007 && age_target == "ALL") {
      MDA_districts <-
        c("APAC","MOYO","ADJUMANI","ARUA","NEBBI","LIRA","KABERAMAIDO","SOROTI","KAMULI",
          "NAKASONGOLA","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MAYUGE","KALANGALA","MPIGI","MASAKA","KABAROLE","BUNDIBUGYO","KIBAALE","MASINDI")
    }
    if (year_input == 2007 && age_target == "SAC") {
      MDA_districts <-
        c("ADJUMANI","NEBBI","LIRA","KABERAMAIDO","SOROTI","NAKASONGOLA","HOIMA","BUGIRI","BUSIA",
          "KAYUNGA","JINJA","MUKONO","MAYUGE","KABAROLE","BUNDIBUGYO","KIBAALE","MASINDI", "KAMULI")
    }
    if (year_input == 2007 && age_target == "non-SAC") {
      MDA_districts <-
        c("ADJUMANI","NEBBI","LIRA","KABERAMAIDO","SOROTI","KAMULI",
          "NAKASONGOLA","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO",
          "MAYUGE","KABAROLE","BUNDIBUGYO","KIBAALE","MASINDI")
    }
    
    # 2008 MDA year # 
    if (year_input == 2008 && age_target == "ALL" || age_target == "non-SAC") {
      MDA_districts <-
        c("APAC","YUMBE","ARUA","NEBBI","LIRA","NAKASONGOLA","HOIMA","KAYUNGA","JINJA",
          "MUKONO","WAKISO","KALANGALA","MPIGI","MASAKA","KIBAALE","MASINDI","BUSIA")
    }
        if (year_input == 2008 && age_target == "SAC") {
      MDA_districts <-
        c("YUMBE","ARUA","NEBBI","NAKASONGOLA","KAYUNGA","JINJA",
          "MUKONO","KALANGALA","MASAKA","MASINDI","BUSIA")
    }

    # 2009 MDA year #
    if (year_input == 2009 && age_target == "ALL" || age_target == "SAC" || age_target == "non-SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA","KABERAMAIDO",
          "NAKASONGOLA","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MAYUGE","MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","KIBAALE","RAKAI","SOROTI","PALLISA",
          "KAMULI","MASINDI")
    }
    
    # 2009 rnd 2 MDA year#
    if (year_input == "2009rnd2" && age_target == "ALL" || age_target == "SAC" || age_target == "non-SAC") {
      MDA_districts <-
        c("APAC","YUMBE","ARUA","GULU","LIRA","KAYUNGA","MUKONO","KAMULI","KIBAALE")
    }
    
    # 2010 MDA year #
    if (year_input == 2010 && age_target == "ALL" || age_target == "SAC" || age_target == "non-SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","ARUA","GULU","LIRA","KABERAMAIDO","NAKASONGOLA","HOIMA",
          "BUGIRI","MAYUGE","MUBENDE","BUNDIBUGYO","SOROTI","MASINDI","KALANGALA","MASAKA",
          "ADJUMANI")
    }
    
    # 2011 MDA year: currently no available data #
    
    # 2012 MDA year #
    if (year_input == 2012 && age_target == "ALL" || age_target == "SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA","KABERAMAIDO",
          "NAKASONGOLA","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MAYUGE","MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","KIBAALE","SOROTI","PALLISA",
          "KAMULI","MASINDI","ADJUMANI","MASAKA","MBARARA","RUKUNGIRI","BUSHENYI",
          "KAMWENGE","KASESE","KALANGALA")
    }
    if (year_input == 2012 && age_target == "non-SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA",
          "NAKASONGOLA","HOIMA","BUGIRI","BUSIA","KAYUNGA","MUKONO","WAKISO",
          "MAYUGE","KABAROLE","MUBENDE","BUNDIBUGYO","KIBAALE","SOROTI","PALLISA",
          "KAMULI","MASINDI","ADJUMANI","MASAKA","MBARARA","RUKUNGIRI","BUSHENYI",
          "KAMWENGE","KASESE","KALANGALA")
    }
    
    # 2013 MDA year #
    if (year_input == 2013 && age_target == "ALL" || age_target == "SAC") {
      MDA_districts <-
        c("KITGUM","ARUA","LIRA","NAKASONGOLA","MUBENDE","SOROTI","PALLISA",
          "KAMULI","MASAKA","RUKUNGIRI","RAKAI",
          "TORORO","KUMI","KATAKWI","IGANGA","KAPCHORWA","MBALE","MBARARA","SIRONKO")
    }
    if (year_input == 2013 && age_target == "non-SAC") {
      MDA_districts <-
        c("")
    }
    
    # 2014 MDA year #
    if (year_input == 2014 && age_target == "ALL" || age_target == "SAC" || age_target == "non-SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA",
          "KABERAMAIDO","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","KIBAALE","SOROTI",
          "KAMULI","MASINDI","ADJUMANI","MASAKA","RUKUNGIRI","BUSHENYI",
          "KAMWENGE","KASESE","KALANGALA","IGANGA","MBALE","SIRONKO")
    }
    
    # 2015 MDA year #
    if (year_input == 2015 && age_target == "ALL" || age_target == "SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA",
          "KABERAMAIDO","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","KIBAALE","SOROTI","MAYUGE",
          "KAMULI","MASINDI","ADJUMANI","MASAKA","RUKUNGIRI","BUSHENYI",
          "KAMWENGE","KASESE","KALANGALA","IGANGA","MBALE","SIRONKO",
          "KAPCHORWA","KATAKWI","KUMI","MBARARA","NAKAPIRIPIRIT","NAKASONGOLA","PALLISA",
          "RAKAI","TORORO")
    }
    if (year_input == 2015 && age_target == "non-SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA",
          "KABERAMAIDO","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","KIBAALE","SOROTI","MAYUGE",
          "KAMULI","MASINDI","ADJUMANI","MASAKA","BUSHENYI",
          "KAMWENGE","KASESE","KALANGALA")
    }
    
    # 2016 MDA year #
    if (year_input == 2016 && age_target == "ALL" || age_target == "SAC" || age_target == "non-SAC") {
      MDA_districts <-
        c("MUBENDE","BUNDIBUGYO","BUSIA","KABAROLE","KALANGALA","KAYUNGA",
          "MASINDI","MPIGI","MUKONO","WAKISO")
    }
    
    # 2017 MDA year #
    if (year_input == 2017 && age_target == "ALL" || age_target == "SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA",
          "KABERAMAIDO","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","SOROTI","MAYUGE",
          "KAMULI","MASINDI","ADJUMANI","MASAKA","RUKUNGIRI","BUSHENYI",
          "KAMWENGE","KASESE","KALANGALA","IGANGA","MBALE","SIRONKO",
          "KAPCHORWA","KATAKWI","KUMI","MBARARA","NAKAPIRIPIRIT","NAKASONGOLA","PALLISA",
          "RAKAI","TORORO")
    }
    if (year_input == 2017 && age_target == "non-SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA",
          "KABERAMAIDO","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","SOROTI","MAYUGE",
          "KAMULI","MASINDI","ADJUMANI","MASAKA","BUSHENYI",
          "KAMWENGE","KASESE","KALANGALA")
      }
    
    # 2018 MDA year #
    if (year_input == 2018 && age_target == "ALL" || age_target == "SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA",
          "KABERAMAIDO","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","SOROTI","MAYUGE",
          "KAMULI","MASINDI","ADJUMANI","MASAKA","BUSHENYI","KAMWENGE","KASESE",
          "KALANGALA")
    }
    if (year_input == 2018 && age_target == "non-ALL") {
      MDA_districts <-
        c("PADER","APAC","YUMBE","ARUA","NEBBI","LIRA",
          "KABERAMAIDO","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","SOROTI","MAYUGE",
          "KAMULI","MASINDI","ADJUMANI","MASAKA","BUSHENYI","KAMWENGE","KASESE",
          "KALANGALA")
    }  
    
    # 2019 MDA year #
    if (year_input == 2019 && age_target == "ALL" || age_target == "SAC") {
      MDA_districts <-
        c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA",
          "KABERAMAIDO","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
          "MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","SOROTI","MAYUGE",
          "KAMULI","MASINDI","ADJUMANI","MASAKA","RUKUNGIRI","BUSHENYI",
          "KAMWENGE","KASESE","KALANGALA","IGANGA","MBALE","SIRONKO","KABALE",
          "KAPCHORWA","KATAKWI","KUMI","MBARARA","NAKAPIRIPIRIT","NAKASONGOLA","PALLISA",
          "RAKAI","TORORO")
    }
    if (year_input == 2019 && age_target == "non-SAC") {
      MDA_districts <-
        c("")
    }
    
    return(MDA_districts)
  }
  
  MDA_districts <- selecting_MDA_districts_func(year_input = year_input, age_target = age_target) # call function
  
  length(MDA_districts)
  
  UGA_dist_MDA_names <- district_names # copy variable (dist names)
  
  UGA_dist_MDA_names$MDA <- ifelse(district_names$Dist_name %in% MDA_districts, "MDA","none") # code whether MDA or not
  
  UGA_dist_MDA_names <- UGA_dist_MDA_names  %>% rename(DISTRICT = Dist_name) # rename column
  
  UGA_districts_tidy <- left_join(district_map, UGA_dist_MDA_names) # join boundary data to MDA presence data
  
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
  
  District_name_vec <-  c("ADJUMANI","APAC","ARUA","BUGIRI","BUNDIBUGYO","BUSHENYI","BUSIA","GULU",
                              "HOIMA","IGANGA","JINJA","KABALE","KABAROLE","KABERAMAIDO","KALANGALA","KAMPALA",
                              "KAMULI","KAMWENGE","KANUNGU","KAPCHORWA","KASESE","KATAKWI","KAYUNGA","KIBAALE",
                              "KIBOGA","KISORO","KITGUM","KOTIDO","KUMI","KYENJOJO","LIRA","LUWEERO",
                              "MASAKA","MASINDI","MAYUGE","MBALE","MBARARA","MOROTO","MOYO","MPIGI",
                              "MUBENDE","MUKONO","NAKAPIRIPIRIT","NAKASONGOLA","NEBBI","NTUNGAMO","PADER","PALLISA",
                              "RAKAI","RUKUNGIRI","SEMBABULE","SIRONKO","SOROTI","TORORO","WAKISO","YUMBE")
  
  #===== Coverage 1 (total doses/toal targeted)==============#
  
  #dummy_dataset1 <- data1
  
  dummy_dataset1 <- temp_data1 
  
  #dummy_dataset1 <- filter(dummy_dataset1, District_factor %in% District_name_vec) # filter so any renamed districts incldued
  dummy_dataset1 <- filter(dummy_dataset1, Dist_name %in% District_name_vec) # filter so any renamed districts incldued
  
  UGA_dist_MDAcov1_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003
  
  #UGA_dist_MDAcov1_names$Dist_name_chr <- as.character(UGA_dist_MDAcov1_names$Dist_name)
  UGA_dist_MDAcov1_names$Dist_name_chr <- as.character(UGA_dist_MDAcov1_names$DISTRICT)
  
  UGA_dist_MDAcov1_names <- UGA_dist_MDAcov1_names[order(UGA_dist_MDAcov1_names$Dist_name_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 
  
  make_cov_byyear_func1 <-
    function (year_input,
              UGA_dist_MDAcov1_names,
              dummy_dataset1) {
      if (year_input == 2003) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2003 # add coverage values to dataframe for mapping
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2003")
      }
      if (year_input == 2004) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2004
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2004")
      }
      if (year_input == 2005) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2005
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2005")
      }
      if (year_input == 2006) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2006
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2006")
      }
      if (year_input == 2007) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2007
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2007")
      }
      if (year_input == 2008) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2008
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2008")
      }
      if (year_input == 2009) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2009
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2009")
      }
      if (year_input == "2009rnd2") {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2009rnd2
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2009_rnd2")
      }
      if (year_input == 2010) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2010
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2010")
      }
      if (year_input == 2012) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2012
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2012")
      }
      if (year_input == 2013) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2013
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2013")
      }
      if (year_input == 2014) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2014
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2014")
      }
      if (year_input == 2015) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2015
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2015")
      }
      if (year_input == 2016) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2016
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2016")
      }
      if (year_input == 2017) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2017
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2017")
      }
      if (year_input == 2018) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2018
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2018")
      }
      if (year_input == 2019) {
        UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2019
        UGA_dist_MDAcov1_names$MDA_year <- as.factor("2019")
      }
      return(UGA_dist_MDAcov1_names)
    }
  
  UGA_dist_MDAcov1_names <-
    make_cov_byyear_func1(
      year_input = year_input,
      UGA_dist_MDAcov1_names = UGA_dist_MDAcov1_names,
      dummy_dataset1 = dummy_dataset1
    ) # call func
  
  UGA_districts_MDAcov1_tidy <- left_join(district_map, UGA_dist_MDAcov1_names) # join boundary data to MDA presence data
  
  UGA_districts_MDAcov1_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov1_tidy$MDA_cov) # make cov value a numeric variable
  
  UGA_districts_MDAcov1_tidy$MDA <- as.factor(UGA_districts_MDAcov1_tidy$MDA) # make MDA presence a factor
  
  UGA_districts_MDAcov1_tidy$Coverage_approach <- as.factor("denominator: total targeted")
  
  alpha.MDA.col <- c(0.9, 0.01) # alpha for gpplot depending on MDA fill
  
  alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov1_tidy$MDA] # vector depending on MDA
  
  UGA_districts_MDAcov1_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon
  
  UGA_districts_MDAcov1_tidy$alpha.MDA.vec <-
    ifelse(
      is.na(UGA_districts_MDAcov1_tidy$MDA_cov) &
        as.character(UGA_districts_MDAcov1_tidy$MDA) == "MDA",
      0.01,
      UGA_districts_MDAcov1_tidy$alpha.MDA.vec
    ) # some districts coded as MDA (from original analysis) but now no MDA coverage calculated after further data review, so change these to alpha = 0.01
  
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
  dummy_dataset2 <- temp_data2 
  
  dummy_dataset2 <- filter(dummy_dataset2, Dist_name %in% District_name_vec) # filter so any renamed districts incldued
  
  UGA_dist_MDAcov2_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003
  
  UGA_dist_MDAcov2_names$Dist_name_chr <- as.character(UGA_dist_MDAcov2_names$DISTRICT)
  
  UGA_dist_MDAcov2_names <- UGA_dist_MDAcov2_names[order(UGA_dist_MDAcov2_names$Dist_name_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 
  
  make_cov_byyear_func2 <-
    function (year_input,
              UGA_dist_MDAcov2_names,
              dummy_dataset2) {
      if (year_input == 2003) {
        UGA_dist_MDAcov2_names$MDA_cov <-
          dummy_dataset2$Cov_2003 # add coverage values to dataframe for mapping
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2003")
      }
      if (year_input == 2004) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2004
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2004")
      }
      if (year_input == 2005) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2005
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2005")
      }
      if (year_input == 2006) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2006
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2006")
      }
      if (year_input == 2007) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2007
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2007")
      }
      if (year_input == 2008) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2008
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2008")
      }
      if (year_input == 2009) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2009
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2009")
      }
      if (year_input == "2009rnd2") {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2009rnd2
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2009_rnd2")
      }
      if (year_input == 2010) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2010
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2010")
      }
      if (year_input == 2012) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2012
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2012")
      }
      if (year_input == 2013) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2013
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2013")
      }
      if (year_input == 2014) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2014
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2014")
      }
      if (year_input == 2015) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2015
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2015")
      }
      if (year_input == 2016) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2016
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2016")
      }
      if (year_input == 2017) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2017
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2017")
      }
      if (year_input == 2018) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2018
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2018")
      }
      if (year_input == 2019) {
        UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2019
        UGA_dist_MDAcov2_names$MDA_year <- as.factor("2019")
      }
      return(UGA_dist_MDAcov2_names)
    }
  
  UGA_dist_MDAcov2_names <-
    make_cov_byyear_func2(
      year_input = year_input,
      UGA_dist_MDAcov2_names = UGA_dist_MDAcov2_names,
      dummy_dataset2 = dummy_dataset2
    )
  
  
  # UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2003 # add coverage values to dataframe for mapping
  
  UGA_districts_MDAcov2_tidy <- left_join(district_map, UGA_dist_MDAcov2_names) # join boundary data to MDA presence data
  
  UGA_districts_MDAcov2_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov2_tidy$MDA_cov) # make cov value a numeric variable
  
  UGA_districts_MDAcov2_tidy$MDA <- as.factor(UGA_districts_MDAcov2_tidy$MDA) # make MDA presence a factor
  
  UGA_districts_MDAcov2_tidy$Coverage_approach <- as.factor("denominator: district population")
  
  alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov2_tidy$MDA] # vector depending on MDA
  
  UGA_districts_MDAcov2_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon
  
  UGA_districts_MDAcov2_tidy$alpha.MDA.vec <-
    ifelse(
      is.na(UGA_districts_MDAcov2_tidy$MDA_cov) &
        as.character(UGA_districts_MDAcov2_tidy$MDA) == "MDA",
      0.01,
      UGA_districts_MDAcov2_tidy$alpha.MDA.vec
    ) # some districts coded as MDA (from original analysis) but now no MDA coverage calculated after further data review, so change these to alpha = 0.01
  
  
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
  
  dummy_dataset3 <- temp_data3 
  
  dummy_dataset3 <- filter(dummy_dataset3, Dist_name %in% District_name_vec) # filter so any renamed districts incldued
  
  UGA_dist_MDAcov3_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003
  
  UGA_dist_MDAcov3_names$Dist_name_chr <- as.character(UGA_dist_MDAcov3_names$DISTRICT)
  
  UGA_dist_MDAcov3_names <- UGA_dist_MDAcov3_names[order(UGA_dist_MDAcov3_names$Dist_name_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 
  
  make_cov_byyear_func3 <-
    function (year_input,
              UGA_dist_MDAcov3_names,
              dummy_dataset3) {
      if (year_input == 2003) {
        UGA_dist_MDAcov3_names$MDA_cov <-
          dummy_dataset3$Cov_2003 # add coverage values to dataframe for mapping
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2003")
      }
      if (year_input == 2004) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2004
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2004")
      }
      if (year_input == 2005) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2005
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2005")
      }
      if (year_input == 2006) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2006
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2006")
      }
      if (year_input == 2007) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2007
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2007")
      }
      if (year_input == 2008) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2008
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2008")
      }
      if (year_input == 2009) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2009
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2009")
      }
      if (year_input == "2009rnd2") {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2009rnd2
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2009_rnd2")
      }
      if (year_input == 2010) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2010
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2010")
      }
      if (year_input == 2012) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2012
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2012")
      }
      if (year_input == 2013) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2013
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2013")
      }
      if (year_input == 2014) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2014
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2014")
      }
      if (year_input == 2015) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2015
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2015")
      }
      if (year_input == 2016) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2016
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2016")
      }
      if (year_input == 2017) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2017
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2017")
      }
      if (year_input == 2018) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2018
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2018")
      }
      if (year_input == 2019) {
        UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2019
        UGA_dist_MDAcov3_names$MDA_year <- as.factor("2019")
      }
      return(UGA_dist_MDAcov3_names)
    }
  
  UGA_dist_MDAcov3_names <-
    make_cov_byyear_func3(
      year_input = year_input,
      UGA_dist_MDAcov3_names = UGA_dist_MDAcov3_names,
      dummy_dataset3 = dummy_dataset3
    )
  
  #UGA_dist_MDAcov3_names_03$MDA_cov <- dummy_dataset_2003c$Cov_2003 # add coverage values to dataframe for mapping
  
  UGA_districts_MDAcov3_tidy <- left_join(district_map, UGA_dist_MDAcov3_names) # join boundary data to MDA presence data
  
  UGA_districts_MDAcov3_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov3_tidy$MDA_cov) # make cov value a numeric variable
  
  UGA_districts_MDAcov3_tidy$MDA <- as.factor(UGA_districts_MDAcov3_tidy$MDA) # make MDA presence a factor
  
  UGA_districts_MDAcov3_tidy$Coverage_approach <- as.factor("denominator: largest targeted number (03-19)")
  
  alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov3_tidy$MDA] # vector depending on MDA
  
  UGA_districts_MDAcov3_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon
  
  UGA_districts_MDAcov3_tidy$alpha.MDA.vec <-
    ifelse(
      is.na(UGA_districts_MDAcov3_tidy$MDA_cov) &
        as.character(UGA_districts_MDAcov3_tidy$MDA) == "MDA",
      0.01,
      UGA_districts_MDAcov3_tidy$alpha.MDA.vec
    ) # some districts coded as MDA (from original analysis) but now no MDA coverage calculated after further data review, so change these to alpha = 0.01
  
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
  
  
  #======================================================#
  # Combined & plot different coverages                  #
  
  
  # 2003-2019 MDA # 
  UGA_districts_MDAcov_tidy <- rbind(UGA_districts_MDAcov1_tidy, UGA_districts_MDAcov2_tidy,
                                     UGA_districts_MDAcov3_tidy)
  
  
  return(UGA_districts_MDAcov_tidy)
  
}




# =========================================================== #
# 1) mapping 03-09 district coverages function - old function #

# district_MDA_coverage_mapping0309_dataframe_func <- function(data1, data2, data3, data4, 
#                                                            district_names_0309, district_map_0309,
#                                                            year_input){
#   
# 
# # rename district names within coverage dataframes (to match shp dataframe object) #
# 
# as.character(unique(unlist(data1$District))) # view names of districts in cov dataframe
# as.character(unique(unlist(data2$District))) # view names of districts in cov dataframe
# as.character(unique(unlist(data3$District))) # view names of districts in cov dataframe
# as.character(unique(unlist(data4$District))) # view names of districts in cov dataframe
# 
# data1$District_factor <- as.factor(data1$District) # create new factor variable
# data2$District_factor <- as.factor(data2$District) # create new factor variable
# data3$District_factor <- as.factor(data3$District) # create new factor variable
# data4$District_factor <- as.factor(data4$District) # create new factor variable
# 
# District_factor_col <- as.data.frame(data1[ , 10]) # select this column
# 
# names(District_factor_col)[1] <- "District_factor" # rename col
# 
# District_factor_col <- District_factor_col %>%
#   mutate(
#     District_factor = recode(District_factor, 'Abim' = 'ABIM', 'Adjumani'= 'ADJUMANI', 'Agogo' = 'AGAGO',
#                              'Amolatar' = 'AMOLATAR', 'Amudat' = 'AMUDAT', 'Amuria' = 'AMURIA', 'Amuru' = 'AMURU',
#                              'Apaca' = 'APAC', 'Aruaa' = 'ARUA', 'Budaka' = 'BUDAKA', 'Bududa' = 'BUDUDA', 'Bugiri' = 'BUGIRI',
#                              'Bukeda' = 'BUKEDEA', 'Bukwo' = 'BUKWO', 'Bulilsa' = 'BULIISA', 'Bundibugyoa' = 'BUNDIBUGYO',
#                              'Bushenyi' = 'BUSHENYI', 'Busia' = 'BUSIA', 'Butaleja' = 'BUTALEJA', 'Dokolo' = 'DOKOLO',
#                              'Gulu' = 'GULU', 'Hoima' = 'HOIMA', 'Ibanda' = 'IBANDA', 'Iganga' = 'IGANGA', 'Isingiro' = 'ISINGIRO',
#                              'Jinja' = 'JINJA', 'Kaabong' = 'KAABONG', 'Kabale' = 'KABALE', 'Kabarole' = 'KABAROLE',
#                              'Kaberamaido' = 'KABERAMAIDO', 'Kalangala' = 'KALANGALA', 'Kaliro' = 'KALIRO', 'Kampala' = 'KAMPALA',
#                              'Kamuli' = 'KAMULI', 'Kamwenge' = 'KAMWENGE', 'Kanungu' = 'KANUNGU', 'Kapchorwa' = 'KAPCHORWA',
#                              'Kasese' = 'KASESE', 'Katakwi' = 'KATAKWI', 'Kayunga' = 'KAYUNGA', 'Kibaale' = 'KIBAALE',
#                              'Kiboga' = 'KIBOGA', 'Kiruhura' = 'KIRUHURA', 'Kisoro' = 'KISORO', 'Kitgum' = 'KITGUM',
#                              'Koboko' = 'KOBOKO', 'Kotido' = 'KOTIDO', 'Kumi' = 'KUMI', 'Kyenjojo' = 'KYENJOJO', 'Lamwo' = 'LAMWO',
#                              'Lira' = 'LIRA', 'Luwero' = 'LUWERO', 'Lyantonde' = 'LYANTONDE', 'Manafwa' = 'MANAFWA',
#                              'Maracha-Terego (or Nyadri)' = 'MARACHA (NYADRI)', 'Masaka' = 'MASAKA', 'Masindi' = 'MASINDI',
#                              'Mayuge' = 'MAYUGE', 'Mbale' = 'MBALE', 'Mbarara' = 'MBARARA', 'Mityana' = 'MITYANA', 'Moroto' = 'MOROTO',
#                              'Moyo' = 'MOYO', 'Mpigi' = 'MPIGI', 'Mubende' = 'MUBENDE', 'Mukono' = 'MUKONO',
#                              'Nakapiripirit' = 'NAKAPIRIPIRIT', 'Nakaseke' = 'NAKASEKE', 'Nakasongola' = 'NAKASONGOLA',
#                              'Namutumba' = 'NAMUTUMBA', 'Napak' = 'NAPAK', 'Nebbi' = 'NEBBI', 'Ntungamo' = 'NTUNGAMO',
#                              'Nwoya (from Amuru)' = 'NWOYA', 'Oyam' = 'OYAM', 'Pader' = 'PADER', 'Pallisa ' = 'PALLISA',
#                              'Rakai' = 'RAKAI', 'Rukungiri' = 'RUKUNGIRI', 'Sironko' = 'SIRONKO', 'Soroti' = 'SOROTI',
#                              'SSembabule'='SSEMBABULE','Tororo' = 'TORORO',
#                              'Wakiso' = 'WAKISO','Yumbe' = 'YUMBE'))
# 
# # levels(District_factor_col$District_factor) # check
# 
# data1$District_factor <- District_factor_col$District_factor
# 
# data2$District_factor <- District_factor_col$District_factor
# 
# data3$District_factor <- District_factor_col$District_factor
# 
# data4$District_factor <- District_factor_col$District_factor
# 
# 
# #==============================================================#
# #   Mapping Presence of MDA: 2003-2009 treatment year          #
# 
# selecting_MDA_districts_func <- function(year_input) {
#   if (year_input == 2003 || year_input == 2004) {
#     MDA_districts <-
#       c(
#         "APAC",
#         "MOYO",
#         "ADJUMANI",
#         "ARUA",
#         "NEBBI",
#         "LIRA",
#         "NAKASONGOLA",
#         "MASINDI",
#         "HOIMA",
#         "BUGIRI",
#         "BUSIA",
#         "KAYUNGA",
#         "JINJA",
#         "MUKONO",
#         "WAKISO",
#         "MAYUGE",
#         "BUNDIBUGYO",
#         "KIBAALE"
#       ) # vector of districts with MDA in 2003
#   }
#   if (year_input == 2005 || year_input == 2006) {
#     MDA_districts <-
#       c(
#         "APAC",
#         "MOYO",
#         "ADJUMANI",
#         "YUMBE",
#         "ARUA",
#         "NEBBI",
#         "LIRA",
#         "KABERAMAIDO",
#         "SOROTI",
#         "NAKASONGOLA",
#         "MASINDI",
#         "HOIMA",
#         "KAMULI",
#         "BUGIRI",
#         "BUSIA",
#         "KAYUNGA",
#         "JINJA",
#         "MUKONO",
#         "WAKISO",
#         "MAYUGE",
#         "KALANGALA",
#         "KABALE",
#         "KISORO",
#         "KANUNGU",
#         "RUKUNGIRI",
#         "BUNDIBUGYO",
#         "KIBAALE"
#       )
#   }
#   if (year_input == 2005 || year_input == 2006) {
#     MDA_districts <-
#       c(
#         "APAC",
#         "MOYO",
#         "ADJUMANI",
#         "YUMBE",
#         "ARUA",
#         "NEBBI",
#         "LIRA",
#         "KABERAMAIDO",
#         "SOROTI",
#         "NAKASONGOLA",
#         "MASINDI",
#         "HOIMA",
#         "KAMULI",
#         "BUGIRI",
#         "BUSIA",
#         "KAYUNGA",
#         "JINJA",
#         "MUKONO",
#         "WAKISO",
#         "MAYUGE",
#         "KALANGALA",
#         "KABALE",
#         "KISORO",
#         "KANUNGU",
#         "RUKUNGIRI",
#         "BUNDIBUGYO",
#         "KIBAALE"
#       )
#   }
#   if (year_input == 2007) {
#     MDA_districts <-
#       c(
#         "APAC",
#         "MOYO",
#         "ADJUMANI",
#         "MARACHA (NYADRI)",
#         "ARUA",
#         "NEBBI",
#         "LIRA",
#         "KABERAMAIDO",
#         "SOROTI",
#         "AMOLATAR",
#         "NAKASONGOLA",
#         "DOKOLO",
#         "BULISA",
#         "HOIMA",
#         "BUGIRI",
#         "BUSIA",
#         "KAYUNGA",
#         "JINJA",
#         "MUKONO",
#         "WAKISO",
#         "MAYUGE",
#         "KALANGALA",
#         "MPIGI",
#         "MASAKA",
#         "KABAROLE",
#         "BUNDIBUGYO",
#         "KIBAALE"
#       )
#   }
#   if (year_input == 2008) {
#     MDA_districts <-
#       c(
#         "APAC",
#         "KOLE",
#         "YUMBE",
#         "KOBOKO",
#         "MARACHA",
#         "ARUA",
#         "NEBBI",
#         "LIRA",
#         "ALBETONG",
#         "OTUKE",
#         "OYAM",
#         "KABERAMAIDO",
#         "SOROTI",
#         "AMOLATAR",
#         "NAKASONGOLA",
#         "DOKOLO",
#         "BULIISA",
#         "HOIMA",
#         "KAMULI",
#         "BUGIRI",
#         "KALIRO",
#         "KAYUNGA",
#         "JINJA",
#         "MUKONO",
#         "WAKISO",
#         "MITYANA",
#         "MAYUGE",
#         "KALANGALA",
#         "MPIGI",
#         "GOMBA",
#         "MASAKA",
#         "RAKAI",
#         "SSEMBABULE",
#         "MUBENDE",
#         "KIBAALE"
#       )
#   }
#   if (year_input == 2009) {
#     MDA_districts <-
#       c(
#         "PADER",
#         "AGAGO",
#         "LAMWO",
#         "APAC",
#         "KOLE",
#         "KITGUM",
#         "MOYO",
#         "ADJUMANI",
#         "YUMBE",
#         "KOBOKO",
#         "MARACHA",
#         "ARUA",
#         "NEBBI",
#         "GULU",
#         "LIRA",
#         "ALEBTONG",
#         "OTUKE",
#         "OYAM",
#         "KABERAMAIDO",
#         "SERERE",
#         "NAKASONGOLA",
#         "DOKOLO",
#         "KIRYANDONGO",
#         "BULIISA",
#         "HOIMA",
#         "BUYENDE",
#         "BUGIRI",
#         "BUSIA",
#         "KAYUNGA",
#         "JINJA",
#         "MUKONO",
#         "WAKISO",
#         "MITYANA",
#         "MAYUGE",
#         "KALANGALA",
#         "MPIGI",
#         "GOMBA",
#         "MASAKA",
#         "KABALE",
#         "KABAROLE",
#         "MUBENDE",
#         "BUNDIBUGYO",
#         "NTOROKO",
#         "KIBAALE"
#       )
#   }
#   
#   return(MDA_districts)
# }
# 
# MDA_districts <- selecting_MDA_districts_func(year_input = year_input) # call function
# 
# length(MDA_districts)
# 
# UGA_dist_MDA_names <- district_names_0309 # copy variable (dist names)
# 
# UGA_dist_MDA_names$MDA <- ifelse(district_names_0309$Dist_name %in% MDA_districts, "MDA","none") # code whether MDA or not
# 
# UGA_dist_MDA_names <- UGA_dist_MDA_names  %>% rename(dname_2006 = Dist_name) # rename column
# 
# UGA_districts_tidy <- left_join(district_map_0309, UGA_dist_MDA_names) # join boundary data to MDA presence data
# 
# UGA_districts_tidy$MDA <- as.factor(UGA_districts_tidy$MDA) # make MDA presence a factor
# 
# MDA.col <- c("purple2","lightgrey") # to colour MDA districts
# MDA.vec <- MDA.col[UGA_districts_tidy$MDA] # specify colour for each polygon
# 
# UGA_districts_tidy$MDA_fill <- MDA.vec # new column for fill in ggplot depending on MDA
# 
# alpha.MDA.col <- c(0.6, 0.01) # alpha for gpplot depending on MDA fill
# 
# alpha.MDA.vec <- alpha.MDA.col[UGA_districts_tidy$MDA] # vector depending on MDA
# 
# UGA_districts_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon
# 
# # to plot: # 
# 
# # Map_03 <-
# #   ggplot() +
# #   geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
# #   geom_polygon(data= UGA_districts_tidy_03, aes(x = long, y = lat, group = group), colour="black", size = 0.1, fill=MDA.vec, alpha=alpha.MDA.vec)+
# #   coord_equal()+
# #   #geom_point(data = UGA_TS_studies, aes(x=long, y=lat, size=Informed.prev, fill=sample.size, shape=Production.setting), colour="black", stroke=1.2, inherit.aes = FALSE)+
# #   #scale_fill_brewer("Sample size", palette = "YlOrRd",aesthetics = "fill")+
# #   #scale_size_discrete("Informed prevalence (%)")+
# #   #scale_shape_manual(values=c(24,25,22))+
# #   scale_colour_manual(values=c("black","purple2"), guide=FALSE)+
# #   labs(title="2003")+
# #   theme_void()+
# #   theme(
# #     plot.title = element_text(color="black", size=16, face="bold.italic"))+
# #   guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
# 
# #=========================================================#
# #           Coverage of MDA: 2003-2009 treatment year          #
# 
# District_name0309_vec <-  c('ABIM', 'ADJUMANI', 'AGAGO', 'AMOLATAR', 'AMUDAT', 'AMURIA', 'AMURU', 'APAC', 'ARUA',
#                             'BUDAKA', 'BUDUDA', 'BUGIRI', 'BUKEDEA', 'BUKWO', 'BULIISA', 'BUNDIBUGYO', 'BUSHENYI', 'BUSIA',
#                             'BUTALEJA', 'DOKOLO', 'GULU', 'HOIMA', 'IBANDA', 'IGANGA', 'ISINGIRO', 'JINJA', 'KAABONG',
#                             'KABALE', 'KABAROLE', 'KABERAMAIDO', 'KALANGALA', 'KALIRO', 'KAMPALA', 'KAMULI', 'KAMWENGE',
#                             'KANUNGU', 'KAPCHORWA', 'KASESE', 'KATAKWI', 'KAYUNGA', 'KIBAALE', 'KIBOGA', 'KIRUHURA',
#                             'KISORO', 'KITGUM', 'KOBOKO', 'KOTIDO', 'KUMI', 'KYENJOJO', 'LAMWO', 'LIRA', 'LUWERO',
#                             'LYANTONDE', 'MANAFWA', 'MARACHA (NYADRI)', 'MASAKA', 'MASINDI','MAYUGE', 'MBALE',
#                             'MBARARA', 'MITYANA', 'MOROTO', 'MOYO', 'MPIGI', 'MUBENDE', 'MUKONO', 'NAKAPIRIPIRIT',
#                             'NAKASEKE', 'NAKASONGOLA', 'NAMUTUMBA', 'NAPAK', 'NEBBI', 'NTUNGAMO', 'NWOYA', 'OYAM', 'PADER',
#                             'PALLISA', 'RAKAI', 'RUKUNGIRI', 'SIRONKO', 'SOROTI', 'SSEMBABULE', 'TORORO', 'WAKISO', 'YUMBE')
# 
# #===== Coverage 1 (total doses/toal targeted)==============#
# 
# dummy_dataset1 <- data1
# 
# dummy_dataset1 <- filter(dummy_dataset1, District_factor %in% District_name0309_vec) # filter so any renamed districts incldued
# 
# UGA_dist_MDAcov1_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003
# 
# UGA_dist_MDAcov1_names$dname_2006_chr <- as.character(UGA_dist_MDAcov1_names$dname_2006)
# 
# UGA_dist_MDAcov1_names <- UGA_dist_MDAcov1_names[order(UGA_dist_MDAcov1_names$dname_2006_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 
# 
# make_cov_byyear_func1 <-
#   function (year_input,
#             UGA_dist_MDAcov1_names,
#             dummy_dataset1) {
#     if (year_input == 2003) {
#       UGA_dist_MDAcov1_names$MDA_cov <-
#         dummy_dataset1$Cov_2003 # add coverage values to dataframe for mapping
#       UGA_dist_MDAcov1_names$MDA_year <- as.factor("2003")
#     }
#     if (year_input == 2004) {
#       UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2004
#       UGA_dist_MDAcov1_names$MDA_year <- as.factor("2004")
#     }
#     if (year_input == 2005) {
#       UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2005
#       UGA_dist_MDAcov1_names$MDA_year <- as.factor("2005")
#     }
#     if (year_input == 2006) {
#       UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2006
#       UGA_dist_MDAcov1_names$MDA_year <- as.factor("2006")
#     }
#     if (year_input == 2007) {
#       UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2007
#       UGA_dist_MDAcov1_names$MDA_year <- as.factor("2007")
#     }
#     if (year_input == 2008) {
#       UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2008
#       UGA_dist_MDAcov1_names$MDA_year <- as.factor("2008")
#     }
#     if (year_input == 2009) {
#       UGA_dist_MDAcov1_names$MDA_cov <- dummy_dataset1$Cov_2009
#       UGA_dist_MDAcov1_names$MDA_year <- as.factor("2009")
#     }
#     return(UGA_dist_MDAcov1_names)
#   }
# 
# UGA_dist_MDAcov1_names <-
#   make_cov_byyear_func1(
#     year_input = year_input,
#     UGA_dist_MDAcov1_names = UGA_dist_MDAcov1_names,
#     dummy_dataset1 = dummy_dataset1
#   ) # call func
# 
# UGA_districts_MDAcov1_tidy <- left_join(district_map_0309, UGA_dist_MDAcov1_names) # join boundary data to MDA presence data
# 
# UGA_districts_MDAcov1_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov1_tidy$MDA_cov) # make cov value a numeric variable
# 
# UGA_districts_MDAcov1_tidy$MDA <- as.factor(UGA_districts_MDAcov1_tidy$MDA) # make MDA presence a factor
# 
# UGA_districts_MDAcov1_tidy$Coverage_approach <- as.factor("denominator: total targeted")
# 
# alpha.MDA.col <- c(0.9, 0.01) # alpha for gpplot depending on MDA fill
# 
# alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov1_tidy$MDA] # vector depending on MDA
# 
# UGA_districts_MDAcov1_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon
# 
# UGA_districts_MDAcov1_tidy$alpha.MDA.vec <-
#   ifelse(
#     is.na(UGA_districts_MDAcov1_tidy$MDA_cov) &
#       as.character(UGA_districts_MDAcov1_tidy$MDA) == "MDA",
#     0.01,
#     UGA_districts_MDAcov1_tidy$alpha.MDA.vec
#   ) # some districts coded as MDA (from original analysis) but now no MDA coverage calculated after further data review, so change these to alpha = 0.01
# 
# # to plot
# 
# # Map_03_MDAcov1 <-
# #   ggplot() +
# #   geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
# #   geom_polygon(data= UGA_districts_MDAcov1_tidy_03, aes(x = long, y = lat, group = group, fill=MDA_cov), colour="black", size = 0.1, alpha=alpha.MDA.vec)+
# #   coord_equal()+
# #   labs(title="2003")+
# #   theme_void()+
# #   scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis")+
# #   theme(
# #     plot.title = element_text(color="black", size=16, face="bold.italic"))
# #guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
# 
# 
# #===== Coverage 2 (total doses/district pop)==============#
# dummy_dataset2 <- data2
# 
# dummy_dataset2 <- filter(dummy_dataset2, District_factor %in% District_name0309_vec) # filter so any renamed districts incldued
# 
# UGA_dist_MDAcov2_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003
# 
# UGA_dist_MDAcov2_names$dname_2006_chr <- as.character(UGA_dist_MDAcov2_names$dname_2006)
# 
# UGA_dist_MDAcov2_names <- UGA_dist_MDAcov2_names[order(UGA_dist_MDAcov2_names$dname_2006_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 
# 
# make_cov_byyear_func2 <-
#   function (year_input,
#             UGA_dist_MDAcov2_names,
#             dummy_dataset2) {
#     if (year_input == 2003) {
#       UGA_dist_MDAcov2_names$MDA_cov <-
#         dummy_dataset2$Cov_2003 # add coverage values to dataframe for mapping
#       UGA_dist_MDAcov2_names$MDA_year <- as.factor("2003")
#     }
#     if (year_input == 2004) {
#       UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2004
#       UGA_dist_MDAcov2_names$MDA_year <- as.factor("2004")
#     }
#     if (year_input == 2005) {
#       UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2005
#       UGA_dist_MDAcov2_names$MDA_year <- as.factor("2005")
#     }
#     if (year_input == 2006) {
#       UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2006
#       UGA_dist_MDAcov2_names$MDA_year <- as.factor("2006")
#     }
#     if (year_input == 2007) {
#       UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2007
#       UGA_dist_MDAcov2_names$MDA_year <- as.factor("2007")
#     }
#     if (year_input == 2008) {
#       UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2008
#       UGA_dist_MDAcov2_names$MDA_year <- as.factor("2008")
#     }
#     if (year_input == 2009) {
#       UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2009
#       UGA_dist_MDAcov2_names$MDA_year <- as.factor("2009")
#     }
#     return(UGA_dist_MDAcov2_names)
#   }
# 
# UGA_dist_MDAcov2_names <-
#   make_cov_byyear_func2(
#     year_input = year_input,
#     UGA_dist_MDAcov2_names = UGA_dist_MDAcov2_names,
#     dummy_dataset2 = dummy_dataset2
#   )
# 
# 
# # UGA_dist_MDAcov2_names$MDA_cov <- dummy_dataset2$Cov_2003 # add coverage values to dataframe for mapping
# 
# UGA_districts_MDAcov2_tidy <- left_join(district_map_0309, UGA_dist_MDAcov2_names) # join boundary data to MDA presence data
# 
# UGA_districts_MDAcov2_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov2_tidy$MDA_cov) # make cov value a numeric variable
# 
# UGA_districts_MDAcov2_tidy$MDA <- as.factor(UGA_districts_MDAcov2_tidy$MDA) # make MDA presence a factor
# 
# UGA_districts_MDAcov2_tidy$Coverage_approach <- as.factor("denominator: district population (constant growth)")
# 
# alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov2_tidy$MDA] # vector depending on MDA
# 
# UGA_districts_MDAcov2_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon
# 
# UGA_districts_MDAcov2_tidy$alpha.MDA.vec <-
#   ifelse(
#     is.na(UGA_districts_MDAcov2_tidy$MDA_cov) &
#       as.character(UGA_districts_MDAcov2_tidy$MDA) == "MDA",
#     0.01,
#     UGA_districts_MDAcov2_tidy$alpha.MDA.vec
#   ) # some districts coded as MDA (from original analysis) but now no MDA coverage calculated after further data review, so change these to alpha = 0.01
# 
# 
# # Map_03_MDAcov2 <-
# #   ggplot() +
# #   geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
# #   geom_polygon(data= UGA_districts_MDAcov2_tidy_03, aes(x = long, y = lat, group = group, fill=MDA_cov), colour="black", size = 0.1, alpha=alpha.MDA.vec)+
# #   coord_equal()+
# #   labs(title="2003")+
# #   theme_void()+
# #   scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis")+
# #   theme(
# #     plot.title = element_text(color="black", size=16, face="bold.italic"))
# #guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
# 
# #===== Coverage 3 (total doses/district pop - using SCIF data & pop growth )==============#
# 
# dummy_dataset3 <- data3
# 
# dummy_dataset3 <- filter(dummy_dataset3, District_factor %in% District_name0309_vec) # filter so any renamed districts incldued
# 
# UGA_dist_MDAcov3_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003
# 
# UGA_dist_MDAcov3_names$dname_2006_chr <- as.character(UGA_dist_MDAcov3_names$dname_2006)
# 
# UGA_dist_MDAcov3_names <- UGA_dist_MDAcov3_names[order(UGA_dist_MDAcov3_names$dname_2006_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 
# 
# make_cov_byyear_func3 <-
#   function (year_input,
#             UGA_dist_MDAcov3_names,
#             dummy_dataset3) {
#     if (year_input == 2003) {
#       UGA_dist_MDAcov3_names$MDA_cov <-
#         dummy_dataset3$Cov_2003 # add coverage values to dataframe for mapping
#       UGA_dist_MDAcov3_names$MDA_year <- as.factor("2003")
#     }
#     if (year_input == 2004) {
#       UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2004
#       UGA_dist_MDAcov3_names$MDA_year <- as.factor("2004")
#     }
#     if (year_input == 2005) {
#       UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2005
#       UGA_dist_MDAcov3_names$MDA_year <- as.factor("2005")
#     }
#     if (year_input == 2006) {
#       UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2006
#       UGA_dist_MDAcov3_names$MDA_year <- as.factor("2006")
#     }
#     if (year_input == 2007) {
#       UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2007
#       UGA_dist_MDAcov3_names$MDA_year <- as.factor("2007")
#     }
#     if (year_input == 2008) {
#       UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2008
#       UGA_dist_MDAcov3_names$MDA_year <- as.factor("2008")
#     }
#     if (year_input == 2009) {
#       UGA_dist_MDAcov3_names$MDA_cov <- dummy_dataset3$Cov_2009
#       UGA_dist_MDAcov3_names$MDA_year <- as.factor("2009")
#     }
#     return(UGA_dist_MDAcov3_names)
#   }
# 
# UGA_dist_MDAcov3_names <-
#   make_cov_byyear_func3(
#     year_input = year_input,
#     UGA_dist_MDAcov3_names = UGA_dist_MDAcov3_names,
#     dummy_dataset3 = dummy_dataset3
#   )
# 
# #UGA_dist_MDAcov3_names_03$MDA_cov <- dummy_dataset_2003c$Cov_2003 # add coverage values to dataframe for mapping
# 
# UGA_districts_MDAcov3_tidy <- left_join(district_map_0309, UGA_dist_MDAcov3_names) # join boundary data to MDA presence data
# 
# UGA_districts_MDAcov3_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov3_tidy$MDA_cov) # make cov value a numeric variable
# 
# UGA_districts_MDAcov3_tidy$MDA <- as.factor(UGA_districts_MDAcov3_tidy$MDA) # make MDA presence a factor
# 
# UGA_districts_MDAcov3_tidy$Coverage_approach <- as.factor("denominator: district population (SCIF numbers
# & constant growth)")
# 
# alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov3_tidy$MDA] # vector depending on MDA
# 
# UGA_districts_MDAcov3_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon
# 
# UGA_districts_MDAcov3_tidy$alpha.MDA.vec <-
#   ifelse(
#     is.na(UGA_districts_MDAcov3_tidy$MDA_cov) &
#       as.character(UGA_districts_MDAcov3_tidy$MDA) == "MDA",
#     0.01,
#     UGA_districts_MDAcov3_tidy$alpha.MDA.vec
#   ) # some districts coded as MDA (from original analysis) but now no MDA coverage calculated after further data review, so change these to alpha = 0.01
# 
# # Map_03_MDAcov3 <-
# #   ggplot() +
# #   geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
# #   geom_polygon(data= UGA_districts_MDAcov3_tidy_03, aes(x = long, y = lat, group = group, fill=MDA_cov), colour="black", size = 0.1, alpha=alpha.MDA.vec)+
# #   coord_equal()+
# #   labs(title="2003")+
# #   theme_void()+
# #   scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis")+
# #   theme(
# #     plot.title = element_text(color="black", size=16, face="bold.italic"))
# #guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
# 
# 
# #===== Coverage 4 (total doses/largest targeted pop across years)==============#
# dummy_dataset4 <- data4
# 
# dummy_dataset4 <- filter(dummy_dataset4, District_factor %in% District_name0309_vec) # filter so any renamed districts incldued
# 
# UGA_dist_MDAcov4_names <- UGA_dist_MDA_names %>% distinct() # remove districts not available in 2003
# 
# UGA_dist_MDAcov4_names$dname_2006_chr <- as.character(UGA_dist_MDAcov4_names$dname_2006)
# 
# UGA_dist_MDAcov4_names <- UGA_dist_MDAcov4_names[order(UGA_dist_MDAcov4_names$dname_2006_chr),] # TO DO (this is a quick fix): to get this dataframe districts to align with dummy dataset district order 
# 
# make_cov_byyear_func4 <-
#   function (year_input,
#             UGA_dist_MDAcov4_names,
#             dummy_dataset4) {
#     if (year_input == 2003) {
#       UGA_dist_MDAcov4_names$MDA_cov <-
#         dummy_dataset4$Cov_2003 # add coverage values to dataframe for mapping
#       UGA_dist_MDAcov4_names$MDA_year <- as.factor("2003")
#     }
#     if (year_input == 2004) {
#       UGA_dist_MDAcov4_names$MDA_cov <- dummy_dataset4$Cov_2004
#       UGA_dist_MDAcov4_names$MDA_year <- as.factor("2004")
#     }
#     if (year_input == 2005) {
#       UGA_dist_MDAcov4_names$MDA_cov <- dummy_dataset4$Cov_2005
#       UGA_dist_MDAcov4_names$MDA_year <- as.factor("2005")
#     }
#     if (year_input == 2006) {
#       UGA_dist_MDAcov4_names$MDA_cov <- dummy_dataset4$Cov_2006
#       UGA_dist_MDAcov4_names$MDA_year <- as.factor("2006")
#     }
#     if (year_input == 2007) {
#       UGA_dist_MDAcov4_names$MDA_cov <- dummy_dataset4$Cov_2007
#       UGA_dist_MDAcov4_names$MDA_year <- as.factor("2007")
#     }
#     if (year_input == 2008) {
#       UGA_dist_MDAcov4_names$MDA_cov <- dummy_dataset4$Cov_2008
#       UGA_dist_MDAcov4_names$MDA_year <- as.factor("2008")
#     }
#     if (year_input == 2009) {
#       UGA_dist_MDAcov4_names$MDA_cov <- dummy_dataset4$Cov_2009
#       UGA_dist_MDAcov4_names$MDA_year <- as.factor("2009")
#     }
#     
#     return(UGA_dist_MDAcov4_names)
#   }
# 
# UGA_dist_MDAcov4_names <-
#   make_cov_byyear_func4(
#     year_input = year_input,
#     UGA_dist_MDAcov4_names = UGA_dist_MDAcov4_names,
#     dummy_dataset4 = dummy_dataset4
#   )
# 
# #UGA_dist_MDAcov4_names_03$MDA_cov <- dummy_dataset_2003d$Cov_2003 # add coverage values to dataframe for mapping
# 
# UGA_districts_MDAcov4_tidy <- left_join(district_map_0309, UGA_dist_MDAcov4_names) # join boundary data to MDA presence data
# 
# UGA_districts_MDAcov4_tidy$MDA_cov <- as.numeric(UGA_districts_MDAcov4_tidy$MDA_cov) # make cov value a numeric variable
# 
# UGA_districts_MDAcov4_tidy$MDA <- as.factor(UGA_districts_MDAcov4_tidy$MDA) # make MDA presence a factor
# 
# UGA_districts_MDAcov4_tidy$Coverage_approach <- as.factor("denominator: largest targeted pop (across years)")
# 
# alpha.MDA.vec <- alpha.MDA.col[UGA_districts_MDAcov4_tidy$MDA] # vector depending on MDA
# 
# UGA_districts_MDAcov4_tidy$alpha.MDA.vec <- alpha.MDA.vec # new column based for alpha in gpplot of each polygon
# 
# UGA_districts_MDAcov4_tidy$alpha.MDA.vec <-
#   ifelse(
#     is.na(UGA_districts_MDAcov4_tidy$MDA_cov) &
#       as.character(UGA_districts_MDAcov4_tidy$MDA) == "MDA",
#     0.01,
#     UGA_districts_MDAcov4_tidy$alpha.MDA.vec
#   ) # some districts coded as MDA (from original analysis) but now no MDA coverage calculated after further data review, so change these to alpha = 0.01
# 
# # Map_03_MDAcov4 <-
# #   ggplot() +
# #   geom_polygon(data = UGA, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
# #   geom_polygon(data= UGA_districts_MDAcov4_tidy_03, aes(x = long, y = lat, group = group, fill=MDA_cov), colour="black", size = 0.1, alpha=alpha.MDA.vec)+
# #   coord_equal()+
# #   labs(title="2003")+
# #   theme_void()+
# #   scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis")+
# #   theme(
# #     plot.title = element_text(color="black", size=16, face="bold.italic"))
# #guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
# 
# 
# #======================================================#
# # Combined & plot different coverages                  #
# 
# 
# # 2003-2009 MDA # 
# UGA_districts_MDAcov_tidy <- rbind(UGA_districts_MDAcov1_tidy, UGA_districts_MDAcov2_tidy,
#                                       UGA_districts_MDAcov3_tidy, UGA_districts_MDAcov4_tidy)
# 
# 
# return(UGA_districts_MDAcov_tidy)
# 
# }


# TO DO: include years for 2010 onwards or make new function?

# plot maps with all denominators for coverage (3 plots across)
plot_UGA_MDA_func <- function(national_map, MDA_data){
  
  MDA_year_label <- as.character(unique(MDA_data$MDA_year)) # get year for plot title
  
  Map_0319_MDAcov <-
    ggplot() +
    geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
    geom_polygon(data= MDA_data, aes(x = long, y = lat, group = group, fill=MDA_cov, alpha = alpha.MDA.vec), colour="black", size = 0.1)+
    coord_equal()+
    labs(title=MDA_year_label, caption ="Dark grey values > 100%")+
    facet_wrap(~Coverage_approach)+
    theme_void()+
    scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis", limits=c(0, 100))+
    scale_alpha_continuous(guide = "none") +
    theme(
    plot.title = element_text(color="black", size=16, face="bold.italic"),
    plot.caption = element_text(face = "italic", size = 9))

return(Map_0319_MDAcov)

}

# plot maps with just denominator 1 (total targeted)
plot_UGA_denominator1_MDA_func <- function(national_map, MDA_data){
  
  MDA_data <- subset(MDA_data, Coverage_approach == "denominator: total targeted")
  
  MDA_year_label <- as.character(unique(MDA_data$MDA_year)) # get year for plot title
  
  Map_0319_MDAcov <-
    ggplot() +
    geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
    geom_polygon(data= MDA_data, aes(x = long, y = lat, group = group, fill=MDA_cov, alpha = alpha.MDA.vec), colour="black", size = 0.1)+
    coord_equal()+
    labs(title=MDA_year_label, caption ="Dark grey values > 100%")+
    #facet_wrap(~Coverage_approach)+
    theme_void()+
    scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis", limits=c(0, 100))+
    scale_alpha_continuous(guide = "none") +
    theme(
      plot.title = element_text(color="black", size=16, face="bold.italic"),
      plot.caption = element_text(face = "italic", size = 9))
  
  return(Map_0319_MDAcov)
  
}

# plot maps with just denominator 2 (total district pop)
plot_UGA_denominator2_MDA_func <- function(national_map, MDA_data){
  
  MDA_data <- subset(MDA_data, Coverage_approach == "denominator: district population")
  
  MDA_year_label <- as.character(unique(MDA_data$MDA_year)) # get year for plot title
  
  Map_0319_MDAcov <-
    ggplot() +
    geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
    geom_polygon(data= MDA_data, aes(x = long, y = lat, group = group, fill=MDA_cov, alpha = alpha.MDA.vec), colour="black", size = 0.1)+
    coord_equal()+
    labs(title=MDA_year_label, caption ="Dark grey values > 100%")+
    #facet_wrap(~Coverage_approach)+
    theme_void()+
    scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis", limits=c(0, 100))+
    scale_alpha_continuous(guide = "none") +
    theme(
      plot.title = element_text(color="black", size=8, face="bold.italic"),
      plot.caption = element_text(face = "italic", size = 6))
  
  return(Map_0319_MDAcov)
  
}

# plot maps with just denominator 3 (largest total targeted across 03-19 for given district)
plot_UGA_denominator3_MDA_func <- function(national_map, MDA_data){
  
  MDA_data <- subset(MDA_data, Coverage_approach == "denominator: largest targeted number (03-19)")
  
  MDA_year_label <- as.character(unique(MDA_data$MDA_year)) # get year for plot title
  
  Map_0319_MDAcov <-
    ggplot() +
    geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
    geom_polygon(data= MDA_data, aes(x = long, y = lat, group = group, fill=MDA_cov, alpha = alpha.MDA.vec), colour="black", size = 0.1)+
    coord_equal()+
    labs(title=MDA_year_label, caption ="")+
    #facet_wrap(~Coverage_approach)+
    theme_void()+
    scale_fill_continuous(name = "MDA Coverage (%)", type = "viridis", limits=c(0, 100))+
    scale_alpha_continuous(guide = "none") +
    theme(
      plot.title = element_text(color="black", size=8, face="bold.italic")
      #plot.caption = element_text(face = "italic", size = 5))
    )
  
  return(Map_0319_MDAcov)
  
}



# TO DO: extend to map from 2010, create additional map and call as list, or make new function?

#===================================================================================================#
#                                Sub-counties                                                       #
 
# ===========================# 
# map sub-counties function  #

UGA_subcounties_boundaries_function <- function(subcounty_shape_file, district_map, national_map_input){
  
  # need to trasnform UGA sub-county data to WGS84 lat/lon co-ordinates first 
  
  subcounties_WGS84 <- spTransform(subcounty_shape_file,
                                        crs(Uganda_dist))
  
  subcounties_plot <- ggplot() +
    geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
    geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
    geom_polygon(data = subcounties_WGS84, aes(x = long, y = lat, group = group), colour = "blue", alpha = 0.75, fill = NA)+
    coord_equal(ratio = 1) # plot district boundaries
  
  #View(subcounties_2010_WGS84)
  
  # turn re-projected sub-county data into dataframe to work with 
  
  subcounties_tidy <- tidy(subcounties_WGS84) # turn into a dataframe with tidy func
  
  # make dataframe object with variables (district name) for mapping #
  
  subcounties_WGS84$id <- row.names(subcounties_WGS84) # include row ids in spatial polygon object
  
  UGA_subcounties_tidy <- left_join(subcounties_tidy, subcounties_WGS84@data) # join variables from spatial polygon into dataframe
  
  return(list(subcounties_plot, subcounties_WGS84, UGA_subcounties_tidy))
  
}

# ==========================================#
# sub-county names for MDA mapping function #

subcounty_name0412_func <- function(shape_file){
  
  UGA_dist_subcounty_MDA_names_2006 <- data.frame(Dist_name = shape_file@data$DNAME_2006,
                                                  Subcounty_name = shape_file@data$SNAME_2006) 
  
  UGA_dist_subcounty_MDA_names_2006 <- with(UGA_dist_subcounty_MDA_names_2006,  UGA_dist_subcounty_MDA_names_2006[order(Dist_name) , ])
  
  # rename sub-counties for 2004 where duplicates
  UGA_dist_subcounty_MDA_names_2006$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2006$Dist_name == "MAYUGE" & 
                                                               UGA_dist_subcounty_MDA_names_2006$Subcounty_name == "MALONGO", 
                                                             "MALONGO1", UGA_dist_subcounty_MDA_names_2006$Subcounty_name)
  
  
  UGA_dist_subcounty_MDA_names_2010 <- data.frame(Dist_name = shape_file@data$DNAME_2010,
                                                  Subcounty_name = shape_file@data$SNAME_2010) 
  
  UGA_dist_subcounty_MDA_names_2010 <- with(UGA_dist_subcounty_MDA_names_2010,  UGA_dist_subcounty_MDA_names_2010[order(Dist_name) , ])
  
  # rename sub-counties for 2012 where duplicates
    UGA_dist_subcounty_MDA_names_2010$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2010$Dist_name == "MAYUGE" & 
                                                               UGA_dist_subcounty_MDA_names_2010$Subcounty_name == "MALONGO", 
                                                  "MALONGO1", UGA_dist_subcounty_MDA_names_2010$Subcounty_name)
    
    UGA_dist_subcounty_MDA_names_2010$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2010$Dist_name == "MASAKA" & 
                                              UGA_dist_subcounty_MDA_names_2010$Subcounty_name == "BUWUNGA", 
                                            "BUWUNGA1", UGA_dist_subcounty_MDA_names_2010$Subcounty_name) # must also change any duplicate sub-counties with MDA in this object
 
    UGA_dist_subcounty_MDA_names_2010$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2010$Dist_name == "JINJA" & 
                                                    UGA_dist_subcounty_MDA_names_2010$Subcounty_name == "CENTRAL DIVISION", 
                                                  "CENTRAL DIVISION1", UGA_dist_subcounty_MDA_names_2010$Subcounty_name) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_dist_subcounty_MDA_names_2010$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2010$Dist_name == "KAYUNGA" & 
                                                    UGA_dist_subcounty_MDA_names_2010$Subcounty_name == "KAYONZA", 
                                                  "KAYONZA1", UGA_dist_subcounty_MDA_names_2010$Subcounty_name) # must also change any duplicate sub-counties with MDA in this object


  return(list(UGA_dist_subcounty_MDA_names_2006, UGA_dist_subcounty_MDA_names_2010))
  
}


subcounty_name19_func <- function(shape_file){
  
    UGA_dist_subcounty_MDA_names_2019 <- data.frame(Dist_name = shape_file@data$District,
                                                    Subcounty_name = shape_file@data$Subcounty) 
    
    UGA_dist_subcounty_MDA_names_2019 <- with(UGA_dist_subcounty_MDA_names_2019,  UGA_dist_subcounty_MDA_names_2019[order(Dist_name) , ])
    
   
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "BUGWERI" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "BUYANGA", 
                                                               "BUYANGA1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "KYOTERA" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "KABIRA", 
                                                               "KABIRA1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "KAYUNGA" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "KAYONZA", 
                                                               "KAYONZA1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "BUSIA" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "MASABA", 
                                                               "MASABA1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "KABAROLE" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "RUTEETE", 
                                                               "RUTEETE1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "NAMAYINGO" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "BANDA", 
                                                               "BANDA1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "BUVUMA" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "BUGAYA", 
                                                               "BUGAYA1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "MITYANA" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "CENTRAL DIVISION", 
                                                               "CENTRAL DIVISION1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 

    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "JINJA" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "CENTRAL DIVISION", 
                                                               "CENTRAL DIVISION2", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "JINJA" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "KAKIRA TOWN COUNCIL", 
                                                               "KAKIRA TOWN COUNCIL1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "NTOROKO" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "KANARA", 
                                                               "KANARA1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "KAYUNGA" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "KAYONZA", 
                                                               "KAYONZA1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name) 
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "MAYUGE" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "MALONGO", 
                                                               "MALONGO1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name)
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "BUSIA" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "MASABA", 
                                                               "MASABA1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name)
   
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "KOBOKO" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "NORTHERN DIVISON", 
                                                               "NORTHERN DIVISON1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name)
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "KOBOKO" & 
                                                                   UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "SOUTHERN DIVISON", 
                                                                 "SOUTHERN DIVISON1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name)
    
    UGA_dist_subcounty_MDA_names_2019$Subcounty_name <- ifelse(UGA_dist_subcounty_MDA_names_2019$Dist_name == "KOBOKO" & 
                                                                 UGA_dist_subcounty_MDA_names_2019$Subcounty_name == "WESTERN DIVISON", 
                                                               "WESTERN DIVISON1", UGA_dist_subcounty_MDA_names_2019$Subcounty_name)
    return(list(UGA_dist_subcounty_MDA_names_2019))
  
}

# ========================================================================================================== #
# get subcounties names variable (not unique) function - prepare for next stage (select by presence of MDA)  #
subcounties_name_func2 <- function(shape_file, year){
  
  if(year == 2004){
  
  UGA_SC_MDA_names <- data.frame(Subcounty_name = shape_file@data$SNAME_2006,
                                 District_name = shape_file@data$DNAME_2006) 
  
  UGA_SC_MDA_names <- with(UGA_SC_MDA_names,  UGA_SC_MDA_names[order(District_name) , ])
  
  UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "MAYUGE" & UGA_SC_MDA_names$Subcounty_name == "MALONGO", 
                                         "MALONGO1", UGA_SC_MDA_names$Subcounty_name)
}
  
  if(year == 2012){
    
    UGA_SC_MDA_names <- data.frame(Subcounty_name = shape_file@data$SNAME_2010,
                                   District_name = shape_file@data$DNAME_2010) 
    
    UGA_SC_MDA_names <- with(UGA_SC_MDA_names,  UGA_SC_MDA_names[order(District_name) , ])
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "MASAKA" & UGA_SC_MDA_names$Subcounty_name == "BUWUNGA", 
                                            "BUWUNGA1", UGA_SC_MDA_names$Subcounty_name)

    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "JINJA" & UGA_SC_MDA_names$Subcounty_name == "CENTRAL DIVISION", 
                                            "CENTRAL DIVISION1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "KAYUNGA" & UGA_SC_MDA_names$Subcounty_name == "KAYONZA", 
                                              "KAYONZA1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "MAYUGE" & UGA_SC_MDA_names$Subcounty_name == "MALONGO", 
                                              "MALONGO1", UGA_SC_MDA_names$Subcounty_name)
  }
  
  if(year == 2015){
    
    UGA_SC_MDA_names <- data.frame(Subcounty_name = shape_file@data$Subcounty,
                                   District_name = shape_file@data$District)
    
    UGA_SC_MDA_names <- with(UGA_SC_MDA_names,  UGA_SC_MDA_names[order(District_name) , ])
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "BUGWERI" & UGA_SC_MDA_names$Subcounty_name == "BUYANGA", 
                                              "BUYANGA1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "KYOTERA" & UGA_SC_MDA_names$Subcounty_name == "KABIRA", 
                                              "KABIRA1", UGA_SC_MDA_names$Subcounty_name)
    
  }
  
  if(year == 2016){
    
    UGA_SC_MDA_names <- data.frame(Subcounty_name = shape_file@data$Subcounty,
                                   District_name = shape_file@data$District)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "KAYUNGA" & UGA_SC_MDA_names$Subcounty_name == "KAYONZA", 
                                              "KAYONZA1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "BUSIA" & UGA_SC_MDA_names$Subcounty_name == "MASABA", 
                                              "MASABA1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "KABAROLE" & UGA_SC_MDA_names$Subcounty_name == "RUTEETE", 
                                              "RUTEETE1", UGA_SC_MDA_names$Subcounty_name)

  }
  
  
  if(year == 2018){
    
    UGA_SC_MDA_names <- data.frame(Subcounty_name = shape_file@data$Subcounty,
                                   District_name = shape_file@data$District)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "NAMAYINGO" & UGA_SC_MDA_names$Subcounty_name == "BANDA", 
                                              "BANDA1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "BUVUMA" & UGA_SC_MDA_names$Subcounty_name == "BUGAYA", 
                                              "BUGAYA1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "MITYANA" & UGA_SC_MDA_names$Subcounty_name == "CENTRAL DIVISION", 
                                              "CENTRAL DIVISION1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "JINJA" & UGA_SC_MDA_names$Subcounty_name == "CENTRAL DIVISION", 
                                              "CENTRAL DIVISION2", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "JINJA" & UGA_SC_MDA_names$Subcounty_name == "KAKIRA TOWN COUNCIL", 
                                              "KAKIRA TOWN COUNCIL1", UGA_SC_MDA_names$Subcounty_name)

    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "NTOROKO" & UGA_SC_MDA_names$Subcounty_name == "KANARA", 
                                              "KANARA1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "KAYUNGA" & UGA_SC_MDA_names$Subcounty_name == "KAYONZA", 
                                              "KAYONZA1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "MAYUGE" & UGA_SC_MDA_names$Subcounty_name == "MALONGO", 
                                              "MALONGO1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "BUSIA" & UGA_SC_MDA_names$Subcounty_name == "MASABA", 
                                              "MASABA1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "KOBOKO" & UGA_SC_MDA_names$Subcounty_name == "NORTHERN DIVISON", 
                                              "NORTHERN DIVISON1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "KABAROLE" & UGA_SC_MDA_names$Subcounty_name == "RUTEETE", 
                                              "RUTEETE1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "KOBOKO" & UGA_SC_MDA_names$Subcounty_name == "SOUTHERN DIVISON", 
                                              "SOUTHERN DIVISON1", UGA_SC_MDA_names$Subcounty_name)
    
    UGA_SC_MDA_names$Subcounty_name <- ifelse(UGA_SC_MDA_names$District_name == "KOBOKO" & UGA_SC_MDA_names$Subcounty_name == "WESTERN DIVISON", 
                                              "WESTERN DIVISON1", UGA_SC_MDA_names$Subcounty_name)
  }
  
  return(UGA_SC_MDA_names)
  
}


# ==================================================================== #
#   Plotting sub-county MDA (with district-level MDA overlay) function #


subcounty_MDA_processing_plotting_func <- function(sc_names, UGA_subcounties_tidy, district_2001, national_map_input, year){

  # =========================#
  #  Extract sub-county MDAs #
  
  if (year == 2004){
    
    MDA_subcounties <-
      c("DZAIPI", "AKOKORO", "RHINO CAMP", "BANDA", "LUNYO", "KYANGWALI", "BUSERUKA", "KABWOYA", "KIGOROBYA",
        "MASESE/WALUKUBA", "GALIRAAYA", "MPEEFU", "MUNTU", "BIISO", "BULIISA", "MALONGO1", "DUFILE", "NGOGWE",
        "LWAMPANGA", "PAKWACH", "DIVISION A", "KANARA") # vector of subcounties with MDA in 2003
    # Notes on 2003 subcounties: BIISO & BULIISA sub-counties found in Buliisa, rather than in Masindi, and MUNTU found in Amolotar (not Lira
    # MALONGO1 also renamed from MALONGO in MAYUGE as there are 2 MALONGO sub-counties (one if MASAKA), so need to make unique
    
    sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
    
    sc_names$MDA <- ifelse(sc_names$Subcounty_name %in% MDA_subcounties, "MDA","none") # code whether MDA or not
    
    sc_names <- sc_names  %>% rename(SNAME_2006 = Subcounty_name, DNAME_2006 = District_name) # rename column
    
    UGA_subcounties_tidy$SNAME_2006 <- ifelse(UGA_subcounties_tidy$DNAME_2006 == "MAYUGE" & UGA_subcounties_tidy$SNAME_2006 == "MALONGO", 
                                          "MALONGO1", UGA_subcounties_tidy$SNAME_2006) # must also change any duplicate sub-counties with MDA in this object
    
    # ==================================================== #
    # make MDA yes or no variable for sub counties with MDA
    UGA_subcounties_tidy <- left_join(UGA_subcounties_tidy, sc_names) # join boundary data to MDA presence data
    
    UGA_subcounties_tidy$MDA <- as.factor(UGA_subcounties_tidy$MDA) # make MDA presence a factor
    
    MDA.SC.col <- c("purple2", NA) # to colour MDA districts
    
    MDA.SC.vec <- MDA.SC.col[UGA_subcounties_tidy$MDA] # specify colour for each polygon
    
    UGA_subcounties_tidy$MDA_colour <- MDA.SC.vec # new column for fill in ggplot depending on MDA
    
    UGA_subcounties_tidy$label <- ifelse(UGA_subcounties_tidy$MDA == "MDA", 
                                     UGA_subcounties_tidy$SNAME_2006, NA)
  }
  
  if (year == 2012){
    
    MDA_subcounties <-
      c("ABER", "ACHOLIBUR","ADILANG", "ADJUMANI TC", "AGORO", "AGWATA", "AKALO", "AKOKORO",
        "ALERO", "ALOI", "AMURU", "ANAKA", "APUTI", "ATANGA", "ATIAK", "AWACH", "AWELO",
        "AWERE", "BAGEZZA", "BAITAMBOGWE", "BANDA", "BAR-DEGE", "BBAALE", "BELEAFE",
        "BIISO", "BOBI", "BUDONDO", "BUFUMIRA", "BUHEHE", "BUJJUMBA", "BUKAKATA","BUKULULA",
        "BULERA", "BULIISA", "BULULU", "BUNGATIRA", "BUSAMUZI", "BUSERUKA", "BUSIMBI",
        "BUTOLOOGO", "BUWAMA", "BUWUNGA1", "BUYENDE", "BWAMBARA", "BWEEMA", "BWIKARA",
        "CENTRAL DIVISION1", "CHAWENTE", "CIFORO", "DUFILE", "DZAIPI", "GALIRAAYA", "GIMARA",
        "IBUJE", "IBULANKU", "IMMANYIRO", "INOMO", "ITULA", "IVUKULA", "KABWOYA", "KADUNGULU",
        "KAGULU", "KAKOMONGOLE", "KALANGALA TC", "KALONGO", "KALUNGI", "KANARA", "KANGAI",
        "KANGULUMIRA", "KARUGUTU", "KASANJE", "KATABI","KATUNGURU", "KATWE KABATORO TC", "KAYONZA1",
        "KEI", "KICHWAMBA", "KICUZI", "KIDERA","KIGANDALO","KIGOROBYA","KIGOROBYA TC",
        "KILAK","KITIGUM MATIDI","KITGUM TC", "KITYERERA","KKOME ISLANDS","KOBOKO TC","KORO",
        "KULUBA", "KURU", "KYAMUSWA", "KYANAMUKAAKA", "KYANGWALI", "LABONGO AKWANG", 
        "LABONGO AMIDA", "LABONGO LAYAMO", "LAGORO", "LAGUTI", "LAKWANA", "	LALOGI", 
        "LAMOGI", "LAPUL", "LAROO", "LAYIBI", "LEFORI", "LIRA PALWO", "LOBULE", "LOKUNG",
        "LOREGAE", "LORO", "LUDARA", "LUMINO", "LUNYO", "LWABYATA", "LWAMPANGA", "LYAMA",
        "MADI OPEI", "MALONGO1", "MASABA", "MASINDI PORT", "MAZINGA", "METU", "MIDIA",
        "MIDIGO", "MOLO", "MOYO", "MPEEFU", "MUCWINI", "MUGOYE", "MUHOKYA", "MUNTU",
        "MUTUMBA", "NABISWERA", "NAJJA", "NAKISUNGA", "NAMALU", "NAMANYONYI", "NAMASALE",
        "NAMBIESO", "	NAMOKORA", "NAMUGONGO", "NANKOMA", "NGOGWE", "NKONDO", "NTENJERU",
        "NYENGA", "ODEK", "OFUA", "OGOKO", "OKOLLO", "OMIYA ANYIMA", "OMOT", "OMUGO", "ONGAKO",
        "ONGINO", "OROM", "OTWAL", "PABBO", "PADER TC", "PADIBE EAST", "PADIBE WEST",
        "PAICHO", "PAIMOL", "PAJULE", "PAKELLE", "PAKWACH", "PAKWACH TC", "PALABEK GEM",
        "PALABEK KAL", "PALARO", "PALOGA", "PANYIMUR","PANYANGO","PATIKO","PATONGO", "PECE",
        "PINGIRE","PURANGA","PURONGO","RHINO CAMP","RIGBO","ROMOGI","RUGASHARI","	RUTEETE",
        "RWEBISENGO","SIBANGA","SIGULU ISLANDS","SSI-BUKUNJA","SSISA","WADELAI","WAKISI",
        "WOL") 
   
    sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
    
    sc_names$MDA <- ifelse(sc_names$Subcounty_name %in% MDA_subcounties, "MDA","none") # code whether MDA or not
    
    sc_names <- sc_names  %>% rename(SNAME_2010 = Subcounty_name, DNAME_2010 = District_name) # rename column
    
    UGA_subcounties_tidy$SNAME_2010 <- ifelse(UGA_subcounties_tidy$DNAME_2010 == "MAYUGE" & UGA_subcounties_tidy$SNAME_2010 == "MALONGO", 
                                              "MALONGO1", UGA_subcounties_tidy$SNAME_2010) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$SNAME_2010 <- ifelse(UGA_subcounties_tidy$DNAME_2010 == "MASAKA" & UGA_subcounties_tidy$SNAME_2010 == "BUWUNGA", 
                                              "BUWUNGA1", UGA_subcounties_tidy$SNAME_2010) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$SNAME_2010 <- ifelse(UGA_subcounties_tidy$DNAME_2010 == "JINJA" & UGA_subcounties_tidy$SNAME_2010 == "CENTRAL DIVISION", 
                                              "CENTRAL DIVISION1", UGA_subcounties_tidy$SNAME_2010) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$SNAME_2010 <- ifelse(UGA_subcounties_tidy$DNAME_2010 == "KAYUNGA" & UGA_subcounties_tidy$SNAME_2010 == "KAYONZA", 
                                              "KAYONZA1", UGA_subcounties_tidy$SNAME_2010) # must also change any duplicate sub-counties with MDA in this object
    
    # ==================================================== #
    # make MDA yes or no variable for sub counties with MDA
    UGA_subcounties_tidy <- left_join(UGA_subcounties_tidy, sc_names) # join boundary data to MDA presence data
    
    UGA_subcounties_tidy$MDA <- as.factor(UGA_subcounties_tidy$MDA) # make MDA presence a factor
    
    MDA.SC.col <- c("purple2", NA) # to colour MDA districts
    
    MDA.SC.vec <- MDA.SC.col[UGA_subcounties_tidy$MDA] # specify colour for each polygon
    
    UGA_subcounties_tidy$MDA_colour <- MDA.SC.vec # new column for fill in ggplot depending on MDA
    
    UGA_subcounties_tidy$label <- ifelse(UGA_subcounties_tidy$MDA == "MDA", 
                                         UGA_subcounties_tidy$SNAME_2010, NA)
  }
  
  if (year == 2015){
    
    MDA_subcounties <-
      c("ABANGA", "ADEKNINO", "ADOK", "AGWATA", "APOPONG", "AROMO", "BALAWOLI", "BATTA",
        "BUFUMBO", "BUGOBERO", "BUGULUMBYA", "BUKHALU", "BUKHOFU", "BUKOOMA", "BUKULULA",
        "BUKUYA", "BULANGE", "BULONGO", "BUMANYA", "BUREMBA", "BURUNGA", "BUTEBO", "BUTIRU",
        "BUYANGA1", "BWAMBARA", "CHELEKURA", "GADUMIRE", "GOGONYO", "IBULANKU", "IKUMBYA",
        "IVUKULA", "IYOLWA", "JANGOKORO", "KABIRA1", "KABWERI", "KACHONGA", "KACHUMBALA",
        "KADAMA", "KAGAMBA", "KAKOMONGOLE", "KAKOOGE", "KAKUUTO", "KALIRO TOWN COUNCIL",
        "KALISIZO", "KALONGO", "KALUNGI", "KAMEKE", "KAMERUKA", "KAMONKOLI", "KAMUDA",
        "KANGAI","KANYARYERU","KAPIR","KAPUJAN","KASASIRA", "KASAMBYA", "KASHUMBA",
        "KASODO", "KASSANDA", "KATIIRA", "KIBAALE", "KIBUKU", "KICUZI", "KIDONGOLE",
        "KIJOMORO", "KIJONGO", "KIRIKA", "KISOZI", "KOBWIN", "KOLIR", "KUMI", "KWERA",
        "KYALULANGIRA", "KYEBE", "LOREGAE", "LUKAYA TOWN COUNCIL", "LWABIYATA", 
        "LWAMPANGA", "LYAMA", "MADI  OPEI", "MAGADA", "MAGOLA", "MAGORO", "MALABA TOWN COUNCIL",
        "MASHA", "MAZIMASA", "MBULAMUTI", "MELLA", "MERIKIT", "MOLO", "MUKONGORO", 
        "MUKUJU", "MUKURA", "MULANDA", "MYANZI", "NABISWEERA", "NABUYOGA", "NAKITOMA",
        "NAMALU", "NAMASAGALI", "NAMALEMBA", "NAMUTUMBA", "NAMUTUMBA TOWN COUNCIL", 
        "NAMUGONGO", "NAMWIWA","NANSANGA", "NAWAIKOKE", "NAWANDALA", "NGARAMA", "NGENGE",
        "NGORA", "NKUNGU", "NSINZE", "NYADRI", "NYAKASHASHARA", "NYAPEA", "OKWALONGWEN",
        "OKWONGODUL", "OLEBA", "OLOK", "OLUFFE", "OLUVU", "OMODOI", "ONGINO", "OSUKURU",
        "PAIDHA", "PALABEK GEM", "PALABEK KAL", "PALABEK OGILI", "PALOGA", "PAYA", 
        "PUTI-PUTI", "RAKAI TOWN COUNCIL", "RUBONGI", "SANGA", "SIRONKO TOWN COUNCIL", 
        "SISUNI", "TARA", "TIRINYI", "TOROMA", "WABINYONYI", "WANKOLE", "YIVU", 
        "ZOMBO TOWN COUNCIL") 
  
    sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
    
    sc_names$MDA <- ifelse(sc_names$Subcounty_name %in% MDA_subcounties, "MDA","none") # code whether MDA or not
    
    sc_names <- sc_names  %>% rename(Subcounty = Subcounty_name, District = District_name) # rename column
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "BUGWERI" & UGA_subcounties_tidy$Subcounty == "BUYANGA", 
                                              "BUYANGA1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "KYOTERA" & UGA_subcounties_tidy$Subcounty == "KABIRA", 
                                             "KABIRA1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
  }
  
  if (year == 2016){
    
    MDA_subcounties <-
      c("BBAALE", "BUBEKE", "BUFUMIRA","BUHEHE","BUJUMBA", "BUKAKATA", "BURORA",
        "BUSAANA", "BUSAMUZI", "BUSIME", "BUVUMA TOWN COUNCIL", "BUWAMA", "BUWOOYA",
        "BWEEMA", "BWIKARA", "GALIRAYA", "KABULASOKE", "KALANGALA TOWN COUNCIL",
        "KAMMENGO", "KANGULUMIRA", "KANONI TOWN COUNCIL", "KASANJE", "KASENDA",
        "KATABI  TOWN COUNCIL", "KAYONZA1", "KAYUNGA", "KIBIITO", "KOOME ISLAND",
        "KYAKABADIIMA", "KYAMUSWA", "KYANAMUKAAKA", "KYATEREKERA", "KYEGONZA",
        "KYESIIGA", "LUBYA", "LUMINO", "LUNYO", "LWAJJE", "LYABAANA", "MABAALE",
        "MADDU", "MAJANJI", "MASABA1", "MAZINGA", "MPATTA", "MPEEFU", "MPUNGE",
        "MUGOYE", "MUHORRO", "MUHORRO  TOWN COUNCIL", "MUKUNGWE", "NAIRAMBI",
        "NAJJA", "NAKISUNGA", "NAZIGO", "NDAIGA", "NGOGWE", "NJERU DIVISION",
        "NKOZI", "NTENJERU", "NYENGA DIVISION", "RUGASHARI", "RUTEETE1",
        "SSI-BUKUNJA", "WAKISI DIVISION") 
    
    sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
    
    sc_names$MDA <- ifelse(sc_names$Subcounty_name %in% MDA_subcounties, "MDA","none") # code whether MDA or not
    
    sc_names <- sc_names  %>% rename(Subcounty = Subcounty_name, District = District_name) # rename column
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "KAYUNGA" & UGA_subcounties_tidy$Subcounty == "KAYONZA", 
                                             "KAYONZA1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
 
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "BUSIA" & UGA_subcounties_tidy$Subcounty == "MASABA", 
                                             "MASABA1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "KABAROLE" & UGA_subcounties_tidy$Subcounty == "RUTEETE", 
                                             "RUTEETE1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
  }
  
  if (year == 2018){
    
    MDA_subcounties <-
      c("ABAKO","ABIA","ABUKU","ADUKU","ADILANG","AGAGO TOWN COUNCIL","AGALI",
        "AGIKDAK","AGWENG","AGWINGIRI","AKOKORO","AKURA","AKWON","ALEBTONG TOWN COUNCIL",
        "ALOI","AMOLATAR TOWN COUNCIL","AMUGU","APAC","APERKIRA","APALA","AROI",
        "APUTI", "AROMO", "ARUM", "ARWOTCEK", "ATIIRA", "AWEI", "AWELO",
        "BAITAMBOGWE", "BANDA1", "BARR", "BBAALE", "BBANDA", "BUBEKE", "BUDHAYA",
        "BUDONDO", "BUFUMIRA", "BUGAYA1", "BUGEMBE TOWN COUNCIL", "BUGONDO",
        "BUHEHE", "BUHEMBA", "BUJUMBA", "BUKAKATA", "BUKABOOLI", "BUKANA",
        "BUKATUBE", "BULESA", "BULIDHA", "BULUGUYI", "BULULU", "BURORA", "BUSAANA",
        "BUSAKIRA", "BUSAMUZI", "BUSERUKA", "BUSIMBI DIVISION", "BUSIME", "BUSORO",
        "BUSSI", "BUSWALE", "BUTAGAYA", "BUTUNGAMA", "BUVUMA TOWN COUNCIL",
        "BUWAMA", "BUWOOYA", "BUWUNGA", "BUYENDE", "BUYENDE TOWN COUNCIL",
        "BUYENGO", "BUYINJA", "BWEEMA", "BWERAMULE", "BWIKARA", "CENTRAL DIVISION1",
        "CHAWENTE", "CHEGERE", "DRANYA", "ETAM", "GALIRAYA", "IBUJE", "JAGUZI",
        "CENTRAL DIVISION2", "KABERAMAIDO", "KABULASOKE", "KABWOYA", "KADUNGULU",
        "KAGULU", "KAJJANSI TOWN COUNCIL", "KAKIRA TOWN COUNCIL1", "KAKURE",
        "KALAKI", "KALONGO TOWN COUNCIL", "KAMMENGO", "KANARA1", "KANARA TOWN COUNCIL",
        "KANGULUMIRA", "KASANJE", "KASENDA", "KATABI  TOWN COUNCIL", "KATETA",
        "KATUNGURU", "KAYUNGA", "KAYONZA1", "KERWA", "KIBIITO", "KICWAMBA","KIDERA",
        "KIGOROBYA", "KIGUMBA", "KIRYANDONGO", "KITYERERA","KIYOMBYA","KOCHI",
        "KOBULUBULU","KOOME ISLAND","KOTOMOL","KULUBA","KULULU","KURU",
        "KYAKABADIIMA","KYAMUSWA","KYANAMUKAAKA","KYANGWALI","KYATEREKERA",
        "KYEGONZA","LAMIYO","LAPONO","LIRA PALWO","LOBULE","LOLWE","LUBYA",
        "LUDARA","LUMINO","LUNYO","LWAJJE","LYABAANA","MAANYI","MABAALE",
        "MAFUBIRA","MAJANJI","MALONGO1","NAMAYINGO TOWN COUNCIL","MASABA1",
        "MASAJJA DIVISION", "MASINDI PORT", "MAZINGA", "MIDIA", "MIDIGO","MPATTA",
        "MPEEFU", "MPUNGE", "MPUMUDDE DIVISION", "MUGOYE", "MUHORRO",
        "MUHORRO  TOWN COUNCIL", "MUKUNGWE", "MUNTU", "MUTUMBA", "MUTUNDA",
        "NAIRAMBI", "NAJJA", "NAKISUNGA","NAMBIESO","NAZIGO","NDAIGA","NDEJJE DIVISION",
        "NKONDO","NJERU DIVISION","NORTHERN DIVISON1","NTENJERU","NYENGA DIVISION",
        "OCHERO", "ODRAVU","OGUR","OMIYA PACWA","OMORO","OMOT","PAIMOL","PARABONGO",
        "PATONGO","PATONGO TOWN COUNCIL","PINGIRE","ROMOGI","RUGASHARI","RUTEETE",
        "RWEBISENGO","RWIMI","SIGULU ISLAND","SOUTHERN DIVISION1","SSI-BUKUNJA",
        "WAIRASA", "WAKISI DIVISION", "WESTERN DIVISION1", "WOL") 
    
    sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
    
    sc_names$MDA <- ifelse(sc_names$Subcounty_name %in% MDA_subcounties, "MDA","none") # code whether MDA or not
    
    sc_names <- sc_names  %>% rename(Subcounty = Subcounty_name, District = District_name) # rename column
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "NAMAYINGO" & UGA_subcounties_tidy$Subcounty == "BANDA", 
                                             "BANDA1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "BUVUMA" & UGA_subcounties_tidy$Subcounty == "BUGAYA", 
                                             "BUGAYA1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "MITYANA" & UGA_subcounties_tidy$Subcounty == "CENTRAL DIVISION", 
                                             "CENTRAL DIVISION1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "JINJA" & UGA_subcounties_tidy$Subcounty == "CENTRAL DIVISION", 
                                             "CENTRAL DIVISION2", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "JINJA" & UGA_subcounties_tidy$Subcounty == "KAKIRA TOWN COUNCIL", 
                                             "KAKIRA TOWN COUNCIL1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "NTOROKO" & UGA_subcounties_tidy$Subcounty == "KANARA", 
                                             "KANARA1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "KAYUNGA" & UGA_subcounties_tidy$Subcounty == "KAYONZA", 
                                             "KAYONZA1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "MAYUGE" & UGA_subcounties_tidy$Subcounty == "MALONGO", 
                                             "MALONGO1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "BUSIA" & UGA_subcounties_tidy$Subcounty == "MASABA", 
                                             "MASABA1", UGA_subcounties_tidy$Subcounty) 
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "KOBOKO" & UGA_subcounties_tidy$Subcounty == "NORTHERN DIVISON", 
                                             "NORTHERN DIVISON1", UGA_subcounties_tidy$Subcounty) 
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "KABAROLE" & UGA_subcounties_tidy$Subcounty == "RUTEETE", 
                                             "RUTEETE1", UGA_subcounties_tidy$Subcounty) # must also change any duplicate sub-counties with MDA in this object
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "KOBOKO" & UGA_subcounties_tidy$Subcounty == "SOUTHERN DIVISON", 
                                             "SOUTHERN DIVISON1", UGA_subcounties_tidy$Subcounty) 
    
    UGA_subcounties_tidy$Subcounty <- ifelse(UGA_subcounties_tidy$District == "KOBOKO" & UGA_subcounties_tidy$Subcounty == "WESTERN DIVISON", 
                                             "WESTERN DIVISON1", UGA_subcounties_tidy$Subcounty) 
    
  }
  
  if (year == 2015 || year == 2016 || year == 2018){
    # ==================================================== #
    # make MDA yes or no variable for sub counties with MDA
    UGA_subcounties_tidy <- left_join(UGA_subcounties_tidy, sc_names) # join boundary data to MDA presence data
    
    UGA_subcounties_tidy$MDA <- as.factor(UGA_subcounties_tidy$MDA) # make MDA presence a factor
    
    MDA.SC.col <- c("purple2", NA) # to colour MDA districts
    
    MDA.SC.vec <- MDA.SC.col[UGA_subcounties_tidy$MDA] # specify colour for each polygon
    
    UGA_subcounties_tidy$MDA_colour <- MDA.SC.vec # new column for fill in ggplot depending on MDA
    
    UGA_subcounties_tidy$label <- ifelse(UGA_subcounties_tidy$MDA == "MDA", 
                                         UGA_subcounties_tidy$Subcounty, NA)
    }
  
  
# ===========================================================================================================#
#  Extract district MDAs (valid for all MDAs across 2033-2019 when analysing by original districts of 2003)  #

district_map_0319 <- UGA_district_boundaries_function(shape_file = districts_2001, national_map_input = national_map) 
 
district_names_0319 <- district_name_func(shape_file = districts_2001) # using original districts throughout 2003-2019

UGA_dist_MDA_names <- district_names_0319 # copy variable (dist names)

if (year == 2004){
    # repeat for districts to highlight & check sub-counties (for each district) for 2004#
    MDA_districts <-
    c("APAC", "MOYO", "ADJUMANI", "ARUA", "NEBBI", "LIRA", "NAKASONGOLA", "MASINDI", "HOIMA", "BUGIRI",
    "BUSIA", "KAYUNGA", "JINJA", "MUKONO", "WAKISO", "MAYUGE", "BUNDIBUGYO", "KIBAALE") # vector of districts with MDA in 2003
} 

if (year == 2012){
  # repeat for districts to highlight & check sub-counties (for each district) for 2004#
  MDA_districts <-
    c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA","KABERAMAIDO",
      "NAKASONGOLA","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
      "MAYUGE","MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","KIBAALE","SOROTI","PALLISA",
      "KAMULI","MASINDI","ADJUMANI","MASAKA","MBARARA","RUKUNGIRI","BUSHENYI",
      "KAMWENGE","KASESE","KALANGALA") # vector of districts with MDA in 2003
} 

if (year == 2015){
  # repeat for districts to highlight & check sub-counties (for each district) for 2004#
  MDA_districts <-c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA",
    "KABERAMAIDO","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
    "MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","KIBAALE","SOROTI","MAYUGE",
    "KAMULI","MASINDI","ADJUMANI","MASAKA","RUKUNGIRI","BUSHENYI",
    "KAMWENGE","KASESE","KALANGALA","IGANGA","MBALE","SIRONKO",
    "KAPCHORWA","KATAKWI","KUMI","MBARARA","NAKAPIRIPIRIT","NAKASONGOLA","PALLISA",
    "RAKAI","TORORO")
} 


if (year == 2016){
  MDA_districts <-
    c("MUBENDE","BUNDIBUGYO","BUSIA","KABAROLE","KALANGALA","KAYUNGA",
      "MASINDI","MPIGI","MUKONO","WAKISO")
}

if (year == 2018){
  MDA_districts <- c("PADER","APAC","KITGUM","MOYO","YUMBE","ARUA","NEBBI","GULU","LIRA",
    "KABERAMAIDO","HOIMA","BUGIRI","BUSIA","KAYUNGA","JINJA","MUKONO","WAKISO",
    "MPIGI","KABAROLE","MUBENDE","BUNDIBUGYO","SOROTI","MAYUGE",
    "KAMULI","MASINDI","ADJUMANI","MASAKA","BUSHENYI","KAMWENGE","KASESE",
    "KALANGALA")
  
}

  UGA_dist_MDA_names$MDA <- ifelse(district_names_0319$Dist_name %in% MDA_districts, "MDA","none") # code whether MDA or not
  
  UGA_dist_MDA_names <- UGA_dist_MDA_names  %>% rename(DISTRICT = Dist_name) # rename column
  
  UGA_districts_tidy <- left_join(district_map_0319[[2]], UGA_dist_MDA_names) # join boundary data to MDA presence data
  
  UGA_districts_tidy$MDA <- as.factor(UGA_districts_tidy$MDA) # make MDA presence a factor
  
  MDA.dist.col <- c("blue",NA) # to colour MDA district
  
  MDA.dist.vec <- MDA.dist.col[UGA_districts_tidy$MDA] # specify colour for each polygon
  
 UGA_districts_tidy$MDA_colour <- MDA.dist.vec # new column for fill in ggplot depending on MDA
 
#======================= 
# make labels for plot

if (year == 2004){
# make labels (sub-counties with MDA) for plotting
UGA_subcounties_tidy_subset <- subset(UGA_subcounties_tidy, MDA=="MDA")   #subset just for NYS
scnames <- aggregate(cbind(long, lat) ~ SNAME_2006, data=UGA_subcounties_tidy_subset, FUN=mean)
scnames$label <- scnames$SNAME_2006
}

if (year == 2012){
  # make labels (sub-counties with MDA) for plotting
  UGA_subcounties_tidy_subset <- subset(UGA_subcounties_tidy, MDA=="MDA")   #subset just for NYS
  scnames <- aggregate(cbind(long, lat) ~ SNAME_2010, data=UGA_subcounties_tidy_subset, FUN=mean)
  scnames$label <- scnames$SNAME_2010
}
  
if (year == 2015 || year == 2016 || year == 2018){
    # make labels (sub-counties with MDA) for plotting
    UGA_subcounties_tidy_subset <- subset(UGA_subcounties_tidy, MDA=="MDA")   #subset just for NYS
    scnames <- aggregate(cbind(long, lat) ~ Subcounty, data=UGA_subcounties_tidy_subset, FUN=mean)
    scnames$label <- scnames$Subcounty
  }
  
# PLOT
  
  if (year == 2004){
    UGA_districts_tidy_subset <- subset(UGA_districts_tidy, MDA=="MDA")
    
    plot1 <- ggplot() +
      geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
      geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
      geom_polygon(data= UGA_districts_tidy_subset, aes(x = long, y = lat, group = group, colour= MDA_colour), fill= "purple", size = 1, alpha=0.25)+
      geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 0.75, fill=NA, alpha=NA)+
      coord_equal(ratio = 1)+
      scale_colour_manual(values=c("blue","purple2",NA), guide=FALSE)+
      ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
      theme_void()+
      theme(
        plot.title = element_text(color="black", size=16, face="bold.italic"))+
      guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
  }
  
  if (year == 2012 || year == 2015 || year == 2016 || year == 2018){
    
    UGA_districts_tidy_subset <- subset(UGA_districts_tidy, MDA=="MDA")   # subset MDA districts to plot
    
    plot1 <- ggplot() +
      geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
      geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
      geom_polygon(data= UGA_districts_tidy_subset, aes(x = long, y = lat, group = group, colour= MDA_colour), fill= "purple", size = 1, alpha=0.25)+
      geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 0.75, fill=NA, alpha=NA)+
      coord_equal(ratio = 1)+
      scale_colour_manual(values=c("blue","purple2",NA), guide=FALSE)+
      ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 2.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
      theme_void()+
      theme(
        plot.title = element_text(color="black", size=16, face="bold.italic"))+
      guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
  }
  
 
return(list(UGA_subcounties_tidy, UGA_districts_tidy, plot1, scnames, UGA_subcounties_tidy_subset, UGA_districts_tidy_subset))
} 

#===========================================================================================================================================#
#========================================================================#
#  identifying sub-counties where PCC surveys have been conducted        #
#========================================================================#

subcounties_name_func3 <- function(shape_file, year){
  
 
  if(year == 2011){
    
    UGA_SC_PCCstudies_names <- data.frame(Subcounty_name = shape_file@data$SNAME_2010,
                                   District_name = shape_file@data$DNAME_2010) 
    
    UGA_SC_PCCstudies_names <- with(UGA_SC_PCCstudies_names,  UGA_SC_PCCstudies_names[order(District_name) , ])
    
  }
  
  if(year == 2013 || year == 2015 || year == 2019){
    
    UGA_SC_PCCstudies_names <- data.frame(Subcounty_name = shape_file@data$Subcounty,
                                   District_name = shape_file@data$District)
    
    UGA_SC_PCCstudies_names <- with(UGA_SC_PCCstudies_names,  UGA_SC_PCCstudies_names[order(District_name) , ])
    

  }
  

  # if(year == 2019){
  #   
  #   UGA_SC_PCCstudies_names <- data.frame(Subcounty_name = shape_file@data$Subcounty,
  #                                  District_name = shape_file@data$District)
  # }
  
  return(UGA_SC_PCCstudies_names)
  
}


subcounty_PCCstudies_processing_plotting_func <- function(sc_names, UGA_subcounties_tidy, district_2001, national_map_input, PCC_survey_year){
  
  # =============================================================#
  #  Extract sub-county locations where PCC studies have occured #
  
  
  if (PCC_survey_year == 2011){
    
    PCCstudy_subcounties <-
      c("OCHERO", "GALIRAYA", "KIDERA", "NAWAIKOKE", "BULULU", "NAMBIESO",
        "AWELO", "ADUMI", "LUMINO", "MUHORRO", "KALUNGU", "GADUMIRE") 
    
    sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
    
    sc_names$PCC_survey <- ifelse(sc_names$Subcounty_name %in% PCCstudy_subcounties, "PCC survey","none") # code whether MDA or not
    
    sc_names <- sc_names  %>% rename(SNAME_2010 = Subcounty_name, DNAME_2010 = District_name) # rename column
  
    # ============================================================ #
    # make MDA yes or no variable for sub counties with PCC survey #
    UGA_subcounties_tidy <- left_join(UGA_subcounties_tidy, sc_names) # join boundary data to MDA presence data
    
    UGA_subcounties_tidy$PCC_survey <- as.factor(UGA_subcounties_tidy$PCC_survey) # make MDA presence a factor
    
    PCC_survey.SC.col <- c("purple2", NA) # to colour MDA districts
    
    PCC_survey.SC.vec <- PCC_survey.SC.col[UGA_subcounties_tidy$PCC_survey] # specify colour for each polygon
    
    UGA_subcounties_tidy$PCC_survey_colour <- PCC_survey.SC.vec # new column for fill in ggplot depending on MDA
    
    UGA_subcounties_tidy$label <- ifelse(UGA_subcounties_tidy$PCC_survey == "PCC survey", 
                                         UGA_subcounties_tidy$SNAME_2010, NA)
  }
  
  #================================================================#
  #  For PCC study locations >2012 (using a different shape file)  #
  
  if (PCC_survey_year == 2013){
    
    PCCstudy_subcounties <-
      c("BUTANSI", "KITAYUNJWA", "NAMWENDWA", "BUGULUMBYA", "KABONERA", "KATWE-BUTEGO DIVISION",
        "KIMAANYA-KYABAKUZA DIVISION", "KKINGO", "KYANAMUKAAKA", "NYENDO-SSENYANGE DIVISION",
        "KYAMPISI", "NTENJERU") # there is no Mukono TC sub-county
    
    sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
    
    sc_names$PCC_survey <- ifelse(sc_names$Subcounty_name %in% PCCstudy_subcounties, "PCC survey","none") # code whether MDA or not
    
    sc_names <- sc_names  %>% rename(Subcounty = Subcounty_name, District = District_name) # rename column
    
  }
  
  
  if (PCC_survey_year == 2015){
    
    PCCstudy_subcounties <-
      c("OJWINA DIVISION", "BARR", "LIRA", "ADYEL DIVISION", "ADEKOKWOK", "MOYO", "METU") 
    
    sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
    
    sc_names$PCC_survey <- ifelse(sc_names$Subcounty_name %in% PCCstudy_subcounties, "PCC survey","none") # code whether MDA or not
    
    sc_names <- sc_names  %>% rename(Subcounty = Subcounty_name, District = District_name) # rename column
    
  }
  
  if (PCC_survey_year == 2019){
    
    PCCstudy_subcounties <-
      c("PAJULU","LAMOGI","UNYAMA") 
    
    sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
    
    sc_names$PCC_survey <- ifelse(sc_names$Subcounty_name %in% PCCstudy_subcounties, "PCC survey","none") # code whether MDA or not
    
    sc_names <- sc_names  %>% rename(Subcounty = Subcounty_name, District = District_name) # rename column
    
    }
  
  if (PCC_survey_year == 2013 || PCC_survey_year == 2015 || PCC_survey_year == 2019){
    # ==================================================== #
    # make MDA yes or no variable for sub counties with MDA
    UGA_subcounties_tidy <- left_join(UGA_subcounties_tidy, sc_names) # join boundary data to MDA presence data
    
    UGA_subcounties_tidy$PCC_survey <- as.factor(UGA_subcounties_tidy$PCC_survey) # make MDA presence a factor
    
    PCC_survey.SC.col <- c("purple2", NA) # to colour MDA districts
    
    PCC_survey.SC.vec <- PCC_survey.SC.col[UGA_subcounties_tidy$PCC_survey] # specify colour for each polygon
    
    UGA_subcounties_tidy$PCC_survey_colour <- PCC_survey.SC.vec # new column for fill in ggplot depending on MDA
    
    UGA_subcounties_tidy$label <- ifelse(UGA_subcounties_tidy$PCC_survey == "PCC survey", 
                                         UGA_subcounties_tidy$Subcounty, NA)
  }
  
  
  # ===========================================================================================================#
  #  Extract district MDAs (valid for all MDAs across 2033-2019 when analysing by original districts of 2003)  #
  
  district_map_0319 <- UGA_district_boundaries_function(shape_file = districts_2001, national_map_input = national_map) 
  
  district_names_0319 <- district_name_func(shape_file = districts_2001) # using original districts throughout 2003-2019
  
  UGA_dist_PCCsurvey_names <- district_names_0319 # copy variable (dist names)
  
  # if (year == 2004){
  #   # repeat for districts to highlight & check sub-counties (for each district) for 2004#
  #   MDA_districts <-
  #     c("APAC", "MOYO", "ADJUMANI", "ARUA", "NEBBI", "LIRA", "NAKASONGOLA", "MASINDI", "HOIMA", "BUGIRI",
  #       "BUSIA", "KAYUNGA", "JINJA", "MUKONO", "WAKISO", "MAYUGE", "BUNDIBUGYO", "KIBAALE") # vector of districts with MDA in 2003
  # } 
  
  if (PCC_survey_year == 2011){
   
    PCC_survey_districts <-
      c("KABERAMAIDO","KAYUNGA", "KAMULI", "APAC", "LIRA", "ARUA", "BUSIA", "KIBAALE",
        "MASAKA") # vector of districts with MDA in 2003
  } 
  
  if (PCC_survey_year == 2013){
    
    PCC_survey_districts <-
      c("LWENGO", "MUKONO", "MASAKA", "KAMULI")
  } 
  
  if (PCC_survey_year == 2015){
    
    PCC_survey_districts <-
      c("LIRA", "MOYO")
  } 
  
  if (PCC_survey_year == 2019){
    
    PCC_survey_districts <-
      c("ARUA","GULU", "AMURU")
  } 
  
  UGA_dist_PCCsurvey_names$PCC_survey <- ifelse(district_names_0319$Dist_name %in% PCC_survey_districts, "PCC survey","none") # code whether MDA or not
  
  UGA_dist_PCCsurvey_names <- UGA_dist_PCCsurvey_names  %>% rename(DISTRICT = Dist_name) # rename column
  
  UGA_districts_tidy <- left_join(district_map_0319[[2]], UGA_dist_PCCsurvey_names) # join boundary data to MDA presence data
  
  UGA_districts_tidy$PCC_survey <- as.factor(UGA_districts_tidy$PCC_survey) # make MDA presence a factor
  
  PCC_survey.dist.col <- c("blue",NA) # to colour MDA district
  
  PCC_survey.dist.vec <- PCC_survey.dist.col[UGA_districts_tidy$PCC_survey] # specify colour for each polygon
  
  UGA_districts_tidy$PCC_survey_colour <- PCC_survey.dist.vec # new column for fill in ggplot depending on MDA
  
  #======================= 
  # make labels for plot
  
  # if (year == 2004){
  #   # make labels (sub-counties with MDA) for plotting
  #   UGA_subcounties_tidy_subset <- subset(UGA_subcounties_tidy, MDA=="MDA")   #subset just for NYS
  #   scnames <- aggregate(cbind(long, lat) ~ SNAME_2006, data=UGA_subcounties_tidy_subset, FUN=mean)
  #   scnames$label <- scnames$SNAME_2006
  # }
  
  if (PCC_survey_year == 2011){
    # make labels (sub-counties with MDA) for plotting
    UGA_subcounties_tidy_subset <- subset(UGA_subcounties_tidy, PCC_survey=="PCC survey")   #subset just for NYS
    scnames <- aggregate(cbind(long, lat) ~ SNAME_2010, data=UGA_subcounties_tidy_subset, FUN=mean)
    scnames$label <- scnames$SNAME_2010
  }
  
  if (PCC_survey_year == 2013 || PCC_survey_year == 2015 || PCC_survey_year == 2019){
    # make labels (sub-counties with MDA) for plotting
    UGA_subcounties_tidy_subset <- subset(UGA_subcounties_tidy, PCC_survey=="PCC survey")   #subset just for NYS
    scnames <- aggregate(cbind(long, lat) ~ Subcounty, data=UGA_subcounties_tidy_subset, FUN=mean)
    scnames$label <- scnames$Subcounty
  }
  
  # PLOT
  
  # if (year == 2004){
  #   UGA_districts_tidy_subset <- subset(UGA_districts_tidy, MDA=="MDA")
  #   
  #   plot1 <- ggplot() +
  #     geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
  #     geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
  #     geom_polygon(data= UGA_districts_tidy_subset, aes(x = long, y = lat, group = group, colour= MDA_colour), fill= "purple", size = 1, alpha=0.25)+
  #     geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 0.75, fill=NA, alpha=NA)+
  #     coord_equal(ratio = 1)+
  #     scale_colour_manual(values=c("blue","purple2",NA), guide=FALSE)+
  #     ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
  #     theme_void()+
  #     theme(
  #       plot.title = element_text(color="black", size=16, face="bold.italic"))+
  #     guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
  # }
  
  if (PCC_survey_year == 2011 || PCC_survey_year == 2013 || PCC_survey_year == 2015 || PCC_survey_year == 2019){
    
    UGA_districts_tidy_subset <- subset(UGA_districts_tidy, PCC_survey=="PCC survey")   # subset MDA districts to plot
    
    plot1 <- ggplot() +
      geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
      geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
      geom_polygon(data= UGA_districts_tidy_subset, aes(x = long, y = lat, group = group, colour= PCC_survey_colour), fill= "orange", size = 1, alpha=0.25)+
      geom_polygon(data= UGA_subcounties_tidy_subset, aes(x = long, y = lat, group = group, colour= PCC_survey), size = 0.75, fill=NA, alpha=NA)+
      coord_equal(ratio = 1)+
      scale_colour_manual(values=c("blue","purple2",NA), guide=FALSE)+
      ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 2.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
      theme_void()+
      theme(
        plot.title = element_text(color="black", size=16, face="bold.italic"))+
      guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
  }
  
  
  return(list(UGA_subcounties_tidy, UGA_districts_tidy, plot1, scnames, UGA_subcounties_tidy_subset, UGA_districts_tidy_subset))
} 


#===========================================================================================================================================#
#=========================================================================================================#
#  identifying districts where PCC surveys have been conducted (only district-level data available)       #
#=========================================================================================================#



districts_name_func3 <- function(shape_file, year){
  
  if(year == "2002-2005"){
    
    UGA_SC_PCCstudies_names <- data.frame(District_name = shape_file@data$DNAME_2006) 
    
    UGA_SC_PCCstudies_names <- with(UGA_SC_PCCstudies_names,  UGA_SC_PCCstudies_names[order(District_name) , ])
    
  }
  
  if(year == "2006-2011"){
    
    UGA_SC_PCCstudies_names <- data.frame(District_name = shape_file@data$DNAME_2010) 
    
    UGA_SC_PCCstudies_names <- with(UGA_SC_PCCstudies_names,  UGA_SC_PCCstudies_names[order(District_name) , ])
    
  }
  
  if(year == "2012-2015" || year == "2016-2020"){
    
    UGA_SC_PCCstudies_names <- data.frame(District_name = shape_file@data$District)
    
    UGA_SC_PCCstudies_names <- with(UGA_SC_PCCstudies_names,  UGA_SC_PCCstudies_names[order(District_name) , ])
    
    
  }
  
  return(UGA_SC_PCCstudies_names)
  
}





district_PCCstudies_processing_plotting_func <- function(dist_names, district_2001, national_map_input, PCC_survey_years){
  
  # =============================================================#
  #  Extract sub-county locations where PCC studies have occured #
  
  
  # if (PCC_survey_years == "2022-2005"){
  #   
  #   PCCstudy_districts <-
  #     c("LIRA","KAMULI") 
  #   
  #   dist_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
  #   
  #   dist_names$PCC_survey <- ifelse(dist_names$Dist_name %in% PCCstudy_districts, "PCC survey","none") # code whether MDA or not
  #   
  #   dist_names <- dist_names  %>% rename(DNAME_2006 = Dist_name) # rename column
  #   
  #   # ============================================================ #
  #   # make MDA yes or no variable for sub counties with PCC survey #
  #   UGA_subcounties_tidy <- left_join(UGA_subcounties_tidy, dist_names) # join boundary data to MDA presence data
  #   
  #   UGA_subcounties_tidy$PCC_survey <- as.factor(UGA_subcounties_tidy$PCC_survey) # make MDA presence a factor
  #   
  #   PCC_survey.dist.col <- c("purple2", NA) # to colour MDA districts
  #   
  #   PCC_survey.dist.vec <- PCC_survey.dist.col[UGA_subcounties_tidy$PCC_survey] # specify colour for each polygon
  #   
  #   UGA_subcounties_tidy$PCC_survey_colour <- PCC_survey.dist.vec # new column for fill in ggplot depending on MDA
  #   
  #   UGA_subcounties_tidy$label <- ifelse(UGA_subcounties_tidy$PCC_survey == "PCC survey", 
  #                                        UGA_subcounties_tidy$DNAME_2006, NA)
  # }
  # 
  # #================================================================#
  # #  For PCC study locations >2012 (using a different shape file)  #
  # 
  # if (PCC_survey_year == 2013){
  #   
  #   PCCstudy_subcounties <-
  #     c("BUTANSI", "KITAYUNJWA", "NAMWENDWA", "BUGULUMBYA", "KABONERA", "KATWE-BUTEGO DIVISION",
  #       "KIMAANYA-KYABAKUZA DIVISION", "KKINGO", "KYANAMUKAAKA", "NYENDO-SSENYANGE DIVISION",
  #       "KYAMPISI", "NTENJERU") # there is no Mukono TC sub-county
  #   
  #   sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
  #   
  #   sc_names$PCC_survey <- ifelse(sc_names$Subcounty_name %in% PCCstudy_subcounties, "PCC survey","none") # code whether MDA or not
  #   
  #   sc_names <- sc_names  %>% rename(Subcounty = Subcounty_name, District = District_name) # rename column
  #   
  # }
  # 
  # 
  # if (PCC_survey_year == 2015){
  #   
  #   PCCstudy_subcounties <-
  #     c("OJWINA DIVISION", "BARR", "LIRA", "ADYEL DIVISION", "ADEKOKWOK", "MOYO", "METU") 
  #   
  #   sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
  #   
  #   sc_names$PCC_survey <- ifelse(sc_names$Subcounty_name %in% PCCstudy_subcounties, "PCC survey","none") # code whether MDA or not
  #   
  #   sc_names <- sc_names  %>% rename(Subcounty = Subcounty_name, District = District_name) # rename column
  #   
  # }
  # 
  # if (PCC_survey_year == 2019){
  #   
  #   PCCstudy_subcounties <-
  #     c("PAJULU","LAMOGI","UNYAMA") 
  #   
  #   sc_names # copy variable (dist names) : UGA_dist_MDA_names <- district_names 
  #   
  #   sc_names$PCC_survey <- ifelse(sc_names$Subcounty_name %in% PCCstudy_subcounties, "PCC survey","none") # code whether MDA or not
  #   
  #   sc_names <- sc_names  %>% rename(Subcounty = Subcounty_name, District = District_name) # rename column
  #   
  # }
  # 
  # if (PCC_survey_year == 2013 || PCC_survey_year == 2015 || PCC_survey_year == 2019){
  #   # ==================================================== #
  #   # make MDA yes or no variable for sub counties with MDA
  #   UGA_subcounties_tidy <- left_join(UGA_subcounties_tidy, sc_names) # join boundary data to MDA presence data
  #   
  #   UGA_subcounties_tidy$PCC_survey <- as.factor(UGA_subcounties_tidy$PCC_survey) # make MDA presence a factor
  #   
  #   PCC_survey.SC.col <- c("purple2", NA) # to colour MDA districts
  #   
  #   PCC_survey.SC.vec <- PCC_survey.SC.col[UGA_subcounties_tidy$PCC_survey] # specify colour for each polygon
  #   
  #   UGA_subcounties_tidy$PCC_survey_colour <- PCC_survey.SC.vec # new column for fill in ggplot depending on MDA
  #   
  #   UGA_subcounties_tidy$label <- ifelse(UGA_subcounties_tidy$PCC_survey == "PCC survey", 
  #                                        UGA_subcounties_tidy$Subcounty, NA)
  # }
  # 
  # 
  # ===========================================================================================================#
  #  Extract district MDAs (valid for all MDAs across 2033-2019 when analysing by original districts of 2003)  #
  
  district_map_0319 <- UGA_district_boundaries_function(shape_file = districts_2001, national_map_input = national_map) 
  
  district_names_0319 <- district_name_func(shape_file = districts_2001) # using original districts throughout 2003-2019
  
  UGA_dist_PCCsurvey_names <- district_names_0319 # copy variable (dist names)
  
  # if (year == 2004){
  #   # repeat for districts to highlight & check sub-counties (for each district) for 2004#
  #   MDA_districts <-
  #     c("APAC", "MOYO", "ADJUMANI", "ARUA", "NEBBI", "LIRA", "NAKASONGOLA", "MASINDI", "HOIMA", "BUGIRI",
  #       "BUSIA", "KAYUNGA", "JINJA", "MUKONO", "WAKISO", "MAYUGE", "BUNDIBUGYO", "KIBAALE") # vector of districts with MDA in 2003
  # } 
  
  if (PCC_survey_years == "2002-2005"){
    
    PCC_survey_districts <-
      c("LIRA", "KAMULI") # vector of districts with MDA in 2003
  } 
  
  # if (PCC_survey_year == 2013){
  #   
  #   PCC_survey_districts <-
  #     c("LWENGO", "MUKONO", "MASAKA", "KAMULI")
  # } 
  # 
  # if (PCC_survey_year == 2015){
  #   
  #   PCC_survey_districts <-
  #     c("LIRA", "MOYO")
  # } 
  # 
  # if (PCC_survey_year == 2019){
  #   
  #   PCC_survey_districts <-
  #     c("ARUA","GULU", "AMURU")
  # } 
  # 
  UGA_dist_PCCsurvey_names$PCC_survey <- ifelse(district_names_0319$Dist_name %in% PCC_survey_districts, "PCC survey","none") # code whether MDA or not
  
  UGA_dist_PCCsurvey_names <- UGA_dist_PCCsurvey_names  %>% rename(DISTRICT = Dist_name) # rename column
  
  UGA_districts_tidy <- left_join(district_map_0319[[2]], UGA_dist_PCCsurvey_names) # join boundary data to MDA presence data
  
  UGA_districts_tidy$PCC_survey <- as.factor(UGA_districts_tidy$PCC_survey) # make MDA presence a factor
  
  PCC_survey.dist.col <- c("blue",NA) # to colour MDA district
  
  PCC_survey.dist.vec <- PCC_survey.dist.col[UGA_districts_tidy$PCC_survey] # specify colour for each polygon
  
  UGA_districts_tidy$PCC_survey_colour <- PCC_survey.dist.vec # new column for fill in ggplot depending on MDA
  
  #======================= 
  # make labels for plot
  
  # if (year == 2004){
  #   # make labels (sub-counties with MDA) for plotting
  #   UGA_subcounties_tidy_subset <- subset(UGA_subcounties_tidy, MDA=="MDA")   #subset just for NYS
  #   scnames <- aggregate(cbind(long, lat) ~ SNAME_2006, data=UGA_subcounties_tidy_subset, FUN=mean)
  #   scnames$label <- scnames$SNAME_2006
  # }
  
  if (PCC_survey_years == "2002-2005"){
    # make labels (sub-counties with MDA) for plotting
    UGA_districts_tidy_subset <- subset(UGA_districts_tidy, PCC_survey=="PCC survey")   #subset just for NYS
    distnames <- aggregate(cbind(long, lat) ~ DISTRICT, data=UGA_districts_tidy_subset, FUN=mean)
    distnames$label <- distnames$DISTRICT
  }
  
  # if (PCC_survey_year == 2011){
  #   # make labels (sub-counties with MDA) for plotting
  #   UGA_subcounties_tidy_subset <- subset(UGA_subcounties_tidy, PCC_survey=="PCC survey")   #subset just for NYS
  #   scnames <- aggregate(cbind(long, lat) ~ SNAME_2010, data=UGA_subcounties_tidy_subset, FUN=mean)
  #   scnames$label <- scnames$SNAME_2010
  # }
  # 
  # if (PCC_survey_year == 2013 || PCC_survey_year == 2015 || PCC_survey_year == 2019){
  #   # make labels (sub-counties with MDA) for plotting
  #   UGA_subcounties_tidy_subset <- subset(UGA_subcounties_tidy, PCC_survey=="PCC survey")   #subset just for NYS
  #   scnames <- aggregate(cbind(long, lat) ~ Subcounty, data=UGA_subcounties_tidy_subset, FUN=mean)
  #   scnames$label <- scnames$Subcounty
  # }
  
  # PLOT
  
  # if (year == 2004){
  #   UGA_districts_tidy_subset <- subset(UGA_districts_tidy, MDA=="MDA")
  #   
  #   plot1 <- ggplot() +
  #     geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
  #     geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
  #     geom_polygon(data= UGA_districts_tidy_subset, aes(x = long, y = lat, group = group, colour= MDA_colour), fill= "purple", size = 1, alpha=0.25)+
  #     geom_polygon(data= UGA_subcounties_tidy, aes(x = long, y = lat, group = group, colour= MDA_colour), size = 0.75, fill=NA, alpha=NA)+
  #     coord_equal(ratio = 1)+
  #     scale_colour_manual(values=c("blue","purple2",NA), guide=FALSE)+
  #     ggrepel::geom_text_repel(data = scnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 4.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
  #     theme_void()+
  #     theme(
  #       plot.title = element_text(color="black", size=16, face="bold.italic"))+
  #     guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
  # }
  
  if (PCC_survey_years == "2002-2005"){
    
    #UGA_districts_tidy_subset <- subset(UGA_districts_tidy, PCC_survey=="PCC survey")   # subset MDA districts to plot
    
    plot1 <- ggplot() +
      geom_polygon(data = national_map, aes(x=long, y = lat, group = group), color = "black", size = 0.1, fill = "lightgrey") +
      geom_polygon(data = districts_2001, aes(x = long, y = lat, group = group), colour = "black", alpha = 1, fill = NA)+
      geom_polygon(data= UGA_districts_tidy_subset, aes(x = long, y = lat, group = group, colour= PCC_survey_colour), fill= "orange", size = 1, alpha=0.25)+
      #geom_polygon(data= UGA_subcounties_tidy_subset, aes(x = long, y = lat, group = group, colour= PCC_survey), size = 0.75, fill=NA, alpha=NA)+
      coord_equal(ratio = 1)+
      scale_colour_manual(values=c("blue","purple2",NA), guide=FALSE)+
      ggrepel::geom_text_repel(data = distnames, aes(long, lat, label = label), box.padding = 1.15, max.overlaps = Inf, size = 2.5, family = 'Avenir', segment.color = "#333333", fontface = "bold")+
      theme_void()+
      theme(
        plot.title = element_text(color="black", size=16, face="bold.italic"))+
      guides(fill=guide_legend(override.aes=list(shape=21, size=3, colour="black", stroke=1.2))) # need this to get colour in the fill (sample.size) legend
  }
  
  
  return(list(UGA_districts_tidy, plot1, distnames, UGA_districts_tidy_subset))
} 
