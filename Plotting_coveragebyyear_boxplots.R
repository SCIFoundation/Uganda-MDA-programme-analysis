#===================================================================================================================#
#=====================================    Coverage by year (boxxplots)    ==========================================#

#==========================================================================================================#
# function to create dataframe suitable for plotting box plots (with district, MDA coverage, year columns) #


create_dataframe_plotting_coverageboxplots_func <- function(cov_type,
                                                            coverage_dataframe1, coverage_dataframe2,
                                                            coverage_dataframe3, coverage_dataframe4){
  if(cov_type =="cov1"){
    coverage_dataframe = coverage_dataframe1
  }
  if(cov_type == "cov2"){
    coverage_dataframe = coverage_dataframe2
  }
  if(cov_type == "cov3"){
    coverage_dataframe = coverage_dataframe3
  }
  if(cov_type == "cov4"){
    coverage_dataframe = coverage_dataframe4
  }
  
  data03_cov <- coverage_dataframe[ , c("District", "Region", "Cov_2003")]
  data03_cov$year <- as.factor("2003")
  
  data04_cov <- coverage_dataframe[ , c("District", "Region", "Cov_2004")]
  data04_cov$year <- as.factor("2004")
  
  data05_cov <- coverage_dataframe[ , c("District", "Region", "Cov_2005")]
  data05_cov$year <- as.factor("2005")
  
  data06_cov <- coverage_dataframe[ , c("District", "Region", "Cov_2006")]
  data06_cov$year <- as.factor("2006")
  
  data07_cov <- coverage_dataframe[ , c("District", "Region", "Cov_2007")]
  data07_cov$year <- as.factor("2007")
  
  data08_cov <- coverage_dataframe[ , c("District", "Region", "Cov_2008")]
  data08_cov$year <- as.factor("2008")
  
  data09_cov <- coverage_dataframe[ , c("District", "Region", "Cov_2009")]
  data09_cov$year <- as.factor("2009")
  
  df_list <- list(data03_cov, data04_cov, data04_cov, data05_cov,
                data06_cov, data07_cov, data08_cov, data09_cov)
  
  colnames <- c("District", "Region", "Cov", "year")
  
  df_list <- lapply(df_list, setNames, colnames)
  
  master_dataframe_cov <- bind_rows(df_list, .id = "District_ID") # unlist dataframes and row bind
  
  master_dataframe_cov$District <- as.factor(master_dataframe_cov$District)
  
  return(master_dataframe_cov)
  
}

# TO DO: extend this to 2020; or produce split maps

#================================#
#     Plot boxplots function     #



plot_boxplot_MDAcov_func <- function(cov_type, master_dataframe_cov){

master_dataframe_cov[["cov_indicator"]] = ifelse(master_dataframe_cov[["Cov"]] > 100, "red", "black") # filter MDA cov points and assign colour (< or > 100%)


if(cov_type =="cov1"){
  title <- c("coverage = total doses / total targeted")
}
if(cov_type =="cov2"){
  title <- c("coverage = total doses / district pop ;  using constant pop growth across years")
}
if(cov_type =="cov3"){
  title <- c("coverage = total doses / district pop ;  using constant pop growth across years 
& SCIF numbers")
}
if(cov_type =="cov4"){
  title <- c("coverage = total doses / max target pop")
}

boxplot_cov <- ggplot(master_dataframe_cov, aes(x=year, y=Cov)) +
  geom_boxplot(aes(fill=year), outlier.shape = NA) +
  geom_jitter(aes(colour=cov_indicator), shape=16, position=position_jitter(0.15), show.legend = FALSE)+
  scale_colour_manual(values = c("red" = "red", "black" = "black"))+
  scale_fill_brewer(name = "MDA year", palette="Dark2")+
  labs(title = title, x="Year", y = "Coverage (%)", caption ="red datapoints >100% coverage")+
  theme_classic()+
  theme(axis.title.x = element_text(size=11, face="bold", colour = "black"),    
        axis.title.y = element_text(size=11, face="bold", colour = "black"),
        plot.caption = element_text(face = "italic", size = 10))

return(boxplot_cov)

}
