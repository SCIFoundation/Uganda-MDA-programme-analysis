#====================================================================================================================#
#===================================              PLOTS TO SAVE            ==========================================#
#====================================================================================================================#




print_PDF_Cov_maps <- function(map1, map2, map3, map4){
  
  multi.page <- ggarrange(map1, map2, map3, map4, nrow=1, ncol=1) # for one plot per page - need ggarrange package
  
  multi.page[[1]] # for seeing the first plot
  
  ggexport(multi.page, filename="Coverage_maps_plot.pdf") # need ggpubr package

} 






