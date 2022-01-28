#====================================================================================================================#
#===================================              PLOTS TO SAVE            ==========================================#
#====================================================================================================================#



# coverage maps plotting into a single PDF # 
print_PDF_Cov_maps <- function(map1, map2, map3, map4, map5, map6, map7, map8, map9, map10, map11, map12, map13, map14, map15,
                               map16, map17){
  
  multi.page <- ggarrange(map1, map2, map3, map4, map5, map6, map7, map8, map9, map10, map11, map12, map13, map14, map15,
                          map16, map17, common.legend = TRUE, legend = "bottom", nrow=3, ncol=1) # for one plot per page - need ggarrange package
  
  multi.page[[1]] # for seeing the first plot
  
  # multi.page.caption <- annotate_figure(multi.page,
  #                                       bottom = text_grob("Dark grey values > 100%", color = "grey",
  #                                                          hjust = 1, x = 1, face = "italic", size = 10),
  #                                       fig.lab = "Uganda MDA coverage maps (all denominators)", fig.lab.face = "bold")
  # #https://rpkgs.datanovia.com/ggpubr/reference/annotate_figure.html 
  
  ggexport(multi.page, filename="Coverage_maps_plot.pdf") # need ggpubr package

} 

# coverage maps (only 1 denominator) plotting into a single PDF # 
print_PDF_Cov_maps2 <- function(map1, map2, map3, map4, map5, map6, map7, map8, map9, map10, map11, map12, map13, map14, map15,
                               map16, map17){
  
  multi.page <- ggarrange(map1, map2, map3, map4, map5, map6, map7, map8, map9, map10, map11, map12, map13, map14, map15,
                          map16, map17, common.legend = TRUE, legend = "bottom", nrow=3, ncol=3, labels = "Dark grey values > 100%",
                          font.label = list(size = 8, face = "plain", color = "grey60")) # for one plot per page - need ggarrange package
  
  multi.page[[1]] # for seeing the first plot
  
  ggexport(multi.page, filename="Coverage_maps_plot2.pdf") # need ggpubr package
  
  #ggexport(multi.page, filename="Coverage_maps_plot2.tiff", res = 300)
  
  } 


# TO DO: include maps from 2010 or make new function?


# boxplot plotting into a single PDF # 
print_PDF_Cov_boxplots <- function(boxplt1, boxplt2, boxplt3, boxplt4){
  
  multi.page <- ggarrange(boxplt1, boxplt2, boxplt3, boxplt4, nrow=2, ncol=1) # for one plot per page - need ggarrange package
  
  multi.page[[1]] # for seeing the first plot
  
  ggexport(multi.page, filename="Coverage_boxplots_plot.pdf") # need ggpubr package
  
} 


# TO DO: include boxplots with new years, or is this already factored in?