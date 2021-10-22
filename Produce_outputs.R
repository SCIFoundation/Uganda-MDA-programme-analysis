#====================================================================================================================#
#===================================              PLOTS TO SAVE            ==========================================#
#====================================================================================================================#


print_PDF_Cov_maps <- function(map1, map2, map3, map4){
  
  PDFPath = "~/Uganda-MDA-programme-analysis/"
  
  #plots.list = list(map1, map2, map3, map4)  # Make a list of plots
  
  # Generate plots to be saved to pdf, warning the argument to marrangeGrob
  # have to be passed using do.call
  # nrow (ncol) gives the number of rows (columns) of plots per page
  # nrow and ncol have to be specificed inside a list
  # Here, we'll obtain 2 plots in rows by page
  #plots = do.call(marrangeGrob, c(plots.list, list(nrow = 2, ncol = 1)))
  
  MapPlots = list(map1, map2, map3, map4)
  
  # To save to file, here on A4 paper
  ggsave(path = PDFPath, filename = "Coverage_maps_plot.pdf", MapPlots, width = 21, height = 29.7, units = "cm")

} 


