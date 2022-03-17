

Processing_plotting_UGA_MDA_TSdata_func <- function(data){

  data <- UGA_MDA_PCCprev_data  
# ================================#
# data clean /adjust for plotting #
  
  UGA_MDA_PCCprev_data$Type <- as.factor(UGA_MDA_PCCprev_data$Type)
  
  UGA_MDA_PCCprev_data$District <- as.factor(UGA_MDA_PCCprev_data$District)
  
  UGA_MDA_PCCprev_data$Upper <- as.numeric(UGA_MDA_PCCprev_data$Upper)
  
  UGA_MDA_PCCprev_data$Type <- factor(UGA_MDA_PCCprev_data$Type, levels = c("Informed", "Apparent"))

# =================================#
#    plot TS data x district & MDA #

  UGA_MDA_PCCprev_data$First_MDA_year
  
  p <- ggplot(data = UGA_MDA_PCCprev_data, aes(group = Type))+
  #geom_rect(subset(UGA_MDA_PCCprev_data, District="Apac"), aes(x = NULL, y=NULL, xmin=2000, xmax=2003, ymin=0, ymax=100), alpha=0.1, fill = "blue")+
  geom_point(aes(x = Year_offset, y = Prevalence))+
  geom_errorbar(aes(x = Year_offset, y = Prevalence, linetype = Type, ymin= Lower, ymax = Upper), 
                width= 1.25)+
  scale_x_continuous(limits=c(2000,2020))+
  scale_y_continuous(limits=c(0,100))+
  facet_wrap(~District, scales = "free")+
  #annotate("rect", xmin=2000, xmax=2003, ymin=0, ymax=100, fill="blue", alpha = .1)+
  theme_bw()+
  theme(axis.line=element_line())+
  labs(linetype='')+
  xlab("Year") + 
  ylab("Porcine cysticercosis prevalence")


# make pre-MDA (endemic) rectangles on TS PCC prevalence data by district plots

p1 <- p + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Apac", Type = NA), 
                    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p2 <- p1 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Arua", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p3 <- p2 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Busia", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p4 <- p3 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Kaberamaido", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p5 <- p4 + geom_rect(data = data.frame(xmin=2000, xmax=2020, ymin=0, ymax=100, District = "Kampala", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p6 <- p5 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Kamuli", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p7 <- p6 + geom_rect(data = data.frame(xmin=2000, xmax=2013, ymin=0, ymax=100, District = "Katakwi", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p8 <- p7 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Kayunga", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p9 <- p8 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Kibaale", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p10 <- p9 + geom_rect(data = data.frame(xmin=2000, xmax=2013, ymin=0, ymax=100, District = "Kumi", Type = NA), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p11 <- p10 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Lira", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p12 <- p11 + geom_rect(data = data.frame(xmin=2000, xmax=2020, ymin=0, ymax=100, District = "Luwero", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p13 <- p12 + geom_rect(data = data.frame(xmin=2000, xmax=2007, ymin=0, ymax=100, District = "Masaka", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p14 <- p13 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Moyo", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p15 <- p14 + geom_rect(data = data.frame(xmin=2000, xmax=2007, ymin=0, ymax=100, District = "Mpigi", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p16 <- p15 + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, District = "Mubende", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p17 <- p16 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Mukono", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p18 <- p17 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Nakasongola", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p18 <- p17 + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, District = "Pallisa", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p19 <- p18 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Soroti", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

pFinal <- p19 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Wakiso", Type = NA), 
                          aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

pFinal

return(list(pFinal, UGA_MDA_PCCprev_data))

}