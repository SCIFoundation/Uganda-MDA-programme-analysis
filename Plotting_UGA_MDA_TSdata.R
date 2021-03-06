

Processing_plotting_UGA_MDA_TSdata_func <- function(data){

  data <- UGA_MDA_PCCprev_data  
# ================================#
# data clean /adjust for plotting #
  
  UGA_MDA_PCCprev_data$Type <- as.factor(UGA_MDA_PCCprev_data$Type)
  
  UGA_MDA_PCCprev_data$District <- as.factor(UGA_MDA_PCCprev_data$District)
  
  UGA_MDA_PCCprev_data$Upper <- as.numeric(UGA_MDA_PCCprev_data$Upper)
  
  UGA_MDA_PCCprev_data$Type <- factor(UGA_MDA_PCCprev_data$Type, levels = c("Informed", "Apparent", "Apparent (<5 sampled)"))

# =================================#
#    plot TS data x district & MDA #

  UGA_MDA_PCCprev_data$First_MDA_year
  
  p <- ggplot(data = UGA_MDA_PCCprev_data, aes(group = Type))+
  #geom_rect(subset(UGA_MDA_PCCprev_data, District="Apac"), aes(x = NULL, y=NULL, xmin=2000, xmax=2003, ymin=0, ymax=100), alpha=0.1, fill = "blue")+
  geom_point(aes(x = Year_offset, y = Prevalence, colour = Type, shape = SC_risk))+
  geom_errorbar(aes(x = Year_offset, y = Prevalence, linetype = Type, ymin= Lower, ymax = Upper, colour = Type), 
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

p1a <- p + geom_rect(data = data.frame(xmin=2000, xmax=2010, ymin=0, ymax=100, District = "Amolator (Lira)", Type = NA), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")
  
p1b <- p1a + geom_rect(data = data.frame(xmin=2000, xmax=2020, ymin=0, ymax=100, District = "Amuria (Katakwi)", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p1c <- p1b + geom_rect(data = data.frame(xmin=2000, xmax=2010, ymin=0, ymax=100, District = "Amuru (Gulu)", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")
  
p1 <- p1c + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Apac", Type = NA), 
                    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p2 <- p1 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Arua", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p2a <- p2 + geom_rect(data = data.frame(xmin=2000, xmax=2015, ymin=0, ymax=100, District = "Bukedea (Kumi)", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p3 <- p2a + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Busia", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p3a <- p3 + geom_rect(data = data.frame(xmin=2000, xmax=2020, ymin=0, ymax=100, District = "Butambala (Mpigi)", Type = NA), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p3b <- p3a + geom_rect(data = data.frame(xmin=2000, xmax=2007, ymin=0, ymax=100, District = "Gomba (Mpigi)", Type = NA), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p3c <- p3b + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, District = "Gulu", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p4 <- p3c + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Kaberamaido", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p5 <- p4 + geom_rect(data = data.frame(xmin=2000, xmax=2020, ymin=0, ymax=100, District = "Kampala", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p6 <- p5 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Kamuli", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p7 <- p6 + geom_rect(data = data.frame(xmin=2000, xmax=2013, ymin=0, ymax=100, District = "Katakwi", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p8 <- p7 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Kayunga", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p9 <- p8 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Kibale", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p10 <- p9 + geom_rect(data = data.frame(xmin=2000, xmax=2013, ymin=0, ymax=100, District = "Kumi", Type = NA), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p11 <- p10 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Lira", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p12 <- p11 + geom_rect(data = data.frame(xmin=2000, xmax=2020, ymin=0, ymax=100, District = "Lwengo (Masaka)", Type = NA), 
                        aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p13 <- p12 + geom_rect(data = data.frame(xmin=2000, xmax=2007, ymin=0, ymax=100, District = "Masaka", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p13a <- p13 + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, District = "Mityana", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p14 <- p13a + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Moyo", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p15 <- p14 + geom_rect(data = data.frame(xmin=2000, xmax=2007, ymin=0, ymax=100, District = "Mpigi", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

# p16 <- p15 + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, District = "Mubende", Type = NA), 
#                        aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p17 <- p15 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Mukono", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p17a <- p17 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Nakaseke (Luwero)", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p18 <- p17a + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Nakasongola", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p19 <- p18 + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, District = "Pallisa", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p20 <- p19 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, District = "Soroti", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

pFinal <- p20 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, District = "Wakiso", Type = NA), 
                          aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

pFinal

# plot PCC data by original districts 
p_old <- ggplot(data = UGA_MDA_PCCprev_data, aes(group = Type))+
  #geom_rect(subset(UGA_MDA_PCCprev_data, District="Apac"), aes(x = NULL, y=NULL, xmin=2000, xmax=2003, ymin=0, ymax=100), alpha=0.1, fill = "blue")+
  geom_point(aes(x = Year_offset, y = Prevalence, colour = District, shape = SC_risk))+
  geom_errorbar(aes(x = Year_offset, y = Prevalence, linetype = Type, ymin= Lower, ymax = Upper, colour = District), 
                width= 1.25)+
  scale_x_continuous(limits=c(2000,2020))+
  scale_y_continuous(limits=c(0,100))+
  facet_wrap(~Old_districts, scales = "free")+
  #annotate("rect", xmin=2000, xmax=2003, ymin=0, ymax=100, fill="blue", alpha = .1)+
  theme_bw()+
  theme(axis.line=element_line())+
  labs(linetype='')+
  xlab("Year") + 
  ylab("Porcine cysticercosis prevalence")


p_old1 <- p_old + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, Old_districts = "Apac", Type = NA), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old2 <- p_old1 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, Old_districts = "Arua", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old3 <- p_old2 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, Old_districts = "Busia", Type = NA), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old4 <- p_old3 + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, Old_districts = "Gulu", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old4a <- p_old4 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, Old_districts = "Kaberamaido", Type = NA), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old5 <- p_old4a + geom_rect(data = data.frame(xmin=2000, xmax=2020, ymin=0, ymax=100, Old_districts = "Kampala", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old6 <- p_old5 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, Old_districts = "Kamuli", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old7 <- p_old6 + geom_rect(data = data.frame(xmin=2000, xmax=2013, ymin=0, ymax=100, Old_districts = "Katakwi", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old8 <- p_old7 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, Old_districts = "Kayunga", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old9 <- p_old8 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, Old_districts = "Kibale", Type = NA), 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old10 <- p_old9 + geom_rect(data = data.frame(xmin=2000, xmax=2013, ymin=0, ymax=100, Old_districts = "Kumi", Type = NA), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old11 <- p_old10 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, Old_districts = "Lira", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old11a <- p_old11 + geom_rect(data = data.frame(xmin=2000, xmax=2020, ymin=0, ymax=100, Old_districts = "Luwero", Type = NA), 
                               aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old12 <- p_old11a + geom_rect(data = data.frame(xmin=2000, xmax=2007, ymin=0, ymax=100, Old_districts = "Masaka", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old13 <- p_old12 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, Old_districts = "Moyo", Type = NA), 
                        aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old14 <- p_old13 + geom_rect(data = data.frame(xmin=2000, xmax=2007, ymin=0, ymax=100, Old_districts = "Mpigi", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old15 <- p_old14 + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, Old_districts = "Mubende", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old16 <- p_old15 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, Old_districts = "Mukono", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old17 <- p_old16 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, Old_districts = "Nakasongola", Type = NA), 
                        aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old18 <- p_old17 + geom_rect(data = data.frame(xmin=2000, xmax=2009, ymin=0, ymax=100, Old_districts = "Pallisa", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_old19 <- p_old18 + geom_rect(data = data.frame(xmin=2000, xmax=2005, ymin=0, ymax=100, Old_districts = "Soroti", Type = NA), 
                       aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

p_oldFinal <- p_old19 + geom_rect(data = data.frame(xmin=2000, xmax=2003, ymin=0, ymax=100, Old_districts = "Wakiso", Type = NA), 
                          aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, group = Type), alpha=0.1, fill = "blue")

#p_oldFinal

return(list(pFinal, p_old, p_oldFinal, UGA_MDA_PCCprev_data))

}