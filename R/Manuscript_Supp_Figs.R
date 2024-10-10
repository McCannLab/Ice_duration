library(nsRFA)
library(dplyr)
library(cowplot)
library(gridExtra)
library(MuMIn)
library(olsrr)
library(car)
library(data.table)
library(ggplot2)
library(jtools)
library(sandwich)
library(tidyverse)
library(maps)
library(mapdata)
library(viridis)

###### MS Figure 1 #####

setwd("C:/Users/Connor/OneDrive/PhD Projects/ice duration/data")
lakes1 <- read.table("lakes_input_removingGAPS_NEW_zeros_redone1.csv", header = TRUE, sep = ",")
colnames(lakes1) <- c("lakecode", "lake", "year_ice_off", "ice_duration", "AirTEMP", "PPT","obs","IDTREND", "posneg", "IDTREND_SIG")

lakesx<-subset(lakes1, lakes1$obs>24)
lakesx1<-subset(lakes1, lakes1$obs>24 & lakes1$IDTREND_SIG<0.5)
lakesx2<-subset(lakes1, lakes1$obs>24 & lakes1$IDTREND_SIG<0.25)
lakesx3<-subset(lakes1, lakes1$obs>24 & lakes1$IDTREND_SIG<0.05)

lakesy <- mutate(lakesx, lake = factor(lake))
vecLakey <- unique(lakesy$lake) 
lakesy1 <- mutate(lakesx1, lake = factor(lake))
vecLakey1 <- unique(lakesy1$lake) 
lakesy2 <- mutate(lakesx2, lake = factor(lake))
vecLakey2 <- unique(lakesy2$lake) 
lakesy3 <- mutate(lakesx3, lake = factor(lake))
vecLakey3 <- unique(lakesy3$lake) 

plotx1<-ggplot(lakesy, aes(year_ice_off, ice_duration, group = lake, color = factor(posneg))) + geom_smooth(method = "lm", se=FALSE) + theme(legend.position="bottom") + labs(x="", y="") 
plotx1 <- plotx1 + theme(axis.title.x = element_blank(), axis.text.x=element_text(size=20)) + theme(axis.title.y = element_text(size=25), axis.text.y=element_text(size=20)) +  labs(colour = "", size=20) +  theme(legend.position = "none") +  scale_color_manual(values=c("red", "blue")) 
plotx2<-ggplot(lakesy1, aes(year_ice_off, ice_duration, group = lake, color = factor(posneg))) + geom_smooth(method = "lm", se=FALSE) + theme(legend.position="bottom") + labs(x="", y="") 
plotx2 <- plotx2 + theme(axis.title.x = element_blank(), axis.text.x=element_text(size=20)) + theme(axis.title.y = element_text(size=25), axis.text.y=element_text(size=20)) +  labs(colour = "", size=20) +  theme(legend.position = "none") +  scale_color_manual(values=c("red", "blue")) 
plotx3<-ggplot(lakesy2, aes(year_ice_off, ice_duration, group = lake, color = factor(posneg))) + geom_smooth(method = "lm", se=FALSE) + theme(legend.position="bottom") + labs(x="", y="") 
plotx3 <- plotx3 + theme(axis.title.x = element_blank(), axis.text.x=element_text(size=20)) + theme(axis.title.y = element_text(size=25), axis.text.y=element_text(size=20)) +  labs(colour = "", size=20) +  theme(legend.position = "none") +  scale_color_manual(values=c("red", "blue")) 
plotx4<-ggplot(lakesy3, aes(year_ice_off, ice_duration, group = lake, color = factor(posneg))) + geom_smooth(method = "lm", se=FALSE) + theme(legend.position="bottom") + labs(x="", y="") 
plotx4 <- plotx4 + theme(axis.title.x = element_text(size=25), axis.text.x=element_text(size=20)) + theme(axis.title.y = element_text(size=25), axis.text.y=element_text(size=20)) +  labs(colour = "Ice Duration Trend Direction", size=20) + theme(legend.text=element_text(size=20), legend.title=element_text(size=20)) +  scale_color_manual(values=c("red", "blue")) 

plot<-plot_grid(plotx1,plotx2,plotx3,plotx4, ncol=1, labels="auto", label_size=25, align='vh', hjust=-1, scale = 1)
y.grob <- textGrob("Lake Ice Duration (Days)", 
                   gp=gpar(col="black", fontsize=25), rot=90)
cplot<-grid.arrange(arrangeGrob(plot, left = y.grob))
ggsave("./Figure1.tiff", plot = cplot, units="in", width=10, height=20, dpi=500, device = "tiff")

###### MS Figure 2 #####
lakes <- read.table("model_input_chapter1.csv", header = TRUE, sep = ",")
colnames(lakes) <- c("lake", "lakename", "latitude", "Latitude", "longitude", "longstand", "Longitude", "longcont", "LONGRAW", "loglongcont", "elevation", "Elevation", "surface_area", "Surface_Area", "mean_depth", "Mean_Depth", "Air_Temperature", "AT_stand", "logAT", "Precipitation", "PPT_stand", "logPPT", "ID_slope", "posneg", "ID_slope_p", "obs", "TS_CAT", "CONT")
lakes <- lakes %>% filter(lake != "WRS279")
# just remove this from the .csv file if this is the case

lakes1<-subset(lakes, lakes$obs>24 & lakes$ID_slope_p<0.05)
lakes2<-subset(lakes, lakes$obs>24 & lakes$ID_slope_p<0.25)
lakes3<-subset(lakes, lakes$obs>24 & lakes$ID_slope_p<0.5)
lakes4<-subset(lakes, lakes$obs>24)

# run this for each dataset (lakes1-4 and the same predictor/response variables (ID_slope~logAT+logPPT+Elevation+Latitude+loglongcont+Mean_Depth + Surface_Area))
mod = lm(ID_slope~logAT+logPPT+Elevation+Latitude+loglongcont+Mean_Depth + Surface_Area, data=lakes1, na.action="na.fail") #run the model with all the predictor variables in it
vif(mod)
d = dredge(mod, extra="R^2", rank="AICc") #run all possible subset combinations of models with the predictors listed in mod and then ranked them by AICc and give the R2 of each model
d #creates table with all the models ranked, their AICc and R2 scores and parameter estimates
#select the model with a delta value <2 (best AICc or within 2 of it) and the highest R2 value to produce these multiple linear models:
fit1<-lm(ID_slope~logAT+Elevation+Latitude, data=lakes1)
fit2<-lm(ID_slope~logAT+Elevation+Latitude+loglongcont+Mean_Depth, data=lakes2)
fit3<-lm(ID_slope~logAT+Elevation+Latitude+loglongcont+Mean_Depth + Surface_Area, data=lakes3)
fit4<-lm(ID_slope~logAT+Elevation+Latitude+loglongcont+Mean_Depth, data=lakes4)

testplot<-plot_summs(fit1, fit2, fit3, fit4, scale = TRUE,  inner_ci_level = .9, model.names = c("DS 1 (221 Lakes)", "DS 2 (140 Lakes)", "DS 3 (97 Lakes)", "DS 4 (42 Lakes)"), legend.title = "Dataset", colors = "Rainbow", coefs = c("Air Temperature" = "logAT", "Precipitation" = "logPPT", "Latitude" = "Latitude", "Longitude" = "loglongcont", "Elevation" = "Elevation",  
                                                                                                                                                                                                                                        "Mean Depth" = "Mean_Depth", "Surface Area" = "Surface_Area"))
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Helvetica'),
        legend.title=element_text(size=12.5),
        axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        legend.text = element_text(size = 12.5), )
testplot <-testplot + apatheme + labs(x = "\n Beta Estimate \n ", y = "\n Predictor \n ")  
ggsave("./Figure2.tiff", plot = testplot, units="in", width=10, height=10, dpi=500, device = "tiff")

###### Figure S1 #####
lakes <- read.table("lakesnewtrends_mar28_oldtemp_newppt.csv", header = TRUE, sep = ",")
colnames(lakes) <- c("lake", "lakename", "latitude", "Latitude", "longitude", "longstand", "Longitude", "longcont", "LONGRAW", "loglongcont", "elevation", "Elevation", "surface_area", "Surface_Area", "mean_depth", "Mean_Depth", "Air_Temperature", "AT_stand", "logAT", "Precipitation", "PPT_stand", "logPPT", "ID_slope", "posneg", "ID_slope_p", "obs", "TS_CAT", "CONT")
lakes1 <- subset(lakes, lakes$obs>24)

world=map_data('world')

ggplot() + geom_polygon(data=world, aes(x=long, y=lat, group=group))
# Left: use size and color
lakemap<-ggplot() + coord_map("azequalarea") +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="black", alpha=0.5) +
  geom_point(data=lakes1, aes(x=longitude, y=latitude, size = surface_area, color=elevation)) +
  scale_size_continuous(breaks = c(30,3000,30000)) +
  scale_color_viridis(breaks =c(0, 15, 150, 1500), trans="log") +
  theme_void() +  labs(colour = "Elevation (m)", size = "Surface Area (km2)") +
  theme(legend.position = c(1,0.75),
        legend.background = element_rect(color=NA, size=0.5))
ggsave("./FigureS1.tiff", plot = lakemap, units="in", width=10, height=10, dpi=500, device = "tiff")

###### Figure S2 #####
lakes <- read.table("lakesnewtrends_mar28_oldtemp_newppt.csv", header = TRUE, sep = ",")
colnames(lakes) <- c("lake", "lakename", "latitude", "Latitude", "longitude", "longstand", "Longitude", "longcont", "LONGRAW", "loglongcont", "elevation", "Elevation", "surface_area", "Surface_Area", "mean_depth", "Mean_Depth", "Air_Temperature", "AT_stand", "logAT", "Precipitation", "PPT_stand", "logPPT", "ID_slope", "posneg", "ID_slope_p", "obs", "TS_CAT", "CONT")
lakes3<-subset(lakes, lakes$obs>24)

p1<-ggplot(lakes3, aes(x=obs, y=Elevation, color=ID_slope)) + 
  geom_point(size=3)
p11<-p1 + scale_size(range = c(-3, 3)) + scale_colour_viridis() + labs(colour = "I.D. Trend") + 
  ylab("Lake Elevation") + xlab("")  + ylim(0,4) + theme(axis.title.y = element_text(size = 10))
p2<-ggplot(lakes3, aes(x=obs, y=Mean_Depth, color=ID_slope)) + 
  geom_point(size=3)
p22<-p2 + scale_size(range = c(-3, 3)) + scale_colour_viridis() + labs(colour = "I.D. Trend") + 
  ylab("Lake Mean Depth") + xlab("")  + ylim(0,3.5) + theme(axis.title.y = element_text(size = 10))
p3<-ggplot(lakes3, aes(x=obs, y=Longitude, color=ID_slope)) + 
  geom_point(size=3)
p33<-p3 + scale_size(range = c(-3, 3)) + scale_colour_viridis() + labs(colour = "I.D. Trend") + 
  ylab("Lake Longitude") + xlab("")  + ylim(0,3.5) + theme(axis.title.y = element_text(size = 10))
p4<-ggplot(lakes3, aes(x=obs, y=Air_Temperature, color=ID_slope)) + 
  geom_point(size=3)
p44<-p4 + scale_size(range = c(-3, 3)) + scale_colour_viridis() + labs(colour = "I.D. Trend") + 
  ylab("Lake Air Temperature Trend") + xlab("")  + ylim(-0.1,0.1) + theme(axis.title.y = element_text(size = 10))
p8<-ggplot(lakes3, aes(x=obs, y=Latitude, color=ID_slope)) + 
  geom_point(size=3)
p88<-p8 + scale_size(range = c(-3, 3)) + scale_colour_viridis() + labs(colour = "I.D. Trend") + 
  ylab("Lake Latitude") + xlab("")  + ylim(1.55,1.9) + theme(axis.title.y = element_text(size = 10))

#for making p55 and p66
lakes <- read.table("lakes_summarydata_julyanalyses2019.csv", header = TRUE, sep = ",")
summary (lakes) 
colnames(lakes) <- c("lake", "lakename", "latitude", "Latitude", "longitude", "longstandard", "Longitude", "elevation", "Elevation", "surface_area", "Surface_Area", "mean_depth", "Mean_Depth", "Air_Temperature", "Precipitation", "ID_slope", "ID_slope_p", "obs","IDVAR")

lakes3<-subset(lakes, lakes$obs>24)

p5<-ggplot(lakes3, aes(x=obs, y=ID_slope, color=ID_slope)) + 
  geom_point(size=3)
p55<-p5 + scale_size(range = c(-3, 3)) + scale_colour_viridis_b() + labs(colour = "I.D. Trend") + 
  ylab("Lake Ice Duration Trend") + xlab("") + ylim(-1.5,1.5) + theme(axis.title.y = element_text(size = 10))
p6<-ggplot(lakes3, aes(x=obs, y=IDVAR, color=IDVAR)) + 
  geom_point(size=3)
p66<-p6 + scale_size(range = c(0,1000)) + scale_colour_viridis_b() + labs(colour = "I.D. Variance") + 
  ylab("Lake Ice Duration Variance") + xlab("") + ylim(0,1000) + theme(axis.title.y = element_text(size = 10))

plot<-plot_grid(p11,p22,p33,p44,p88,p55,p66, ncol=2, labels="auto", hjust=0, vjust=1, scale = 1)
plot
#create common x and y labels
x.grob <- textGrob("Lake Time Series Length (Years)", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))
tsplot<-grid.arrange(arrangeGrob(plot, bottom = x.grob))

ggsave("./FigureS2.tiff", plot = tsplot, units="in", width=10, height=10, dpi=500, device = "tiff")

###### Figure S3 #####
lakes <- read.table("new_input50.csv", header = TRUE, sep = ",")
colnames(lakes) <- c("lake", "lakename", "latitude", "Latitude", "longitude", "longstandard", "Longitude", "elevation", "Elevation", "surface_area", "Surface_Area", "mean_depth", "Mean_Depth", "Air_Temperature", "Precipitation", "ID_slope", "ID_slope_p", "obs")

fit1<-lm(ID_slope~Air_Temperature+Surface_Area+Elevation+Latitude+Longitude+Mean_Depth, data=lakes)
testplot<-plot_summs(fit1, scale = TRUE, transform.response=TRUE, inner_ci_level = .9, model.names = c("Lakes Overlapping"), legend.title = "   Ice Duration Trend\nSignificance Threshold", colors = "Rainbow", coefs = c("Air Temperature" = "Air_Temperature",  "Latitude" = "Latitude", "Longitude" = "Longitude",  "Elevation" = "Elevation", "Mean Depth" = "Mean_Depth",  "Surface Area" = "Surface_Area"))
testplot
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Helvetica'),
        legend.title=element_text(size=12.5),
        axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        legend.text = element_text(size = 12.5), 
        )
plot50 <- testplot + apatheme + labs(x = "\n Beta Estimate \n ", y = "\n Predictor \n ")  
ggsave("./FigureS3.tiff", plot = plot50, units="in", width=10, height=10, dpi=500, device = "tiff")

###### Figure S4 #####
lakes1 <- read.table("lakes_stand_tibble.csv", header = TRUE, sep = ",")
colnames(lakes1) <- c("lake", "year_ice_off", "value", "metric")

standplot<-ggplot(data = lakes1, mapping = aes(x = year_ice_off, y = value, group = metric, color=metric)) +
  geom_point() + geom_smooth(method=lm) + theme_bw() + theme(legend.position="bottom") + labs(x="Year", y="Standardized Metric") +
  scale_color_manual(name="", labels = c("Air Temperature", "Ice Duration (Days)"), values = c("blue", "red")) +
  theme(axis.title.x = element_text(size=25), axis.text.x=element_text(size=20)) + theme(axis.title.y = element_text(size=25), axis.text.y=element_text(size=20)) +
  theme(legend.text=element_text(size=20))

ggsave("./FigureS4.tiff", plot = standplot, units="in", width=10, height=10, dpi=500, device = "tiff")

###### Figure S5 #####
lakes <- read.table("lakesnewtrends_mar28_oldtemp_newppt.csv", header = TRUE, sep = ",")
summary (lakes) 
colnames(lakes) <- c("lake", "lakename", "latitude", "Latitude", "longitude", "longstand", "Longitude", "longcont", "LONGRAW", "loglongcont", "elevation", "Elevation", "surface_area", "Surface_Area", "mean_depth", "Mean_Depth", "Air_Temperature", "AT_stand", "logAT", "Precipitation", "PPT_stand", "logPPT", "ID_slope", "posneg", "ID_slope_p", "obs", "TS_CAT", "CONT")

lakes4<-subset(lakes, lakes$obs>24)

plot<-ggplot(lakes4, aes(obs, ID_slope, fill = posneg)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

tslength <- plot + theme_bw() + theme(axis.title.x = element_text(size=20), axis.text.x=element_text(size=15)) + 
  theme(axis.title.y = element_text(size=20), axis.text.y=element_text(size=15)) +
  theme(legend.text=element_text(size=20)) + 
  theme(legend.position="none") +
  xlab("Length of Ice Duration Time Series (years)") + ylab("Ice Duration Slope")

ggsave("./FigureS5.tiff", plot = tslength, units="in", width=10, height=10, dpi=500, device = "tiff") 
