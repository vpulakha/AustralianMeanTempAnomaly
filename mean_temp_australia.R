library(data.table)
library(ggplot2)
library(httr)
library(gganimate)
library(RColorBrewer)

#Read the average temperate data
mean_temp <- fread("~/Documents/customers/Datasets/weather/temperature/mean_temp_since1910.csv")
names(mean_temp) <- c("year", "mean_temp_above_or_below")
mean_temp$year <- as.Date(strptime(substr(mean_temp$year,1,4), "%Y")) 
mean_temp$country <- c("Australia")

theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #panel.grid.major=element_blank(),
        legend.title = element_text(size=10),
        legend.position = "top",
        axis.text.x=element_text(vjust=1),
        #panel.grid.minor=element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size=18,face="bold")
  )

col_strip <- brewer.pal(11,"RdBu")

ggplot(mean_temp, aes(year, country, fill=mean_temp_above_or_below))+
  geom_tile()+scale_fill_gradientn(name='Temperature \nDeviation', colors=rev(col_strip))+
  theme_strip+ 
  labs(caption="Source: Australian Bureau of Meteorology (BOM)",
       title="Australian Annual Mean Temperature Anomaly (1910-2021)",
       subtitle="Temperature Deviation from Average of 21.8 °C - calculated from 1961-1990")


#Read the average temperate data
global_mean_temp <- fread("~/Documents/customers/Datasets/weather/temperature/Global_mean_temp.csv")
names(global_mean_temp) <- c("year", "mean_temp_above_or_below")
global_mean_temp$year <- as.Date(strptime(substr(global_mean_temp$year,1,4), "%Y")) 
global_mean_temp$country <- c("Global")

aus_global_temp <- rbind(global_mean_temp, mean_temp, fill=TRUE)

ggplot(aus_global_temp[year(year) >= 1910 & year(year)<= 2019], aes(year, mean_temp_above_or_below, col=country))+geom_line(size=1.0)+
  theme_minimal()+  scale_color_manual(values=c('red','blue'))+
  scale_y_continuous(position = "left", limits = c(-1.5, 1.5), breaks = c(-1.5,-1.25, -1.0,-0.75, -0.5, -0.25,0.0,0.25, 0.5,0.75, 1.0,1.25, 1.5 ))+
  theme(legend.title = element_blank(), 
        legend.position = "top", 
        plot.title=element_text(size=18,face="bold"))+
  labs(caption="Source: Australian Bureau of Meteorology (BOM)", fill="",
       title="Global versus Australian Annual Mean Temperature Anomaly (1910-2019)",
       subtitle="Temperature Deviation from Average: Global = 13.97°C and Australia = 21.8 °C - calculated from 1961-1990")+ 
  ylab("Temperature Anomaly °C")+ xlab("")
  
  
