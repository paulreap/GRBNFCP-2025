##### P. Reap 1/24/2024; M&A; Script tracking CPUE trends of annual sites monitored by GRBMP. 
##### Will continue to use script as more survey years unfold, most sites are on a three year rotation, eventually 
##### all sites will be able to run on this script and have CPUE trend analysis for them.

#Run Database_Wrangling_Script first
if(exists("CPUEse")==FALSE){
  if(file.exists("GRBMPReporting.RData")){
    load("GRBMPReporting.RData")
  }
  else{source("Database_Wrangling_Script.R")}
}

#Load packages and other lab functions

packages(jtools)
packages(readxl)
packages(xlsx)
packages(dplyr)
packages(ggplot2)

#Table of total catch by year, by site
AnnualCatch <- CPUETrend %>%
  dplyr::group_by(Year, Stream, Species) %>%
  dplyr::summarise(Catch = sum(Count), CPUE = sum(CPUE)) %>%
  dplyr::ungroup()

TICOGISHeatmap <- Catch %>%
  filter(Water_Name == "Hot Springs Canyon", CODE == "TICO")

write.csv(TICOGISHeatmap, "output/TICOGISHeatmap.csv")
#Blue River decade trend
CPUELBR <- CPUEse %>%
  filter(Stream == c('Lower Blue River')) %>%
  ggplot(aes(x=Year, y=CPUE, color = Species)) +
  geom_point(aes(shape = Species), size = 2.5) +
  geom_line() +
  geom_errorbar(aes(ymin=CPUE-se, ymax=CPUE+se), width=.2) +
  scale_color_manual(values=(c("#49b7fc", "#ff7b00")))+
  scale_y_continuous(name = "CPUE (Fish/Hr)", limits=c(0,300)) +
  scale_x_continuous(breaks=2012:2024) +
  theme_bw() +
  theme(text=element_text(size=16), axis.text.x=element_text(angle=45,hjust=0.7,vjust= 0.9)) 

CPUELBR
#Hot Springs trend
CPUEHSC <- CPUEse %>%
  filter(Stream == c('Hot Springs Canyon')) %>%
  ggplot(aes(x=Year, y=CPUE, color = Species)) +
  geom_point(aes(shape = Species), size = 2.5) +
  geom_line() +
  geom_errorbar(aes(ymin=CPUE-se, ymax=CPUE+se), width=.2) +
  scale_color_manual(values=(c("#669933","#49b7fc", "#ff7b00")))+
  scale_y_continuous(name = "CPUE (Fish/Hr)", limits=c(0,180)) +
  scale_x_continuous(breaks=2012:2024)+
  theme_bw() +
  theme(text=element_text(size=16), axis.text.x=element_text(angle=45,hjust=0.7,vjust= 0.9))

CPUEHSC

#Monkey Spring Trend
CPUEMKS <- CPUEse %>%
  filter(Stream == c('Monkey Spring')) %>%
  ggplot(aes(x=Year, y=CPUE, color = Species)) +
  geom_point(aes(shape = Species), size = 2.5) +
  geom_line() +
  geom_errorbar(aes(ymin=CPUE-se, ymax=CPUE+se), width=.2) +
  scale_color_manual(values=(c("#49b7fc", "#ff7b00")))+
  scale_y_continuous(limits=c(0,15)) +
  scale_x_continuous(breaks=2012:2024) +
  ylab(expression('CPUE'  (Fish/m^2))) +
  theme_bw() +
  theme(text=element_text(size=16), axis.text.x=element_text(angle=45,hjust=0.7,vjust= 0.9)) 
CPUEMKS

# Cottonwood Spring trend
CPUECWS <- CPUEse %>%
  filter(Stream == c('Cottonwood Spring')) %>%
  ggplot(aes(x=Year, y=CPUE, color = Species)) +
  geom_point(aes(shape = Species), size = 2.5) +
  geom_line() +
  geom_errorbar(aes(ymin=CPUE-se, ymax=CPUE+se), width=.2) +
  scale_color_manual(values=(c("#49b7fc", "#ff7b00")))+
  scale_y_continuous(limits=c(0,50)) +
  scale_x_continuous(breaks=2012:2024) +
  ylab(bquote('CPUE' (Fish/m^2))) +
  theme_bw() +
  theme(text=element_text(size=16), axis.text.x=element_text(angle=45,hjust=0.7,vjust= 0.9)) 
CPUECWS

#Fresno Canyon trend
CPUEFRC <- CPUEse %>%
  filter(Stream == c('Fresno Canyon')) %>%
  ggplot(aes(x=Year, y=CPUE, color = Species)) +
  geom_point(aes(shape = Species), size = 2.5) +
  geom_line() +
  geom_errorbar(aes(ymin=CPUE-se, ymax=CPUE+se), width=.2) +
  scale_color_manual(values=(c("#49b7fc", "#ff7b00")))+
  scale_y_continuous(name = "CPUE (Fish/Hr)", limits=c(0,25)) +
  scale_x_continuous(breaks=2012:2024) +
  theme_bw() +
  theme(text=element_text(size=16), axis.text.x=element_text(angle=45,hjust=0.7,vjust= 0.9)) 
CPUEFRC

#Coal Mine Canyon trend
CPUECMC <- CPUEse %>%
  filter(Stream == c('Coal Mine Canyon')) %>%
  ggplot(aes(x=Year, y=CPUE, color = Species)) +
  geom_point(aes(shape = Species), size = 2.5) +
  geom_line() +
  geom_errorbar(aes(ymin=CPUE-se, ymax=CPUE+se), width=.2) +
  scale_color_manual(values=(c("#49b7fc", "#ff7b00")))+
  scale_y_continuous(name = "CPUE (Fish/Hr)", limits=c(0,50)) +
  scale_x_continuous(breaks=2012:2024) +
  theme_bw() +
  theme(text=element_text(size=16), axis.text.x=element_text(angle=45,hjust=0.7,vjust= 0.9)) 
CPUECMC

#SHeehy Springs
CPUESS <- CPUEse %>%
  filter(Stream == ('Sheehy Spring')) %>%
  ggplot(aes(Year, CPUE, color = Species)) +
  geom_point(aes(shape = Species), size = 2.5) +
  geom_line() +
  geom_errorbar(aes(ymin=CPUE-se, ymax=CPUE+se), width=.2) +
  scale_color_manual(values=(c("#669933")))+
  scale_y_continuous(name = "CPUE (Fish/Hr)", limits=c(0,2)) +
  scale_x_continuous(breaks = 2014:2024) +
  theme_bw() +
  theme(text=element_text(size=16), axis.text.x=element_text(angle=45,hjust=0.7,vjust= 0.9))

CPUESS
