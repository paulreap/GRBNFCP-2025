###### Paul Reap 1/25/2024; M&A; Script to summarise findings from GRBMP monitoring.######
###### What can be found in this script: Summary tables for sites and focal species, summary of catch by gear type, #####
######catch by mesohabitat delineation, total distance of mesohabitat ######

#Run Database_Wrangling_Script to update all data capture for the FY and save the workspace before running this script
if(exists("Catch")==FALSE){
  if(file.exists("GRBMPReporting.RData")){
    load("GRBMPReporting.RData")
  }
  else{source("Database_Wrangling_Script.R")}
}

packages(lemon)
packages(ggplot2)
packages(dplyr)
packages(tidyr)
packages(gghighlight)



### Declaring Survey year before running analysis

SurveyFY <- 2024
#####Summary tables for fiscal year by  site and focal species####
CatchFY <- Catch %>%
  filter(SurveyYear == SurveyFY)

YearlyCatch <- Catch %>%
  filter(!Gear_Type %in% ('Visual Survey')) %>%
  group_by(SurveyYear) %>%
  dplyr::summarise(Catch = sum(Count))

FYSites <- CatchFY %>%
  filter(SurveyYear == SurveyFY) %>%
  group_by(Water_Name) %>%
  dplyr::summarise(Catch = sum(Count))

AllStations <- CatchFY %>%
  group_by(Site = Water_Name, Station = Station_Name, Easting_Lower, Northing_Lower, CODE) %>%
  dplyr::summarise(Count = sum(Count)) %>%
  ungroup() %>%
  pivot_wider(names_from = CODE, values_from = Count)
###remove column of No fish Caught, and replace NAs with 0; if there are no stations with No Fish Caught
###comment out line 32
RemoveNFC <- "0000"
col_index <- which(names(AllStations) == RemoveNFC)
AllStations <- AllStations[, -col_index]

AllStations[is.na(AllStations)] <- 0
### add Catch column and Total catch to table
CatchColumn <- CatchFY %>%
  group_by(Station = Station_Name) %>%
  summarise(Catch = sum(Count))

AllStations <- left_join(AllStations, CatchColumn)

TotalRow <- c("Total", colSums(AllStations[, -1, drop = FALSE]), sum(AllStations$Catch))

AllStations <- rbind(AllStations, TotalRow)

###Save output to csv for mapping in GIS software
write.csv(AllStations, "output/AllStationCatchFY.csv")

CatchbyGear <- Catch %>%
  group_by(SurveyYear, Gear_Type) %>%
  dplyr::summarise(Catch = sum(Count), Stations = n_distinct(Station_Name))

POOCcatch <- Catch %>%
  filter(SurveyYear == SurveyFY, CODE == "POOC") %>%
  group_by(SurveyYear) %>%
  dplyr::summarise(Catch = sum(Count), Stations = n_distinct(Station_Name), sites = n_distinct(Water_Name)) %>%
  ungroup()

TICOcatch <- Catch %>%
  filter(SurveyYear == SurveyFY, CODE == "TICO") %>%
  group_by(SurveyYear) %>%
  dplyr::summarise(Catch = sum(Count), Stations = n_distinct(Station_Name), sites = n_distinct(Water_Name)) %>%
  ungroup()

MEFUcatch <- Catch %>%
  filter(SurveyYear == SurveyFY, CODE == "MEFU") %>%
  group_by(SurveyYear) %>%
  dplyr::summarise(Catch = sum(Count), Stations = n_distinct(Station_Name), sites = n_distinct(Water_Name)) %>%
  ungroup()

GIINcatch <- Catch %>%
  filter(SurveyYear == SurveyFY, CODE == "GIIN") %>%
  group_by(SurveyYear) %>%
  dplyr::summarise(Catch = sum(Count), Stations = n_distinct(Station_Name), sites = n_distinct(Water_Name)) %>%
  ungroup()

mesohabitat <- Catch %>%
  filter(SurveyYear == SurveyFY, CODE == 'POOC' | CODE == 'GIIN' | CODE == 'TICO' | CODE == 'MEFU') %>%
  dplyr::group_by(CODE, Size_Class, Habitat_Type) %>%
  dplyr::summarise(Count = sum(Count)) 
yearlymesohabitat <- Catch %>%
  filter(CODE == 'POOC' | CODE == 'GIIN' | CODE == 'TICO' | CODE == 'MEFU') %>%
  dplyr::group_by(SurveyYear, CODE, Size_Class, Habitat_Type) %>%
  dplyr::summarise(Count = sum(Count)) 


###BPEF summary data, Catch and CPUE by year, species, size class, site######

BPEFEffort <- Catch %>%
  dplyr::filter(Gear_Type == "Backpack Electrofisher") %>%
  dplyr::group_by(SurveyYear, Water_Name, Station_Name, Effort_Name, Gear_Type) %>%
  dplyr::summarise(Seconds = unique(Efisher_Seconds)) %>%
  ungroup()

BPEFEffortSum <- BPEFEffort %>%
  group_by(SurveyYear, Water_Name, Station_Name, Gear_Type) %>%
  dplyr::summarise(Seconds = sum(Seconds)) %>%
  ungroup()

BPEFCatch <- Catch %>%
  filter(Gear_Type == "Backpack Electrofisher") %>%
  group_by(Water_Name, SurveyYear, Station_Name) %>%
  dplyr::summarise(Count = sum(Count)) %>%
  ungroup()

BPEFCPUE <- BPEFEffortSum %>%
  left_join(BPEFCatch, by = c("Station_Name", "SurveyYear", "Water_Name")) %>%
  mutate(CPUEHr = Count/(Seconds/3600))
BPEFCPUE <- replace(BPEFCPUE, is.na(BPEFCPUE), 0)
BPEFYear <- BPEFCPUE %>%
  group_by(SurveyYear) %>%
  dplyr::summarise(Catch = sum(Count), Seconds = sum(Seconds))

BPEFQGISALL <- BPEFCPUE %>%
  inner_join(AllStations, by = c("Water_Name", "Station_Name")) 
write.csv(BPEFQGISALL, "output/BPEFCPUEALL.csv")

######CPUE by gear type#####
GRBMPFY <- Catch %>%
  filter(SurveyYear == SurveyFY)

GRBMPFY$Count <-as.numeric(GRBMPFY$Count)
GRBMPFY$Efisher_Seconds<-as.numeric(GRBMPFY$Efisher_Seconds)
GRBMPFY$DurationHr<-as.numeric(GRBMPFY$DurationHr)

GRBMPFY$BPEFCPUE <- GRBMPFY$Count/(GRBMPFY$Efisher_Seconds/60)
GRBMPFY$TrapCPUE <- GRBMPFY$Count/GRBMPFY$DurationHr
GRBMPFY$DNCPUE <- GRBMPFY$Count/0.353568
GRBMPFY$SeineCPUE <- GRBMPFY$Count/3.6576

#filter by gear (creating effort sheet)
BPEFFY = BPEFCPUE %>%
  filter(SurveyYear == SurveyFY)

BPEFGIS <- BPEFFY %>%
  inner_join(FYStations, by = c("Water_Name", "Station_Name"))

write.csv(BPEFFY, "output/BPEFCPUE.csv")
write.csv(BPEFGIS, "output/BPEFCPUEQGIS.csv")
#Trap CPUE
TrapFY = GRBMPFY %>%
  filter(Gear_Type != c('Backpack Electrofisher')) %>%
  filter(Gear_Type != c('Dip Net')) %>%
  filter(Gear_Type != c('Seine (12x4)')) %>%
  filter(Gear_Type != c('Visual Survey')) %>%
  group_by(Water_Name,Station_Name,Effort_Name,DurationHr) %>%
  dplyr::summarise(Count=(sum(Count)),CPUE=(sum(TrapCPUE)))

TrapYearly = Catch %>%
  filter(Gear_Type != c('Backpack Electrofisher')) %>%
  filter(Gear_Type != c('Dip Net')) %>%
  filter(Gear_Type != c('Seine (12x4)')) %>%
  filter(Gear_Type != c('Visual Survey')) %>%
  group_by(SurveyYear) %>%
  dplyr::summarise(Count=(sum(Count)), TrapHour = sum(DurationHr)) 

write.csv(TrapFY, "output/TrapCPUE.csv")
write.csv(TrapYearly, "output/TrapYearly.csv")

#Dip Net Effort
DipNetFY = GRBMPFY %>%
  filter(Gear_Type == c('Dip Net')) %>%
  group_by(Water_Name,Station_Name,Effort_Name) %>%
  dplyr::summarise(Count=(sum(Count)),CPUE=(sum(DNCPUE)))

write.csv(DipNetFY, "output/DNCPUE.csv")


#Seine Effort
SeineFY = GRBMPFY %>%
  filter(grepl('Seine',Gear_Type)) %>%
  group_by(Water_Name,Station_Name,Effort_Name) %>%
  summarise(Count=(sum(Count)),CPUE=(sum(SeineCPUE)))

write.csv(SeineFY, "output/SNCPUE.csv")

Gearsummary <- Catch %>%
  filter(SurveyYear == SurveyFY) %>%
  group_by(Gear_Type) %>%
  summarise(Catch = sum(Count))
SpeciessummaryFY <- Catch %>%
  filter(SurveyYear == SurveyFY) %>%
  group_by(Species) %>%
  summarise(Catch = sum(Count))
Speciessummary <- Catch %>%
  group_by(SurveyYear, Species) %>%
  summarise(Catch = sum(Count))

### habitat measurements

DischargeYearly <- Habitat %>%
  filter(Discharge != "NA") %>%
  group_by(SurveyYear, Water_Name) %>%
  summarise(Discharge = mean(Discharge))


mesohabitatfrequency <- Habitat %>%
  filter(!is.na(Habitat_Length)) %>%
  group_by(SurveyYear, Habitat_Type) %>%
  summarise(length = sum(Habitat_Length))

Habitat$WettedStatus <- ifelse(Habitat$Habitat_Type == "Dry", "Dry", "Wetted") 

WettedStations <- Habitat %>%
  filter(!is.na(Habitat_Length)) %>%
  group_by(SurveyYear, WettedStatus) %>%
  summarise(length = sum(Habitat_Length))

hab <- ggplot(mesohabitatfrequency, aes(SurveyYear, length, fill = Habitat_Type)) +
  geom_bar(stat="identity", position = "dodge", alpha = 3 / 4, colour = "grey50") +
  scale_fill_brewer(palette = "Spectral")+
  xlab("Survey Year") +
  ylab("Total Habitat Distance (m)") +
  scale_y_continuous(limits = c(0,6000)) +
  theme_bw()


hab

