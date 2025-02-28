####Old script written by K. Shollenberger but still works. Now lies in the GRBMP FY Summary Tables script.

# Packages function to load or install a package
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(readxl)
packages(Rmisc)
packages(dplyr)
packages(lubridate) 

#Choose excel file to upload 
GRBMPdata = file.choose()

# Bring in data from excel
GRBMP2023 <- read_excel(GRBMPdata, sheet = "Fish", col_types = "text")


#filter non-fish

#GRBMP2023 = GRBMP2023AllData %>%
#filter(Species != c('KISO')) %>%
#filter(Species != c('HYAR')) %>%
#filter(Species != c('LIYA')) %>%
#filter(Species != c('CYPRINIDAE SPP')) %>%
#filter(Species != c('RAYA')) %>%
#filter(Species != c('RANA SPP.')) %>%
#filter(Species != c('RANA SPP')) %>%
#filter(Species != c('RACA')) %>%
#filter(Species != c('NA'))
############################


number_ticks <- function(n) {function(limits) pretty(limits, n)}

####box plot 

#CPUE
GRBMP2023$Count <-as.numeric(GRBMP2023$Count)
GRBMP2023$Efisher_Seconds<-as.numeric(GRBMP2023$Efisher_Seconds)
GRBMP2023$DurationHr<-as.numeric(GRBMP2023$DurationHr)

GRBMP2023$BPEFCPUE <- GRBMP2023$Count/(GRBMP2023$Efisher_Seconds/60)
GRBMP2023$TrapCPUE <- GRBMP2023$Count/GRBMP2023$DurationHr
GRBMP2023$DNCPUE <- GRBMP2023$Count/0.353568
GRBMP2023$SeineCPUE <- GRBMP2023$Count/3.6576

#Filter by gear (creating effort sheet)
BPEF = GRBMP2023 %>%
  filter(Gear_Type == c('Backpack Electrofisher')) %>%
  group_by(Water_Name,Station_Name,Effort_Name,Gear_Type,Efisher_Seconds) %>%
  summarise(Count=(sum(Count)),CPUE=(sum(BPEFCPUE)))

write.csv(BPEF, "BPEFCPUE.csv")



#Trap CPUE
Trap = GRBMP2023 %>%
  filter(Gear_Type != c('Backpack Electrofisher')) %>%
  filter(Gear_Type != c('Dip Net')) %>%
  filter(Gear_Type != c('Seine (12x4)')) %>%
  group_by(Water_Name,Station_Name,Effort_Name,DurationHr) %>%
  summarise(Count=(sum(Count)),CPUE=(sum(TrapCPUE)))



write.csv(Trap, "TrapCPUE.csv")


#Dip Net Effort
DipNet = GRBMP2023 %>%
  filter(Gear_Type == c('Dip Net')) %>%
  group_by(Water_Name,Station_Name,Effort_Name) %>%
  summarise(Count=(sum(Count)),CPUE=(sum(DNCPUE)))

write.csv(DipNet, "DNCPUE.csv")


#Seine Effort
Seine = GRBMP2023 %>%
  filter(grepl('Seine',Gear_Type)) %>%
  group_by(Water_Name,Station_Name,Effort_Name) %>%
  summarise(Count=(sum(Count)),CPUE=(sum(SeineCPUE)))

write.csv(Seine, "SNCPUE.csv")
