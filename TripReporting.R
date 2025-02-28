#P.Reap GRBMP trip reporting script to be run at the end of data capture from a GRBMP site. 

# Loads packages function and other lab functions
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
# Load required packages
packages(readxl)
packages(xlsx)
packages(writexl)
packages(dplyr)
packages(lubridate) 
packages(reshape2)
packages(tidyverse)
packages(FSA)
packages(quarto)


#Choose excel file to upload 
GRBMPdata = file.choose()


# Bring in data from excel
Header <- read_excel(GRBMPdata, sheet = "Gila_River_Basin_Native_Mon_0")
Mesohabitat <-read_excel(GRBMPdata, sheet = "Mesohabitat_1")
Effort <- read_excel(GRBMPdata, sheet = "Effort_Repeat_2")
Catch <- read_excel(GRBMPdata, sheet = "fish_repeat_4")

#rename field in effort to match mesohabitat
names(Effort)[5] <- "Habitat_number"

#merge effort and mesohabitat
Effort_hab <- merge(Effort, Mesohabitat,by=c("Habitat_number","ParentGlobalID"))

#Join tables
Effort_header <-full_join(Header,Effort_hab, by=c("GlobalID"="ParentGlobalID"))
Effort_catch <- full_join(Effort_hab, Catch, by=c("GlobalID.x"="ParentGlobalID"))
Final <-full_join(Header,Effort_catch, by=c("GlobalID"="ParentGlobalID"))

#correct for timezone (Also for set and pull times!)
Final$Start_Date <- Final$Start_Date - hms("07:00:00")
Final$End_Date <- Final$End_Date - hms("07:00:00")
Final$Set_time <- Final$Set_time - hms("07:00:00")
Final$pull_time <- Final$pull_time - hms("07:00:00")

#create trap duration field
Final$DurationHr<- as.POSIXct(Final$pull_time) - as.POSIXct(Final$Set_time)
Final$DurationHr<-as.numeric(Final$DurationHr)

#typing
Species_list <- read_excel("dependencies/species list.xlsx")
colnames(Species_list) <- c('COMMON', 'Species', 'SCIENTIFIC')
Final <- left_join(Final, Species_list, by = "Species")

Final$Start_Time <- format(Final$Start_Date, "%H:%M")
Final$Start_Date <- as.Date(Final$Start_Date)

Final$End_Time <- format(Final$End_Date, "%H:%M")
Final$End_Date <- as.Date(Final$End_Date)

Final$Count[is.na(Final$Count) == TRUE&is.na(Final$Fish_Length) == FALSE] <- 1 
Final$Count[Final$Species == "0000"] <- 0   
Final$Count[Final$Species == "0"] <- 0    

Final$UTM_Zone <- paste(Final$UTM_Zone,Final$UTM_Band)

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "GIIN" & (Final$Fish_Length) <= 100 & (Final$Fish_Length) >50] <- "51-100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "GIIN" & (Final$Fish_Length) > 100] <- ">100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "GIIN" & (Final$Fish_Length) <= 50] <- "<=50"  

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "ONMY" & (Final$Fish_Length) <= 100 & (Final$Fish_Length) >50] <- "51-100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "ONMY" & (Final$Fish_Length) > 100] <- ">100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "ONMY" & (Final$Fish_Length) <= 50] <- "<=50"  

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "LECY" & (Final$Fish_Length) <= 100 & (Final$Fish_Length) >50] <- "51-100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "LECY" & (Final$Fish_Length) > 100] <- ">100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "LECY" & (Final$Fish_Length) <= 50] <- "<=50" 

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "AMNA" & (Final$Fish_Length) <= 100 & (Final$Fish_Length) >50] <- "51-100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "AMNA" & (Final$Fish_Length) > 100] <- ">100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "AMNA" & (Final$Fish_Length) <= 50] <- "<=50" 

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "PYOL" & (Final$Fish_Length) <= 100 & (Final$Fish_Length) >50] <- "51-100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "PYOL" & (Final$Fish_Length) > 100] <- ">100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "PYOL" & (Final$Fish_Length) <= 50] <- "<=50"  

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "SATR" & (Final$Fish_Length) <= 100 & (Final$Fish_Length) >50] <- "51-100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "SATR" & (Final$Fish_Length) > 100] <- ">100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "SATR" & (Final$Fish_Length) <= 50] <- "<=50"  

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "MISA" & (Final$Fish_Length) <= 100 & (Final$Fish_Length) >50] <- "51-100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "MISA" & (Final$Fish_Length) > 100] <- ">100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "MISA" & (Final$Fish_Length) <= 50] <- "<=50"  

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "ICPU" & (Final$Fish_Length) <= 100 & (Final$Fish_Length) >50] <- "51-100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "ICPU" & (Final$Fish_Length) > 100] <- ">100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "ICPU" & (Final$Fish_Length) <= 50] <- "<=50"  

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "MIDO" & (Final$Fish_Length) <= 100 & (Final$Fish_Length) >50] <- "51-100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "MIDO" & (Final$Fish_Length) > 100] <- ">100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "MIDO" & (Final$Fish_Length) <= 50] <- "<=50"  

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "TICO" & (Final$Fish_Length) >= 40] <- ">=40"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "TICO" & (Final$Fish_Length) < 40] <- "<40"  

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "MEFU" & (Final$Fish_Length) >= 40] <- ">=40"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "MEFU" & (Final$Fish_Length) < 40] <- "<40"  

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "ONAP" & (Final$Fish_Length) <= 100 & (Final$Fish_Length) >50] <- "51-100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "ONAP" & (Final$Fish_Length) > 100] <- ">100"  
Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "ONAP" & (Final$Fish_Length) <= 50] <- "<=50"  

#correct for timezone (Also for set and pull times!)
Effort_header$Start_Date <- Effort_header$Start_Date - hms("07:00:00")
Effort_header$End_Date <- Effort_header$End_Date - hms("07:00:00")
Effort_header$Set_time <- Effort_header$Set_time - hms("07:00:00")
Effort_header$pull_time <- Effort_header$pull_time - hms("07:00:00")

#create trap duration field
Effort_header$DurationHr<- as.POSIXct(Effort_header$pull_time) - as.POSIXct(Effort_header$Set_time)
Effort_header$DurationHr<-as.numeric(Effort_header$DurationHr)

## Reorder columns for output 
TripCatch <- Final[ ,c("Water_Name","StationName","Effort_ID","Gear","Habitat_Type", "Start_Date","SCIENTIFIC", "Species", "COMMON",
                              "Size_Class","Count","Fish_Length", "Fish_Weight", "Disposition","FishComments")]

TripEffort <- Effort_header[ ,c("Water_Name","StationName","Effort_ID", "Gear","Habitat_Type", "Voltage", "Amperage",
                        "FreqHz", "DutyCycle", "SecondsShocked", "Set_time", "pull_time", "DurationHr")]


##### Write csv (CHANGE FILE NAME)   
write.xlsx(TripCatch, "data/TripReportingData/RomeroBearSabinoData.xlsx", 
           sheetName = "Catch", append = TRUE)
write.xlsx(TripEffort, "data/TripReportingData/RomeroBearSabinoData.xlsx", 
           sheetName = "Effort", append = TRUE)

save.image("TripSummary.RData")

