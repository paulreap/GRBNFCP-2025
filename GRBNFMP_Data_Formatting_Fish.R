# Packages function to load or install a package
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
packages(dplyr)
packages(lubridate) 
packages(reshape2)
packages(tidyverse)
packages(FSA)


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
colnames(Species_list)[2]="Species"
Final <- left_join(Final, Species_list, by = "Species")

Final$Start_Time <- format(Final$Start_Date, "%H:%M")
Final$Start_Date <- as.Date(Final$Start_Date)

Final$End_Time <- format(Final$End_Date, "%H:%M")
Final$End_Date <- as.Date(Final$End_Date)

Final$Count[is.na(Final$Count) == TRUE&is.na(Final$Fish_Length) == FALSE] <- 1 
Final$Count[Final$Species == "0000"] <- 0   
Final$Count[Final$Species == "0"] <- 0    

Final$UTM_Zone <- paste(Final$UTM_Zone,Final$UTM_Band)

Final$Size_Class[is.na(Final$Size_Class) == TRUE & (Final$Species) == "GIIN" & (Final$Fish_Length) <+ 100 & (Final$Fish_Length) >50] <- "51-100"  
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

#Select what dates you want to see catch data for
DateFunction <- function(x,y) {Final[Final$Start_Date >= x & Final$Start_Date <= y,]}

Date1 <- as.Date("2024-01-01")
Date2 <- as.Date("2024-12-31")

Final <- DateFunction(Date1,Date2)


## Reorder columns for output (need to rename still!!)
GRBNFMP_Fish_Data<- Final[ ,c("Water_Name","StationName","GPS_Datum","UTM_Zone",
                              "Easting_Lower","Northing_Lower", "Easting_Upper", "Northing_Upper","Gear","Effort_ID", "Start_Date","Start_Time",
                              "End_Date", "End_Time", "Set_time","pull_time", "DurationHr", "Habitat_Type", "Length_Habitat", "Width_Habitat", "Depth_Pool", "Voltage", "Amperage", "FreqHz", "SecondsShocked","SCIENTIFIC",
                              "Size_Class","Count","Fish_Length", "Fish_Weight", "Disposition","FishComments",
                              "Surveyors","Creator.y", "CreationDate.x", "Editor.y", "EditDate.y")]

#rename
GRBNFMP_Fish_Data <- GRBNFMP_Fish_Data %>% dplyr::rename("Station_Name" = "StationName",
                                                  "Gear_Type"="Gear",
                                                  "Effort_Name"="Effort_ID",
                                                  "Names_of_Surveyors" = "Surveyors",
                                                  "Species" = "SCIENTIFIC",
                                                  "Date"="Start_Date",
                                                  "Condition"="Disposition",
                                                  "Comments"="FishComments",
                                                  "Efisher_Volts"="Voltage",
                                                  "Efisher_Amps"="Amperage",
                                                  "Efisher_Frequency" = "FreqHz",
                                                  "Efisher_Seconds"="SecondsShocked",
                                                  "Entered_by"="Creator.y",
                                                  "Entered_Date"="CreationDate.x",
                                                  "Checked_by"="Editor.y",
                                                  "Checked_Date"="EditDate.y")


#### Write csv (CHANGE FILE NAME)   
write.xlsx(GRBNFMP_Fish_Data, file = "dependencies/GRBNFMP_Data_2024.xlsx",
           sheetName = "Fish", append = TRUE)

