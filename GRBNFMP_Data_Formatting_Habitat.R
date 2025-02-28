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

#Join tables
Habitat<-full_join(Header,Mesohabitat, by=c("GlobalID"="ParentGlobalID"))

#correct for timezone
Habitat$Start_Date <- Habitat$Start_Date - hms("07:00:00")
Habitat$End_Date <- Habitat$End_Date - hms("07:00:00")

Habitat$Date <- as.Date(Habitat$Start_Date)
Habitat$Time <- format(Habitat$Start_Date, "%H:%M")

Habitat$UTM_Zone <- paste(Habitat$UTM_Zone,Habitat$UTM_Band)

#Select what dates you want to see catch data for
DateFunction <- function(x,y) {Habitat[Habitat$Start_Date >= x & Habitat$Start_Date <= y,]}

Date1 <- as.Date("2024-01-01")
Date2 <- as.Date("2024-12-31")

Habitat <- DateFunction(Date1,Date2)

#calculate discharge at each station (must run discharge script first)
DischargeCalc <- GRBNFMP_Discharge_Data %>%
  mutate(Discharge = Interval_width*Depth_discharge*Velocity)

DischargebyStation <- DischargeCalc %>%
  dplyr::group_by(StationName) %>%
  dplyr::summarise(Discharge = sum(Discharge))

Habitat<-left_join(Habitat,DischargebyStation, by=c("StationName"="StationName"))


GRBNFMP_Habitat_Data<- Habitat[ ,c("Water_Name","StationName","GPS_Datum","UTM_Zone", 
                                 "UTM_Band","Easting_Lower","Northing_Lower", "Easting_Upper", "Northing_Upper","Date","Time", "Habitat_number", "Habitat_Type",
                                 "Length_Habitat", "Width_Habitat", "Depth_Pool", "Water_Temp", "Water_DO", "Water_pH","conductivity","Discharge", "Notes",
                                 "Surveyors","Creator.y", "CreationDate.x", "Editor.y", "EditDate.y")]

GRBNFMP_Habitat_Data <- GRBNFMP_Habitat_Data %>% dplyr::rename("Station_Name" = "StationName",
                                                  "Names_of_Surveyors" = "Surveyors",
                                                  "Habitat_Length"="Length_Habitat",
                                                  "Habitat_Width"="Width_Habitat",
                                                  "Habitat_Depth"="Depth_Pool",
                                                  "Comments"="Notes",
                                                  "DO"="Water_DO",
                                                  "pH"="Water_pH",
                                                  "Names_of_Surveyors"="Surveyors",
                                                  "Entered_by"="Creator.y",
                                                  "Entered_Date"="CreationDate.x",
                                                  "Checked_by"="Editor.y",
                                                  "Checked_Date"="EditDate.y")



#Append habitat sheet to GRBNFMP Data excel
write.xlsx(GRBNFMP_Habitat_Data, file = "dependencies/GRBNFMP_Data_2024.xlsx",
           sheetName = "Habitat", append = TRUE)

