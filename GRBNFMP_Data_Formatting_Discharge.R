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
Discharge <-read_excel(GRBMPdata, sheet = "Discharge_Measurements_3")



#Join tables
Discharge_header <-full_join(Header,Discharge, by=c("GlobalID"="ParentGlobalID"))

#correct for timezone (Also for set and pull times!)
Discharge_header$Start_Date <- Discharge_header$Start_Date - hms("07:00:00")
Discharge_header$End_Date <- Discharge_header$End_Date - hms("07:00:00")


#Select what dates you want to see catch data for
DateFunction <- function(x,y) {Discharge_header[Discharge_header$Start_Date >= x & Discharge_header$Start_Date <= y,]}

Date1 <- as.Date("2024-01-01")
Date2 <- as.Date("2024-12-31")

Discharge_header <- DateFunction(Date1,Date2)

GRBNFMP_Discharge_Data<- Discharge_header[ ,c("Water_Name","StationName","GPS_Datum","UTM_Zone", 
                                   "UTM_Band", "Discharge_Easting", "Discharge_Northing", "interval_num", "Interval_width","Distance",
                                   "Depth_discharge","Velocity", "Discharge_notes", "Surveyors","Creator.y", "CreationDate.x", "Editor.y", "EditDate.y")]


#remove sites where no discharge is measured
GRBNFMP_Discharge_Data <- GRBNFMP_Discharge_Data[!is.na(GRBNFMP_Discharge_Data$Discharge_Easting),]

#Append discharge sheet to GRBNFMP Data excel
write.xlsx(GRBNFMP_Discharge_Data, file = "dependencies/GRBNFMP_Data_2024.xlsx",
           sheetName = "Discharge", append = TRUE)

