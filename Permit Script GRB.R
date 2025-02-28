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
packages(sf)
packages(sp)
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
Fish <-full_join(Header,Effort_catch, by=c("GlobalID"="ParentGlobalID"))


#correct for timezone (Also for set and pull times!)
Fish$Start_Date <- Fish$Start_Date# - hms("07:00:00")
Fish$End_Date <- Fish$End_Date# - hms("07:00:00")
Fish$Set_time <- Fish$Set_time# - hms("07:00:00")
Fish$pull_time <- Fish$pull_time# - hms("07:00:00")

#typing
Fish$Count[is.na(Fish$Count) == TRUE&is.na(Fish$Fish_Length) == FALSE] <- 1 
Fish$Count[Fish$Species == "0000"] <- 0   
Fish$Count[Fish$Species == "0"] <- 0    


#Create Size Class value for fishes with exact lengths taken (need to add any new species each year)
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "GIIN" & (Fish$Fish_Length) < 100 & (Fish$Fish_Length) >50] <- "51-100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "GIIN" & (Fish$Fish_Length) > 100] <- ">100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "GIIN" & (Fish$Fish_Length) <= 50] <- "<=50"  

Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "ONMY" & (Fish$Fish_Length) < 100 & (Fish$Fish_Length) >50] <- "51-100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "ONMY" & (Fish$Fish_Length) > 100] <- ">100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "ONMY" & (Fish$Fish_Length) <= 50] <- "<=50"  

Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "LECY" & (Fish$Fish_Length) < 100 & (Fish$Fish_Length) >50] <- "51-100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "LECY" & (Fish$Fish_Length) > 100] <- ">100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "LECY" & (Fish$Fish_Length) <= 50] <- "<=50" 

Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "AMNA" & (Fish$Fish_Length) < 100 & (Fish$Fish_Length) >50] <- "51-100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "AMNA" & (Fish$Fish_Length) > 100] <- ">100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "AMNA" & (Fish$Fish_Length) <= 50] <- "<=50" 

Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "PYOL" & (Fish$Fish_Length) < 100 & (Fish$Fish_Length) >50] <- "51-100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "PYOL" & (Fish$Fish_Length) > 100] <- ">100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "PYOL" & (Fish$Fish_Length) <= 50] <- "<=50"  

Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "SATR" & (Fish$Fish_Length) < 100 & (Fish$Fish_Length) >50] <- "51-100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "SATR" & (Fish$Fish_Length) > 100] <- ">100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "SATR" & (Fish$Fish_Length) <= 50] <- "<=50"  

Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "MISA" & (Fish$Fish_Length) < 100 & (Fish$Fish_Length) >50] <- "51-100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "MISA" & (Fish$Fish_Length) > 100] <- ">100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "MISA" & (Fish$Fish_Length) <= 50] <- "<=50"  

Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "ICPU" & (Fish$Fish_Length) < 100 & (Fish$Fish_Length) >50] <- "51-100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "ICPU" & (Fish$Fish_Length) > 100] <- ">100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "ICPU" & (Fish$Fish_Length) <= 50] <- "<=50"  

Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "MIDO" & (Fish$Fish_Length) < 100 & (Fish$Fish_Length) >50] <- "51-100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "MIDO" & (Fish$Fish_Length) > 100] <- ">100"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "MIDO" & (Fish$Fish_Length) <= 50] <- "<=50"  

Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "TICO" & (Fish$Fish_Length) >= 40] <- ">=40"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "TICO" & (Fish$Fish_Length) < 40] <- "<40"  

Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "MEFU" & (Fish$Fish_Length) >= 40] <- ">=40"  
Fish$Size_Class[is.na(Fish$Size_Class) == TRUE & (Fish$Species) == "MEFU" & (Fish$Fish_Length) < 40] <- "<40"  


#permit reporting
Permit <- Fish[ ,c("Surveyors","Species","Count","Start_Date","Easting_Lower","Northing_Lower",
"UTM_Zone","UTM_Band", "GPS_Datum", "Size_Class", "Disposition","Habitat_Type", "Water_Name", "FishComments")]

#Size Classifications
Permit$Size_Class[(Permit$Size_Class) == ">100" & (Permit$Species) == "GIIN"] <- "Adult"  
Permit$Size_Class[(Permit$Size_Class) == "51-100" & (Permit$Species) == "GIIN"] <- "Juvenile"  
Permit$Size_Class[(Permit$Size_Class) == ">50" & (Permit$Species) == "GIIN"] <- "Juvenile"  
Permit$Size_Class[(Permit$Size_Class) == ">100" & (Permit$Species) == "MISA"] <- "Adult"  
Permit$Size_Class[(Permit$Size_Class) == "51-100" & (Permit$Species) == "MISA"] <- "Juvenile"  
Permit$Size_Class[(Permit$Size_Class) == ">50" & (Permit$Species) == "MISA"] <- "Juvenile"  
Permit$Size_Class[(Permit$Size_Class) == ">=40"] <- "Adult"
Permit$Size_Class[(Permit$Size_Class) == "<40"] <- "Juvenile"
Permit$Size_Class[(Permit$Size_Class) == ">=20"] <- "Adult"
Permit$Size_Class[(Permit$Size_Class) == "<20"] <- "Juvenile"
Permit$Size_Class[(Permit$Size_Class) == ">100"] <- "Adult"
Permit$Size_Class[(Permit$Size_Class) == "51-100"] <- "Juvenile"
Permit$Size_Class[(Permit$Size_Class) == "<50"] <- "Juvenile"
Permit$Size_Class[(Permit$Size_Class) == "<=50"] <- "Juvenile"
Permit$Size_Class[(Permit$Size_Class) == "TADPOLE"] <- "Juvenile"
Permit$Size_Class[(Permit$Size_Class) == "JUVENILE"] <- "Juvenile"
Permit$Size_Class[(Permit$Size_Class) == "ADULT"] <- "Adult"
Permit$Size_Class[(Permit$Species) == "KISO"] <- "Adult"

#disposition elaboration
Permit$Disposition[(Permit$Disposition) == "RA"] <- "Released Alive"
Permit$Disposition[(Permit$Disposition) == "MN"] <- "Mortality Not Collected"

#Add the same naming convention to all surveyors (change surveyor name if necessary)
Permit$Surveyors[is.na(Permit$Surveyors)==FALSE] <- "M&A -- PCR"
Permit$Surveyors[is.na(Permit$Surveyors)==TRUE] <- "M&A -- PCR"

#Remove the timestamp
Permit$Start_Date <- as.Date(Permit$Start_Date)

#Create a sex column, make all intputs "U'
Permit$Sex <- add_column(Permit$Surveyors)
Permit$Sex[is.na(Permit$Surveyors)==FALSE] <- "U"
Permit$Sex[is.na(Permit$Surveyors)==TRUE] <- "U"

#Left joining Species list to pull in Sci and common names (add species to list if neccesary)
Species_list <- read_excel("dependencies/species list.xlsx")

Species_list <- rename(Species_list, "Species" = "CODE") 

Permit <- left_join(Permit, Species_list, by = "Species")

Permit <- Permit[!Permit$Count == "0",]


#Reorder COlumns
FullPermit <- Permit[, c("Surveyors","SCIENTIFIC","COMMON", "Count","Start_Date","Easting_Lower","Northing_Lower",
            "UTM_Zone","UTM_Band", "GPS_Datum", "Size_Class","Sex", "Disposition","Habitat_Type", "Water_Name", "FishComments")]


#Rename columns to match permit format
FullPermit <- FullPermit %>% rename("Observer (collecting, handling and/or surveying)" = "Surveyors",
                                    "Common Name" = "COMMON",
                                    "Scientific Name" = "SCIENTIFIC",
                                    "Date"="Start_Date",
                                    "Easting" ="Easting_Lower",
                                    "Northing" = "Northing_Lower",
                                    "Zone" = "UTM_Zone",
                                    "Datum"="GPS_Datum",
                                    "Lifestage"="Size_Class",
                                    "Habitat Description"="Habitat_Type",
                                    "Other locality data"="Water_Name",
                                    "Comments (reproductive status, behavior, etc.)"="FishComments")

#Add permit columns that do not exist in our data structure
FullPermit <- FullPermit %>%
  add_column(County = NA, .after="Date")
FullPermit <- FullPermit %>%
  add_column(Museum = NA, .after="Disposition")
FullPermit <- FullPermit %>%
  add_column(Marked = NA, .after="Museum")
FullPermit <- FullPermit %>%
  add_column(FieldTag = NA, .after="Marked")

#Select what dates you want to see catch data for
DateFunction <- function(x,y) {FullPermit[FullPermit$Date >= x & FullPermit$Date <= y,]}

Date1 <- as.Date("2025-01-01")
Date2 <- as.Date("2025-12-31")

SelectedPermit <- DateFunction(Date1,Date2)

#Remove full NA Rows
SelectedPermit <- SelectedPermit %>% filter(!is.na(Datum))

#Write the CSV File
write.csv(SelectedPermit, file = 'AZGFD_Permit_Report.csv', row.names = FALSE)

#########################Populating Counties by coordinates###############

unzip('dependencies/cb_2018_us_county_500k.zip')
counties <- read_sf('cb_2018_us_county_500K.shp')

AZGFD_Permit_Report <- read.csv('AZGFD_Permit_Report.csv')

#Random full row of NA Values
AZGFD_Permit_Report <- AZGFD_Permit_Report[-c(638),]

#isolate Easting and Northing from your data set in a new table
UTMPoints <- data.frame(Easting = AZGFD_Permit_Report$Easting, Northing = AZGFD_Permit_Report$Northing)
UTMPoints <- na.omit(UTMPoints)

#Create a Spatial Object in the data zone (change zone as needed)
spUTM <- SpatialPoints(UTMPoints, proj4string = CRS("+proj=utm +zone=12N +datum=NAD83"))

#Tranform into Lat Long
spgeo <- spTransform(spUTM, CRS("+proj=longlat +datum=NAD83"))

#Pull out the coordinates
LatLong <- coordinates(spgeo)

#Turn Matric into Data Frame
LatLong <- as.data.frame(LatLong) %>% rename("Latitude"="Northing","Longitude"="Easting")

#Create Spatial Frame
LatLongSF <- st_as_sf(LatLong, coords = c('Longitude','Latitude'), crs = st_crs(counties))

#Create the intersection points of count, and area
pnts <- LatLongSF %>% mutate(
  intersection = as.integer(st_intersects(geometry, counties))
  , area = if_else(is.na(intersection), '', counties$NAME[intersection])
)

#Turn back into data frame
pntsDF <- as.data.frame(pnts)

#Rename area to county
colnames(pntsDF)[3] = "County"

#Bind Permit report and pnts df
AZGFD_Permit_Report <- bind_cols(AZGFD_Permit_Report, pntsDF)

#Moving and renaming the various county rows
AZGFD_Permit_Report <- AZGFD_Permit_Report %>% relocate(County...23, .before = County...6)
AZGFD_Permit_Report <- subset(AZGFD_Permit_Report, select = -c(County...6))
colnames(AZGFD_Permit_Report)[6] = "County"
