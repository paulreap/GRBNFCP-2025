###Paul Reap Jan 26, 2024; M&A
#Use this script for the Quarto document TripSummaryemail to call to for tables and other information.

#Run TripReporting script to update all data capture for the site in question and save the workspace before running this script

if(exists("TripCatch")==FALSE){
  if(file.exists("TripSummary.RData")){
    load("TripSummary.RData")
  }
  else{source("TripReporting.R")}
}

packages(dplyr)
packages(quarto)
packages(tidyr)
packages(gt)
packages(htmltools)
packages(htmlwidgets)
packages(plotly)
### summary analysis, tables and graphs if applicable

rm(Effort_catch, Effort_hab, Effort_header, Catch, Effort)

#Summary table of catch
TripCatchSum <- TripCatch %>%
  group_by(Station = StationName, Species) %>%
  summarise(Count = sum(Count)) %>%
  ungroup() %>%
  pivot_wider(names_from = Species, values_from = Count)


###remove column of No fish Caught, and replace NAs with 0; if there are no stations with No Fish Caught
###comment out line 32
RemoveNFC <- "0000"
col_index <- which(names(TripCatchSum) == RemoveNFC)
TripCatchSum <- TripCatchSum[, -col_index]

TripCatchSum[is.na(TripCatchSum)] <- 0
### add Catch column and Total catch to table
CatchColumn <- TripCatch %>%
  group_by(Station = StationName) %>%
  summarise(Catch = sum(Count))

TripCatchSum <- left_join(TripCatchSum, CatchColumn)

TotalRow <- c("Total", colSums(TripCatchSum[, -1, drop = FALSE]), sum(TripCatchSum$Catch))

TripCatchSum <- rbind(TripCatchSum, TotalRow)

CatchTable <- gt(TripCatchSum)
###Station count fixed and random
RandomStations <- sum(!grepl("-F$", CatchColumn$Station))
FixedStations <- sum(grepl("-F$", CatchColumn$Station))
###Count number of total fish captured per species
SpeciesTotal <- TripCatch %>%
  group_by(COMMON) %>%
  summarise(Catch = sum(Count)) %>%
  ungroup() 
SpeciesTotal <- SpeciesTotal[SpeciesTotal$COMMON != "No fish caught", ]

###Habitat encountered during survey


###Coordinates of stations surveyed; using StationTable as table 2. in quarto document.
StationCoords <- Header %>%
  select(Station = StationName, Datum = GPS_Datum, Zone = UTM_Zone, Band = UTM_Band, 
         EastingLower = Easting_Lower, NorthingLower = Northing_Lower, EastingUpper = Easting_Upper, 
         NorthingUpper = Northing_Upper) %>%
  arrange(Station)

StationCoords <- StationCoords %>%
  mutate(LowerBoundary = paste0(EastingLower, "E, ", NorthingLower, "N")) %>%
  mutate(UpperBoundary = paste0(EastingUpper, "E, ", NorthingUpper, "N")) %>%
  select(-EastingLower, -NorthingLower, -EastingUpper, -NorthingUpper) 

StationTable <- gt(StationCoords)
