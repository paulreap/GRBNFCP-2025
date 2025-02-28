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
packages(FSA)
packages(lubridate)
packages(tidyr)
                                ###read sheets for each year and merge into one dataframe named Catch, Habitat, and Discharge########
Catch21 <- read_excel("dependencies/GRBNFMP_Data_2021.xlsx", sheet = "Fish")
Habitat21 <-read_excel("dependencies/GRBNFMP_Data_2021.xlsx", sheet = "Habitat")
Discharge21 <- read_excel("dependencies/GRBNFMP_Data_2021.xlsx", sheet = "Discharge")

Catch21 <- subset(Catch21, select = -(...1))
Habitat21 <- subset(Habitat21, select = -(...1))
Discharge21 <- subset(Discharge21, select = -(...1))

Catch22 <- read_excel("dependencies/GRBNFMP_Data_2022.xlsx", sheet = "Fish", 
                      col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", 
                                    "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", 
                                    "guess", "guess", "guess", "numeric", "numeric", "numeric", "numeric", "guess"
                                    , "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess"
                                    , "guess", "guess", "guess"))
Habitat22 <-read_excel("dependencies/GRBNFMP_Data_2022.xlsx", sheet = "Habitat")
Discharge22 <- read_excel("dependencies/GRBNFMP_Data_2022.xlsx", sheet = "Discharge")

Catch22 <- subset(Catch22, select = -(...1))
Habitat22 <- subset(Habitat22, select = -(...1))
Discharge22 <- subset(Discharge22, select = -(...1))

Catch23 <- read_excel("dependencies/GRBNFMP_Data_2023.xlsx", sheet = "Fish")
Habitat23 <-read_excel("dependencies/GRBNFMP_Data_2023.xlsx", sheet = "Habitat")
Discharge23 <- read_excel("dependencies/GRBNFMP_Data_2023.xlsx", sheet = "Discharge")


Catch23 <- subset(Catch23, select = -(...1))
Habitat23 <- subset(Habitat23, select = -(...1))
Discharge23 <- subset(Discharge23, select = -(...1))

Catch24 <- read_excel("dependencies/GRBNFMP_Data_2024.xlsx", sheet = "Fish")
Habitat24 <- read_excel("dependencies/GRBNFMP_Data_2024.xlsx", sheet = "Habitat")
Discharge24 <-read_excel("dependencies/GRBNFMP_Data_2024.xlsx", sheet = "Discharge")

Catch24 <- subset(Catch24, select = -(...1))
Habitat24 <- subset(Habitat24, select = -(...1))
Discharge24 <- subset(Discharge24, select = -(...1))

Catch <- rbind(Catch21, Catch22, Catch23, Catch24)
Discharge <- rbind(Discharge21, Discharge22, Discharge23, Discharge24)
Habitat <- rbind(Habitat21, Habitat22, Habitat23, Habitat24)


remove(Catch21, Catch22, Catch23, Catch24, Habitat21, Habitat22, Habitat23, Habitat24, 
       Discharge21, Discharge22, Discharge23, Discharge24)


###Adding columns to make analysis more intuitive such as: survey year, 
Catch$SurveyYear <- year(Catch$Date)
Habitat$SurveyYear <- year(Habitat$Date)
Discharge$SurveyYear <- year(Discharge$EditDate.y)

Catch$DurationHr<- as.POSIXct(Catch$pull_time) - as.POSIXct(Catch$Set_time)
Catch$DurationHr<-as.numeric(Catch$DurationHr)

Catch <- filter(Catch, rowSums(is.na(Catch)) != ncol(Catch))
#Rename species to their codes for simplicity
Species_list <- read_excel("dependencies/species list.xlsx")
Catch <- left_join(Catch, Species_list, by = c("Species" = "SCIENTIFIC")) 

#filter non-fish

Catch <- Catch %>%
filter(CODE != c('KISO')) %>%
filter(CODE != c('HYAR')) %>%
filter(CODE != c('LIYA')) %>%
filter(CODE != c('CYPRINIDAE SPP')) %>%
filter(CODE != c('RAYA')) %>%
filter(CODE != c('RANA SPP.')) %>%
filter(CODE != c('RANA SPP')) %>%
filter(CODE != c('RACA')) %>%
filter(CODE != c('ORVI')) %>%
filter(CODE != c('NA'))
############################

save.image("GRBMPReporting.RData")


write.xlsx(Catch, "output/AllCatch.xlsx")
                                                   
