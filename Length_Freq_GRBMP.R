#P.Reap 6/23 Length frequency histograms representing various creeks and focal species taken from GRBMP sites 

           ##### RUN DATABSE_WRANGLING_SCRIPT BEFORE RUNNING THIS SCRIPT####
           
#Run Database_Wrangling_Script first
if(exists("Catch")==FALSE){
  if(file.exists("GRBMPReporting.RData")){
    load("GRBMPReporting.RData")
  }
  else{source("Database_Wrangling_Script.R")}
}
          
# Loads packages function and other lab functions
packages(ggplot2)
packages(dplyr)




#####Length frequency histograms#####

HotSpringsTICO <- Catch %>%
  filter(Water_Name == 'Hot Springs Canyon', CODE == "TICO") %>%
  select(Water_Name, SurveyYear, CODE, Fish_Length)
HotSpringsTICO <- rbind(HotSpringsTICO, HSTICOBefore21)

HSTICOblockA <- HotSpringsTICO %>%
  filter(SurveyYear >= "2019")
HSTICOblockb <- HotSpringsTICO %>%
  filter(SurveyYear < "2019")
HotSpringsTICOSum <- HotSpringsTICO %>%
  group_by(SurveyYear) %>%
  dplyr::summarize(n=n())

#Last 5 Years
TLHotspringsTICOA <- ggplot(HSTICOblockA, aes(x=Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=5) +
  scale_x_continuous(name = "Total Length (mm)", seq(25,80,5))+
  scale_y_continuous(name="Count",seq (0,50,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))+
  facet_grid(rows= vars(SurveyYear), scales='fixed') 
TLHotspringsTICOA
#Before 2019 data
TLHotspringsTICOB <- ggplot(HSTICOblockb, aes(x=Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=5) +
  scale_x_continuous(name = "Total Length (mm)", seq(25,80,5))+
  scale_y_continuous(name="Count",seq (0,50,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))+
  facet_grid(rows= vars(SurveyYear), scales='fixed') 
TLHotspringsTICOB

hotspringsGIIN <- Catch %>%
  filter(Water_Name == 'Hot Springs Canyon', CODE == "GIIN") %>%
  select(Water_Name, SurveyYear, CODE, Fish_Length)

TLHotspringsGIIN <- ggplot(hotspringsGIIN, aes(x=Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=5) +
  scale_x_continuous(name = "Total Length (mm)", seq(0,300,20))+
  scale_y_continuous(name="Count",seq (0,50,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))+
  facet_grid(rows= vars(SurveyYear), scales='fixed') 
TLHotspringsGIIN

#### Lower Blue Length frequency graphs ######

LBRMEFU <- Catch %>%
  filter(Water_Name == 'Lower Blue River'| Water_Name == 'Lower Blue River (Fritz Ranch to Confluence)', CODE == 'MEFU') %>%
  select(Water_Name, SurveyYear, CODE, Fish_Length)
LBRMEFU <- rbind(LBRMEFU, LBRMEFUBefore21)

LBRMEFUblockA <- LBRMEFU %>%
  filter(SurveyYear >= "2019")
LBRMEFUblockB <- LBRMEFU %>%
  filter(SurveyYear < "2019")
#Most recent 5 years
TLLowerBlueMEFUA <- ggplot(LBRMEFUblockA, aes(Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=5) +
  scale_x_continuous(name = "Total Length (mm)", limits = c(25,75), seq(25,85,5)) +
  scale_y_continuous(name="Count",seq (0,200,50))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14)) +
  facet_grid(rows= vars(SurveyYear), scales='fixed')
TLLowerBlueMEFUA
#Before 2019 data
TLLowerBlueMEFUB <- ggplot(LBRMEFUblockB, aes(Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=5) +
  scale_x_continuous(name = "Total Length (mm)", limits = c(25,75), seq(25,85,5)) +
  scale_y_continuous(name="Count",seq (0,200,50))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14)) +
  facet_grid(rows= vars(SurveyYear), scales='fixed')
TLLowerBlueMEFUB

##GIIN length freq##
GIINTL <- Catch %>%
  filter(CODE == "GIIN") %>%
  select(Water_Name, SurveyYear, Fish_Length, Fish_Weight, Gear_Type)

WalkerLF <- GIINTL %>%
  filter(Water_Name == "Walker Canyon") #### Walker Canyon GILA CHUB LF ###

WalkerLength <- ggplot(WalkerLF, aes(Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=10) +
  scale_x_continuous(name = "Total Length (mm)", seq(20,240,20))+
  scale_y_continuous(name="Count",seq (0,35,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))
WalkerLength

SheehyTL <- GIINTL %>%
  filter(Water_Name == "Sheehy Spring")        ### Sheehy Spring GILA CHUB LF ####

SheehyGIIN <- ggplot(SheehyTL, aes(x=Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=10) +
  scale_x_continuous(name = "Total Length (mm)", seq(20,240,20))+
  scale_y_continuous(name="Count",seq (0,35,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))+
  facet_grid(rows= vars(SurveyYear), scales='fixed')

SheehyGIIN

SheehySum <- SheehyTL %>%
  group_by(SurveyYear) %>%
  dplyr::summarise(count=n())

DixCreekGIIN <- GIINTL %>%
  filter(Water_Name == "Dix Creek")  ### Dix Creek GIIN ###

DixGIINTL <- ggplot(DixCreekGIIN, aes(x=Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=10) +
  scale_x_continuous(name = "Total Length (mm)", seq(20,240,20))+
  scale_y_continuous(name="Count",seq (0,75,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))+
  facet_grid(rows= vars(SurveyYear), scales='fixed')
DixGIINTL

DIXGIINLW <- ggplot(DixCreekGIIN, aes(Fish_Length, Fish_Weight)) +
  geom_point() +
  geom_smooth()
DIXGIINLW

HardenCienegaGIIN <- GIINTL %>%
  filter(Water_Name == "Harden Cienega Creek")   #### Harden Cienega GILA CHUB ###

HCGIINTL <- ggplot(HardenCienegaGIIN, aes(x=Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=10) +
  scale_x_continuous(name = "Total Length (mm)", seq(20,240,20))+
  scale_y_continuous(name="Count",seq (0,35,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))+
  facet_grid(rows= vars(SurveyYear), scales='fixed')
HCGIINTL

BassCanyonGIIN <- GIINTL %>%
  filter(Water_Name == "Bass Canyon", Gear_Type == "Backpack Electrofisher")     #### Bass Canyon GILA CHUB ####

BassGIINTL <- ggplot(BassCanyonGIIN, aes(x=Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=10) +
  scale_x_continuous(name = "Total Length (mm)", seq(20,240,20))+
  scale_y_continuous(name="Count",seq (0,75,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))+
  facet_grid(rows= vars(SurveyYear), scales='fixed')
BassGIINTL

BASSCanyonSummary <- BassCanyonGIIN %>%
  group_by(SurveyYear) %>%
  summarise(Catch = n()) 

RomeroTL <- GIINTL %>%
  filter(Water_Name == "Romero Canyon")        ### Romero Canyon GILA CHUB LF ####

RomeroGIIN <- ggplot(RomeroTL, aes(x=Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=10) +
  scale_x_continuous(name = "Total Length (mm)", seq(20,240,20))+
  scale_y_continuous(name="Count",seq (0,50,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))+
  facet_grid(rows= vars(SurveyYear), scales='fixed')

RomeroGIIN

SabinoTL <- GIINTL %>%
  filter(Water_Name == "Sabino Canyon")        ### Sabino Spring GILA CHUB LF ####

SabinoGIIN <- ggplot(SabinoTL, aes(x=Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=10) +
  scale_x_continuous(name = "Total Length (mm)", seq(20,240,20))+
  scale_y_continuous(name="Count",seq (0,60,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))+
  facet_grid(rows= vars(SurveyYear), scales='fixed')

SabinoGIIN


###Apache Trout TL###
ONAPTL <- Catch %>%
  filter(CODE == "ONAP") %>%
  select(Water_Name, SurveyYear, Fish_Length, Fish_Weight)

GrantONAPTL <- ggplot(ONAPTL, aes(x=Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=10) +
  scale_x_continuous(name = "Total Length (mm)", seq(20,280,20))+
  scale_y_continuous(name="Count",seq (0,35,10))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14))+
  facet_grid(rows= vars(SurveyYear), scales='fixed')

GrantONAPTL

ONAPSum <- ONAPTL %>%
  group_by(SurveyYear) %>%
  dplyr::summarise(count = n())

#### TICO Only ####
TICOTL <- Catch %>%
  filter(CODE == "TICO") %>%
  select(Water_Name, SurveyYear, Fish_Length)

BearCreekTL <- TICOTL %>%
  filter(Water_Name == "Bear Creek") 

BearCreekTICO <- ggplot(BearCreekTL, aes(Fish_Length)) +
  geom_histogram(color="black", fill="grey", binwidth=5) +
  scale_x_continuous(name = "Total Length (mm)", limits = c(25,75), seq(25,85,5)) +
  scale_y_continuous(name="Count",seq (0,200,50))+
  scale_fill_grey()+
  theme_bw () +
  theme(text=element_text(size=14)) +
  facet_grid(rows= vars(SurveyYear), scales='fixed')

BearCreekTICO

