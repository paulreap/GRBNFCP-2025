###### Developing script for standard weight equation of Gila Chub found in the Gila River Basin
###P. Reap; 4/25/2024

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

remove(Discharge, Habitat, HSTICOBefore21, LBRMEFUBefore21)
###### Length weight analysis####
##remove NAs from dataset

GIINTL <- Catch %>%
  filter(CODE == "GIIN") %>%
  select(Water_Name, SurveyYear, Fish_Length, Fish_Weight)

####Filter NA's, Decimals, and 0's
GIINLWanalysis <- GIINTL %>%        
  filter(!is.na(Fish_Weight), Fish_Length >= 100)

GIINLWanalysis <- GIINLWanalysis[GIINLWanalysis$Fish_Weight != 0, ]

GIINLWanalysis$Fish_Weight <- round(GIINLWanalysis$Fish_Weight)
###Table of sample size from each waterbody
SampleSizeTable <- GIINLWanalysis %>%
  dplyr::group_by(Water_Name) %>%
  dplyr::summarise(SampleSize = n())

#Equation and regression modelling

model <- lm(log10(Fish_Weight) ~ log10(Fish_Length) + Water_Name, data = GIINLWanalysis)

intercept <- coef(model)[1]  # Convert back from log scale
slope_length <- coef(model)[2]   # Slope for log10(Fish_Length)
slope_water <- coef(model)[3]    # Slope for Water_Name

# Print intercept and slopes
print(intercept)
print(slope_length)

summary(model)


# Fit a linear regression model without Water_Name and name it as model2
model2 <- lm(log10(Fish_Weight) ~ log10(Fish_Length), data = GIINLWanalysis)
intercept2 <- coef(model2)[1]  # Convert back from log scale
slope_length2 <- coef(model2)[2]   # Slope for log10(Fish_Length)

# Print intercept and slopes
print(intercept2)
print(slope_length2)

summary(model2)

GIINLWanalysis <- mutate(GIINLWanalysis, al = (intercept + (slope_length * log10(Fish_Length))))
GIINLWanalysis <- mutate(GIINLWanalysis, Ws = 10^al)
GIINLWanalysis <- mutate(GIINLWanalysis, Wr = (Fish_Weight/Ws)*100)

ggplot(data = GIINLWanalysis, aes(x = log10(Fish_Length), y = log10(Fish_Weight))) +
  geom_point() +
  geom_smooth(data = GIINLWanalysis, method = "lm", se = FALSE, aes(color = Water_Name), formula = y ~ x, method.args = list(family = "gaussian")) +
  labs(x = "log10(Length)", y = "log10(Weight)", title = "Log-Log Regression for Fish Populations") +
  scale_color_discrete(name = "Population")
