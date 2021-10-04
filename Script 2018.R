# Import de package -------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)


# Partie SCA PDM1 2018 ----------------------------------------------------

# Codebook base ménage
BasePDM1CoinjointSoudure2018_Menage<- to_factor(BasePDM1CoinjointSoudure2018_Menage)
codebook_PDM12018 <- var_label(BasePDM1CoinjointSoudure2018_Menage)
codebook_PDM12018 <- as.data.frame(do.call(rbind,codebook_PDM12018))
codebook_PDM12018 <- codebook_PDM12018 %>% rownames_to_column()

write_xlsx(codebook_PDM12018, "Codebook/codebook_PDM12018.xlsx")

BasePDM1CoinjointSoudure2018_Menage <- BasePDM1CoinjointSoudure2018_Menage %>% 
  mutate(
    caa2 = replace_na(caa2, 0), #céréales
    FCSPulse = replace_na(cac2, 0), #Légumineuses/noix
    cab2 = replace_na(cab2, 0), #tubercules
    FCSVegOrg = replace_na(cad2, 0), #légumes oranges
    FCSVegGre = replace_na(cae2, 0), #légumes vertes
    FCSVegOth = replace_na(caf2, 0), # légumes autres
    FCSFruitOrg = replace_na(cag2, 0), #Fruit orange
    FCSFruitOth = replace_na(cah2, 0), #Autres fruits
    FCSPrMeatF = replace_na(cai2, 0), #Viande
    FCSPrMeatO = replace_na(caj2, 0), #abats
    FCSPrFish = replace_na(cak2, 0), # Poissons
    FCSPrEgg = replace_na(cal2, 0), #Oeufs
    FCSDairy = replace_na(cam2, 0), #Lait
    FCSSugar = replace_na(cao2, 0), #Sucre
    FCSFat = replace_na(can2, 0), #Huile
    FCSCond = replace_na(cap2, 0) #Condiment
  )  
BasePDM1CoinjointSoudure2018_Menage <- BasePDM1CoinjointSoudure2018_Menage %>% 
  mutate(
    FCStap = caa2 + cab2, # Céréale et tubercules
    FCSPulse = FCSPulse, #Légumineuse/noix
    FCSVeg = FCSVegOrg + FCSVegGre + FCSVegOth, # Sommer pour avoir légumes,
    FCSPr = FCSPrMeatF + FCSPrMeatO + FCSPrFish + FCSPrEgg,  # Sommer pour avoir protéines
    FCSDairy = FCSDairy,
    FCSSugar = FCSSugar,
    FCSCond = FCSCond,
    FCSFruit = FCSFruitOrg + FCSFruitOth
  ) %>% mutate(
    FCSFruit = ifelse(FCSFruit > 7 , 7 , FCSFruit),
    FCSVeg = ifelse(FCSVeg > 7 , 7, FCSVeg),
    FCSPr = ifelse(FCSPr > 7 , 7, FCSPr),
    FCStap = ifelse(FCStap > 7 , 7, FCStap)
  )

BasePDM1CoinjointSoudure2018_Menage <- BasePDM1CoinjointSoudure2018_Menage %>% 
  mutate( FCS = (2*FCStap) + (3*FCSPulse) + FCSVeg + FCSFruit + (4*FCSPr) + (4*FCSDairy) + (0.5*FCSFat) + (0.5*FCSSugar))

BasePDM1CoinjointSoudure2018_Menage <- BasePDM1CoinjointSoudure2018_Menage %>% 
  mutate(FCSCat28 = case_when(
    FCS <= 28 ~ "Poor", 
    between(FCS, 28.5, 42) ~ "Borderline", 
    FCS > 42 ~ "Acceptable"
  ))

funModeling::freq(BasePDM1CoinjointSoudure2018_Menage, "FCSCat28") 
# Acceptable = 46.73
# Limite =  30.95
# Pauvre = 22.33