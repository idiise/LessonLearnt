
# Import de package -------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)

# import des Différents ---------------------------------------------------
setwd("C:/Users/DELL/Pictures/Dossier R/lessonslearnt2021/Data")
List_bases <- dir_ls(regexp = ".sav") %>% 
  map(read_sav)
nom_base <- names(List_bases)
nom_base <- str_replace(string = nom_base,pattern = ".sav", replacement = "")
names(List_bases) <- nom_base
list2env(List_bases, .GlobalEnv)




# Base 2017 ---------------------------------------------------------------


# Partie SCA --------------------------------------------------------------


# Codebook base ménage
Base_menagePDM1_Conjoint_2017<- to_factor(Base_menagePDM1_Conjoint_2017)
codebook_PDM12017 <- var_label(Base_menagePDM1_Conjoint_2017)
codebook_PDM12017 <- as.data.frame(do.call(rbind,codebook_PDM12017))
codebook_PDM12017 <- codebook_PDM12017 %>% rownames_to_column()

# write_xlsx(codebook_PDM12017, "codebook_PDM12017.xlsx")
# remplacer les données manquantes par zéro

Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% 
  mutate(
    caa2 = replace_na(caa2, 0),
    FCSPulse = replace_na(cac2, 0),
    cab2 = replace_na(cab2, 0),
    FCSVegOrg = replace_na(cad2, 0),
    FCSVegGre = replace_na(cae2, 0),
    FCSVegOth = replace_na(caf2, 0),
    FCSFruitOrg = replace_na(cag2, 0),
    FCSFruitOth = replace_na(cah2, 0),
    FCSPrMeatF = replace_na(cai2, 0),
    FCSPrMeatO = replace_na(caj2, 0),
    FCSPrFish = replace_na(cak2, 0),
    FCSPrEgg = replace_na(cal2, 0),
    FCSDairy = replace_na(cam2, 0),
    FCSSugar = replace_na(cao2, 0),
    FCSFat = replace_na(can2, 0),
    FCSCond = replace_na(cap2, 0)
  )  

Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% 
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

Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% 
  mutate( FCS = (2*FCStap) + (3*FCSPulse) + FCSVeg + FCSFruit + (4*FCSPr) + (4*FCSDairy) + (0.5*FCSFat) + (0.5*FCSSugar))

Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% 
  mutate(FCSCat28 = case_when(
    FCS <= 28 ~ "Poor", 
    between(FCS, 28.5, 42) ~ "Borderline", 
    FCS > 42 ~ "Acceptable"
  ))

funModeling::freq(Base_menagePDM1_Conjoint_2017, "FCSCat28")

# Partie Stratégies d'adaptation aux moyens d'existence LHCS -------------------

#  stratégies de stress
Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% mutate( stress_coping =
  case_when(
    ss9x1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
    ss13x1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
    ss15x1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
    ss17x1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
    ss19x1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
    TRUE ~  "Non"
  ))

#  stratégies de crise
Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% mutate(crisis_coping = 
  case_when(
    ss10x1 %in% c("Oui") ~ "Oui",
    ss11x1 %in% c("Oui") ~ "Oui",
    ss12x1 %in% c("Oui") ~ "Oui",
    TRUE ~ "Non"
  )
)
#  stratégies d'urgence
Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% mutate(emergency_coping =
   case_when(
     ss8x1 %in% c("Oui") ~ "Oui",
     ss14x1 %in% c("Oui") ~ "Oui",
     ss16x1 %in% c("Oui") ~ "Oui",
     ss18x1 %in% c("Oui") ~ "Oui",
     TRUE ~ "Non"
   )
)

Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% mutate(LhCSICat = case_when(
  emergency_coping == "Oui" ~ "StrategiesdeUrgence",
  crisis_coping == "Oui" ~ "StrategiesdeCrise",
  stress_coping == "Oui" ~ "StrategiesdeStress",
  TRUE ~ "Pasdestrategies"))

funModeling::freq(Base_menagePDM1_Conjoint_2017, "LhCSICat")

Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% select(-c(LhCSICat, emergency_coping, crisis_coping, stress_coping))
