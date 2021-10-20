
# Import de package -------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)

# https://wfp-vam.github.io/RBD_FS_CH_guide_FR/lindice-r%C3%A9duit-des-strat%C3%A9gies-de-survie-rcsi.html
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
    FCSDairy = FCSDairy,# Laits 
    FCSSugar = FCSSugar, # Sucres
    FCSCond = FCSCond, #cond
    FCSFruit = FCSFruitOrg + FCSFruitOth #Fruits
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
# Acceptable = 38.47
# Limite = 26.94
# Pauvre = 34.59

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

# stress =  11.88
# crise = 0.43
# urgences = 15.96
# neutres = 71.73

# Partie rCSI PDM1 --------------------------------------------------------

Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% mutate(
  ss1a= replace_na(ss1a, 0),
  ss1b= replace_na(ss1b, 0),
  ss1c= replace_na(ss1c, 0),
  ss1d= replace_na(ss1d, 0),
  ss1e= replace_na(ss1e, 0)
)

Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% mutate(
  rCSI = (ss1a + (2*ss1b) + ss1c + (3*ss1d) + ss1e)
)

Base_menagePDM1_Conjoint_2017 <- Base_menagePDM1_Conjoint_2017 %>% mutate(
  rCSI = case_when(
    rCSI <= 3 ~ "Entre 0 et 3",
    between(rCSI, 4,18)  ~ "Entre 4 et 18",
    rCSI >= 18 ~ "Plus de 18"
  )
)

funModeling::freq(Base_menagePDM1_Conjoint_2017$rCSI)

# Entre 0 et 3 = 99.06
# Entre 4 et 18 = 0.78
# Plus de 18 = 0.16


# Partie SCA PDM2 2017 ----------------------------------------------------
# Codebook base ménage
Base_MenagePDM2_Conjoint_2017<- to_factor(Base_MenagePDM2_Conjoint_2017)
codebook_PDM22017 <- var_label(Base_MenagePDM2_Conjoint_2017)
codebook_PDM22017 <- as.data.frame(do.call(rbind,codebook_PDM22017))
codebook_PDM22017 <- codebook_PDM22017 %>% rownames_to_column()

 write_xlsx(codebook_PDM22017, "Codebook/codebook_PDM22017.xlsx")
Base_MenagePDM2_Conjoint_2017 <- Base_MenagePDM2_Conjoint_2017 %>% 
  mutate(
    caa2 = replace_na(caa2, 0),
    cab2 = replace_na(cab2, 0),
    cac2 = replace_na(cac2, 0),
    cad2 = replace_na(cad2, 0),
    cae2 = replace_na(cae2, 0),
    caf2 = replace_na(caf2, 0),
    cag2 = replace_na(cag2, 0),
    cah2 = replace_na(cah2, 0),
    cai2 = replace_na(cai2, 0),
    caj2 = replace_na(caj2, 0),
    cak2 = replace_na(cak2, 0),
    cal2 = replace_na(cal2, 0),
    cam2 = replace_na(cam2, 0),
    can2 = replace_na(can2, 0),
    cao2 = replace_na(cao2, 0),
    cap2 = replace_na(cap2, 0)
    
  )

Base_MenagePDM2_Conjoint_2017 <- Base_MenagePDM2_Conjoint_2017 %>% 
  mutate(
    FCSPulse = cac2,#légumineuse
    FCStap = caa2 + cab2, # céréales plus tubercules
    FCSVegOrg = cad2,
    FCSVegGre = cae2,
    FCSVegOth = caf2,
    FCSFruitOrg = cag2,
    FCSFruitOth = cah2,
    FCSPrMeatF = cai2,
    FCSPrFish = cak2,
    FCSPrEgg = cal2,
    FCSPrMeatO = caj2,
    FCSDairy = cam2,
    FCSSugar = cao2,
    FCSFat = can2
  )

Base_MenagePDM2_Conjoint_2017 <- Base_MenagePDM2_Conjoint_2017 %>% mutate(
  FCSPulse = FCSPulse,
  FCStap = FCStap,
  FCSfVeg = (FCSVegOrg + FCSVegGre + FCSVegOth),
  FCSFruit = (FCSFruitOrg + FCSFruitOth),
  FCSPr  = (FCSPrMeatF + FCSPrFish + FCSPrEgg + FCSPrMeatO),
  FCSDairy = FCSDairy,
  FCSSugar = FCSSugar,
  FCSFat = FCSFat
)

Base_CommunautairePDM2_Conjoint_2017 <- Base_MenagePDM2_Conjoint_2017 %>% mutate(
  FCSVeg = ifelse(FCSfVeg > 7, 7, FCSfVeg),
  FCSFruit = ifelse(FCSFruit > 7, 7,FCSFruit ),
  FCStap = ifelse(FCStap > 7, 7,FCStap),
  FCSPr = ifelse(FCSPr > 7, 7,FCSPr)
)

Base_MenagePDM2_Conjoint_2017 <- Base_MenagePDM2_Conjoint_2017 %>% mutate(
  FCS = (2*FCStap) + (3*FCSPulse) + FCSfVeg + FCSFruit + (4*FCSPr) + (4*FCSDairy) + (0.5*FCSSugar) + (0.5*FCSFat)
)

Base_MenagePDM2_Conjoint_2017 <- Base_MenagePDM2_Conjoint_2017 %>% mutate(
  FCSCat28 = case_when(
    FCS <= 28 ~ "Poor", 
    between(FCS, 28.5, 42) ~ "Borderline", 
    FCS > 42 ~ "Acceptable")
)

funModeling::freq(Base_MenagePDM2_Conjoint_2017$FCSCat28)

# Acceptable        69.13           
# Borderline        19.69           
# Poor              11.19
#

# Partie Lhcs PDM2 2017 ---------------------------------------------------

#  stratégies de stress
Base_MenagePDM2_Conjoint_2017 <- Base_MenagePDM2_Conjoint_2017 %>% mutate( stress_coping =
                                                                             case_when(
                                                                               ss9x1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                                               ss13x1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                                               ss15x1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                                               ss17x1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                                               ss19x1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                                               TRUE ~  "Non"
                                                                             ))

#  stratégies de crise
Base_MenagePDM2_Conjoint_2017 <- Base_MenagePDM2_Conjoint_2017 %>% mutate(crisis_coping = 
                                                                            case_when(
                                                                              ss10x1 %in% c("Oui") ~ "Oui",
                                                                              ss11x1 %in% c("Oui") ~ "Oui",
                                                                              ss12x1 %in% c("Oui") ~ "Oui",
                                                                              TRUE ~ "Non"
                                                                            )
)
#  stratégies d'urgence
Base_MenagePDM2_Conjoint_2017 <- Base_MenagePDM2_Conjoint_2017 %>% mutate(emergency_coping =
                                                                            case_when(
                                                                              ss8x1 %in% c("Oui") ~ "Oui",
                                                                              ss14x1 %in% c("Oui") ~ "Oui",
                                                                              ss16x1 %in% c("Oui") ~ "Oui",
                                                                              ss18x1 %in% c("Oui") ~ "Oui",
                                                                              TRUE ~ "Non"
                                                                            )
)

Base_MenagePDM2_Conjoint_2017 <- Base_MenagePDM2_Conjoint_2017 %>% mutate(LhCSICat = case_when(
  emergency_coping == "Oui" ~ "StrategiesdeUrgence",
  crisis_coping == "Oui" ~ "StrategiesdeCrise",
  stress_coping == "Oui" ~ "StrategiesdeStress",
  TRUE ~ "Pasdestrategies"))

funModeling::freq(Base_MenagePDM2_Conjoint_2017, "LhCSICat")
# Pasdestrategies            90.16           
# StrategiesdeStress          5.56           
# StrategiesdeUrgence         4.06           
# StrategiesdeCrise           0.22          