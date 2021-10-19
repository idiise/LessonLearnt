# Import de package -------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)

# Partie SCA Pastorale 2021 -----------------------------------------------

# Codebook base ménage
dataPastorale2021<- to_factor(dataPastorale2021)
codebook_PDM1_2021 <- var_label(dataPastorale2021)
codebook_PDM1_2021 <- as.data.frame(do.call(rbind,codebook_PDM1_2021))
codebook_PDM1_2021 <- codebook_PDM1_2021 %>% rownames_to_column()

write_xlsx(codebook_PDM1_2021, "Codebook/codebook_PDM1_2021.xlsx")

dataPastorale2021 <- dataPastorale2021 %>% 
  mutate( FCS = (2*FCSStap) + (3*FCSPulse) + FCSVeg + FCSFruit + (4*FCSPr) + (4*FCSDairy) + (0.5*FCSFat) + (0.5*FCSSugar))

dataPastorale2021 <- dataPastorale2021 %>% 
  mutate(FCSCat28 = case_when(
    FCS <= 28 ~ "Poor", 
    between(FCS, 28.5, 42) ~ "Borderline", 
    FCS > 42 ~ "Acceptable"
  ))

funModeling::freq(dataPastorale2021, "FCSCat28") 

# Acceptable = 50.25
# Limite = 30.35
# Pauvre = 19.4

# Partie Stratégies d'adaptation aux moyens d'existence LHCS Pastorale -------------------

#  stratégies de stress
dataPastorale2021 <- dataPastorale2021 %>% mutate( stress_coping =
                                                                             case_when(
                                                                               LhCSIStress1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                                               LhCSIStress2 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                                               LhCSIStress3 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                                               LhCSIStress4 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                                               TRUE ~  "Non"
                                                                             ))

#  stratégies de crise
dataPastorale2021 <- dataPastorale2021 %>% mutate(crisis_coping = 
                                                                            case_when(
                                                                              LhCSICrisis1 %in% c("Oui") ~ "Oui",
                                                                              LhCSICrisis2 %in% c("Oui") ~ "Oui",
                                                                              LhCSICrisis3 %in% c("Oui") ~ "Oui",
                                                                              TRUE ~ "Non"
                                                                            )
)
#  stratégies d'urgence
dataPastorale2021 <- dataPastorale2021 %>% mutate(emergency_coping =
                                                                            case_when(
                                                                              LhCSIEmergency1 %in% c("Oui") ~ "Oui",
                                                                              LhCSIEmergency2 %in% c("Oui") ~ "Oui",
                                                                              LhCSIEmergency3 %in% c("Oui") ~ "Oui",
                                                                              TRUE ~ "Non"
                                                                            )
)

dataPastorale2021 <- dataPastorale2021 %>% mutate(LhCSICat = case_when(
  emergency_coping == "Oui" ~ "StrategiesdeUrgence",
  crisis_coping == "Oui" ~ "StrategiesdeCrise",
  stress_coping == "Oui" ~ "StrategiesdeStress",
  TRUE ~ "Pasdestrategies"))

funModeling::freq(dataPastorale2021, "LhCSICat")

# stress =  35.82
# crise = 31.84
# urgences = 21.89
# neutres = 10.45

# Partie SCA PDM2 ou agricole baseline 2021 ----------------------------------------------------
# Codebook base ménage
data_endline2021 <- read_sav("Data/menage_7Sep2021.sav")
data_endline2021<- to_factor(data_endline2021)
codebook_PDM22021 <- var_label(data_endline2021)
codebook_PDM22021 <- as.data.frame(do.call(rbind,codebook_PDM22021))
codebook_PDM22021 <- codebook_PDM22021 %>% rownames_to_column()

write_xlsx(codebook_PDM22021, "Codebook/codebook_PDM22021.xlsx")

data_endline2021 <- data_endline2021 %>% 
  mutate( FCS = (2*FCSStap) + (3*FCSPulse) + FCSVeg + FCSFruit + (4*FCSPr) + (4*FCSDairy) + (0.5*FCSFat) + (0.5*FCSSugar))

data_endline2021 <- data_endline2021 %>% 
  mutate(FCSCat28 = case_when(
    FCS <= 28 ~ "Poor", 
    between(FCS, 28.5, 42) ~ "Borderline", 
    FCS > 42 ~ "Acceptable"
  ))

funModeling::freq(data_endline2021, "FCSCat28") 

# Acceptable = 59.5
# Limite = 39.5
# Pauvre = 1


# Partie Lhcs endline PDM2 2021 --------------------------------------------------------


