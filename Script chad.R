# Import de package -------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)

# https://wfp-vam.github.io/RBD_FS_CH_guide_FR/lindice-r%C3%A9duit-des-strat%C3%A9gies-de-survie-rcsi.html
# import des Différents ---------------------------------------------------
setwd("C:/Users/DELL/Pictures/Dossier R/lessonslearnt2021/Tchad/")
List_bases <- dir_ls(regexp = ".sav") %>% 
  map(read_sav)
nom_base <- names(List_bases)
nom_base <- str_replace(string = nom_base,pattern = ".sav", replacement = "")
names(List_bases) <- nom_base
list2env(List_bases, .GlobalEnv)

# Codebook base ménage
BdD_Baseline_Batha<- `BdD Baseline Batha`
BdD_Baseline_Batha <- to_factor(BdD_Baseline_Batha)
codebook_Batha <- var_label(BdD_Baseline_Batha)
codebook_Batha <- as.data.frame(do.call(rbind,codebook_Batha))
codebook_Batha <- codebook_Batha %>% rownames_to_column()

write_xlsx(codebook_Batha, "C:/Users/DELL/Pictures/Dossier R/lessonslearnt2021/Codebook/codebook_Bathe.xlsx")

table(BdD_Baseline_Batha$jour_conso_cereales_tubercules)


# lhcs Batha --------------------------------------------------------------
#  stratégies de stress
BdD_Baseline_Batha <- BdD_Baseline_Batha %>% mutate( stress_coping =
                                                             case_when(
                                                               LhCSIStress1 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                               LhCSIStress2 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                               LhCSIStress3 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                               LhCSIStress4 %in% c("Oui","Non, parce que j’ai déjà vendu ces actifs ou mené cette activité au cours des 12 derniers mois et je ne peux pas c") ~ "Oui",
                                                               TRUE ~  "Non"
                                                             ))

#  stratégies de crise
BdD_Baseline_Batha <- BdD_Baseline_Batha %>% mutate(crisis_coping = 
                                                    case_when(
                                                      LhCSICrisis1 %in% c("Oui") ~ "Oui",
                                                      LhCSICrisis2 %in% c("Oui") ~ "Oui",
                                                      LhCSICrisis3 %in% c("Oui") ~ "Oui",
                                                      TRUE ~ "Non"
                                                    )
)
#  stratégies d'urgence
BdD_Baseline_Batha <- BdD_Baseline_Batha %>% mutate(emergency_coping =
                                                    case_when(
                                                      LhCSIEmergency1 %in% c("Oui") ~ "Oui",
                                                      LhCSIEmergency2 %in% c("Oui") ~ "Oui",
                                                      LhCSIEmergency3 %in% c("Oui") ~ "Oui",
                                                      TRUE ~ "Non"
                                                    )
)

BdD_Baseline_Batha <- BdD_Baseline_Batha %>% mutate(LhCSICat = case_when(
  emergency_coping == "Oui" ~ "StrategiesdeUrgence",
  crisis_coping == "Oui" ~ "StrategiesdeCrise",
  stress_coping == "Oui" ~ "StrategiesdeStress",
  TRUE ~ "Pasdestrategies"))

funModeling::freq(BdD_Baseline_Batha, "LhCSICat")

# stress =  35.82
# crise = 31.84
# urgences = 21.89
# neutres = 10.45

