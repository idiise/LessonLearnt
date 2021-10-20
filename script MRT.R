# Import de package -------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)

# https://wfp-vam.github.io/RBD_FS_CH_guide_FR/lindice-r%C3%A9duit-des-strat%C3%A9gies-de-survie-rcsi.html
# import des Différents ---------------------------------------------------
setwd("C:/Users/DELL/Pictures/Dossier R/lessonslearnt2021/Mrt/")
List_bases <- dir_ls(regexp = ".sav") %>% 
  map(read_sav)
nom_base <- names(List_bases)
nom_base <- str_replace(string = nom_base,pattern = ".sav", replacement = "")
names(List_bases) <- nom_base
list2env(List_bases, .GlobalEnv)

# Codebook base ménage
data_2018 <- to_factor(data_2018)
codebook_MRT_2018 <- var_label(data_2018)
codebook_MRT_2018 <- as.data.frame(do.call(rbind,codebook_MRT_2018))
codebook_MRT_2018 <- codebook_MRT_2018 %>% rownames_to_column()

write_xlsx(codebook_MRT_2018, "C:/Users/DELL/Pictures/Dossier R/lessonslearnt2021/Codebook/codebook_MRT_2018.xlsx")

table(data_2018$FCG)
funModeling::freq(data_2018, "FCG")

# Pauvre 84.59
# limite 15.41


# Lhcs MRT 2018 -----------------------------------------------------------

funModeling::freq(data_2018, "lcsi")
# stress =  35.18
# crise = 19.29
# urgences = 33.41
# neutres = 12.12

# Partie SCA MRT 2019 -----------------------------------------------------

# Codebook base ménage
Endline_ECHO_clean_final_2019 <- to_factor(Endline_ECHO_clean_final_2019)
codebook_MRT_2019 <- var_label(Endline_ECHO_clean_final_2019)
codebook_MRT_2019 <- as.data.frame(do.call(rbind,codebook_MRT_2019))
codebook_MRT_2019 <- codebook_MRT_2019 %>% rownames_to_column()

write_xlsx(codebook_MRT_2019, "C:/Users/DELL/Pictures/Dossier R/lessonslearnt2021/Codebook/codebook_MRT_2019.xlsx")

table(Endline_ECHO_clean_final_2019)

data_Agricole_RMS2020 <- data_Agricole_RMS2020 %>% 
  mutate(
    FCSStap = fcs_calc_1, # Céréale et tubercules
    FCSPulse = fcs_calc_5, #Légumineuse/noix
    FCSVeg = fcs_calc_2, # Sommer pour avoir légumes,
    FCSPr = fcs_calc_4,  # Sommer pour avoir protéines
    FCSDairy = fcs_calc_6,
    FCSSugar = fcs_calc_8,
    FCSFat = fcs_calc_7,
    FCSFruit = fcs_calc_3
  ) %>% mutate(
    FCSFruit = ifelse(FCSFruit > 7 , 7 , FCSFruit),
    FCSVeg = ifelse(FCSVeg > 7 , 7, FCSVeg),
    FCSPr = ifelse(FCSPr > 7 , 7, FCSPr),
    FCSStap = ifelse(FCSStap > 7 , 7, FCSStap),
    FCSPulse = ifelse(FCSPulse > 7 , 7, FCSPulse),
    FCSDairy = ifelse(FCSDairy > 7 , 7, FCSDairy),
    FCSSugar = ifelse(FCSSugar > 7 , 7, FCSSugar),
    FCSFat = ifelse(FCSFat > 7 , 7, FCSFat),
    
  )

# Partie lhcs MRT 2019 ----------------------------------------------------

funModeling::freq(Endline_ECHO_clean_final_2019, "lcsi")

# stress =  55.71
# crise = 20.49
# urgences = 8.19
# neutres = 15.61


# Partie SCA MRT 2020 -----------------------------------------------------
data_2020 <- to_factor(data_2020)
codebook_MRT_2020 <- var_label(data_2020)
codebook_MRT_2020 <- as.data.frame(do.call(rbind,codebook_MRT_2020))
codebook_MRT_2020 <- codebook_MRT_2020 %>% rownames_to_column()

write_xlsx(codebook_MRT_2020, "C:/Users/DELL/Pictures/Dossier R/lessonslearnt2021/Codebook/codebook_MRT_2020.xlsx")

table(data_2020$classe_fcs_index)
funModeling::freq(data_2020, "classe_fcs_index")

# Pauvre 17.86
# limite 13.77
# Acceptable 68.37

# Partie Lhcs MRT 2020 ----------------------------------------------------

funModeling::freq(data_2020, "lcsi")

# stress =  48.96
# crise = 8.06
# urgences = 12.16
# neutres = 30.83


# Partie SCA pdm MRT 2021 -----------------------------------------------------

data_2021 <- read_sav("PDM1_Soudure_2021.sav")
# Partie SCA MRT 2021 -----------------------------------------------------
data_2021 <- to_factor(data_2021)
codebook_MRT_2021 <- var_label(data_2021)
codebook_MRT_2021 <- as.data.frame(do.call(rbind,codebook_MRT_2021))
codebook_MRT_2021 <- codebook_MRT_2021 %>% rownames_to_column()

write_xlsx(codebook_MRT_2021, "C:/Users/DELL/Pictures/Dossier R/lessonslearnt2021/Codebook/codebook_MRT_2021.xlsx")

table(data_2021$SCAG_grp)
funModeling::freq(data_2021, "SCAG_grp")

# Pauvre 30
# limite 20.42
# Acceptable 49.58


# Partie Lhcs MRT 2021 ----------------------------------------------------

funModeling::freq(data_2021, "lcsi_grp")

# stress =  51.33
# crise = 25.75
# urgences = 10.63
# neutres = 12.08

# PDM Soudure 2018 SCA ----------------------------------------------------

PDM1_Soudure_2018 <- read_sav("PDM_SOUDURE_2018.sav")
PDM1_Soudure_2018 <- to_factor(PDM1_Soudure_2018)
codebook_PDM_MRT_2018 <- var_label(PDM1_Soudure_2018)
codebook_PDM_MRT_2018 <- as.data.frame(do.call(rbind,codebook_PDM_MRT_2018))
codebook_PDM_MRT_2018 <- codebook_PDM_MRT_2018 %>% rownames_to_column()

write_xlsx(codebook_PDM_MRT_2018, "C:/Users/DELL/Pictures/Dossier R/lessonslearnt2021/Codebook/codebook_PDM_MRT_2018.xlsx")

PDM1_Soudure_2018 <- PDM1_Soudure_2018 %>% 
  mutate(
    FCSStap = Q11_3_1, # Céréale et tubercules
    FCSPulse = Q11_3_2, #Légumineuse/noix
    FCSVeg = Q11_3_3, # Sommer pour avoir légumes,
    FCSPr = Q11_3_5 ,  # Sommer pour avoir protéines
    FCSDairy = Q11_3_6,# Laits 
    FCSSugar = Q11_3_8, # Sucres
    FCSFat = Q11_3_7, #cond
    FCSFruit = Q11_3_4 #Fruits
  ) %>% mutate(
    FCSFruit = ifelse(FCSFruit > 7 , 7 , FCSFruit),
    FCSVeg = ifelse(FCSVeg > 7 , 7, FCSVeg),
    FCSPr = ifelse(FCSPr > 7 , 7, FCSPr),
    FCSStap = ifelse(FCSStap > 7 , 7, FCSStap),
    FCSPulse = ifelse(FCSPulse > 7 , 7, FCSPulse),
    FCSDairy = ifelse(FCSDairy > 7 , 7, FCSDairy),
    FCSSugar = ifelse(FCSSugar > 7 , 7, FCSSugar),
    FCSFat = ifelse(FCSFat > 7 , 7, FCSFat)
    
  )

PDM1_Soudure_2018 <- PDM1_Soudure_2018 %>% 
  mutate( FCS = (2*FCSStap) + (3*FCSPulse) + FCSVeg + FCSFruit + (4*FCSPr) + (4*FCSDairy) + (0.5*FCSFat) + (0.5*FCSSugar))

PDM1_Soudure_2018 <- PDM1_Soudure_2018 %>% 
  mutate(FCSCat28 = case_when(
    FCS <= 28 ~ "Poor", 
    between(FCS, 28.5, 42) ~ "Borderline", 
    FCS > 42 ~ "Acceptable"
  ))

funModeling::freq(PDM1_Soudure_2018, "FCSCat28") 
# Acceptable = 82.38
# Limite = 12.14
# Pauvre = 5.48