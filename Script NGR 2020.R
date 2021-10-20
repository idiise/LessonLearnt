# Import de package -------------------------------------------------------

library(tidyverse)
library(fs)
library(haven)
library(here)
library(labelled)
library(writexl)

# Partie SCA agricole 2020 -----------------------------------------------

# Codebook base ménage
data_Agricole_RMS2020<- to_factor(data_Agricole_RMS2020)
codebook_PDM1_2020 <- var_label(data_Agricole_RMS2020)
codebook_PDM1_2020 <- as.data.frame(do.call(rbind,codebook_PDM1_2020))
codebook_PDM1_2020 <- codebook_PDM1_2020 %>% rownames_to_column()

write_xlsx(codebook_PDM1_2020, "Codebook/codebook_PDM1_2020.xlsx")

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

data_Agricole_RMS2020 <- data_Agricole_RMS2020 %>% 
  mutate( FCS = (2*FCSStap) + (3*FCSPulse) + FCSVeg + FCSFruit + (4*FCSPr) + (4*FCSDairy) + (0.5*FCSFat) + (0.5*FCSSugar))


