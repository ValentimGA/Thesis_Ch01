################################################################################
# Script for Valentim et al. 2025
# Authors: Gabriela Valentim (valentim.gabriela@gmail.com), 
#          Leonardo Mesquita Pinto (leopinto.ca@gmail.com)
# Publication's Authors: Valentim GA, Pinto LM, Gurgel-Lourenço RC, 
#                        Rodrigues-Filho CA, Sánchez-Botero JI
# Last script update: Mar 05th 2024
# Data: doi.org/10.1080/14772000.2023.2228314 / Suplemental tsab_a_2228314_sm7555.xlsx -> FishNE_database
#       https://doi.org/10.17031/1678 -> FEAS_database
################################################################################

# load libraries ---------------------------------------------------------------
library(openxlsx)
library(dplyr)
library(stringr)

# load dataset -----------------------------------------------------------------

FishNE_database <- read.xlsx("Rosa et al 2023 tsab.xlsx", na.strings = "NA")
FEAS_database <- read.xlsx("FEAS_database.xlsx", na.strings = "NA", sheet = 2)

# wrangle data -----------------------------------------------------------------

FishNE <- FishNE_database %>%
    select(Species) %>%
    mutate(Species = str_replace(Species, "\\s+$", ""))

FEAS <- FEAS_database %>%
    select(Species, FEAS, EUFG) %>%
    mutate(Species = str_replace(Species, "\\s+$", ""))

Fishes <- left_join(FishNE, FEAS, by = "Species")
    #filter(str_detect(EUFG, "DI|MM|MS"))
