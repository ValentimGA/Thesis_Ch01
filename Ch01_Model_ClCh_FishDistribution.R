################################################################################
# Script for Valentim et al. 2024
# Authors: Gabriela Valentim (valentim.gabriela@gmail.com), 
#          Leonardo Mesquita Pinto (leopinto.ca@gmail.com )
# Publication's Authors: Valentim GA, Pinto LM, Sánchez-Botero JI
# Last script update: Jan 17th 2023
# Data: --
################################################################################

# load libraries --------------------------------------------------------------

library(rfishbase)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(rgbif)
library(sf)
devtools::install_github("liibre/Rocc")
library(Rocc)


# load dataset -----------------------------------------------------------------

fishNAMES <- common_names(species_list = NULL)
fishSPECIES <- species(species_list = NULL)
fishCOUNTRY <- countrysub(species_list = NULL)

# wrangle data -----------------------------------------------------------------

fishNAMES <- fishNAMES %>%
    select("SpecCode", "Species")

fishSPECIES <- fishSPECIES %>%
    select("SpecCode", "Fresh", "Brack", "Saltwater")

fishBase <- fishCOUNTRY %>%
    select("SpecCode", "C_Code", "CSub_Code", "Status", "country") %>%
    filter(C_Code == "076") %>%
    filter(CSub_Code %in% c("BR-MA", "BR-PI", "BR-CE","BR-RN","BR-PB",
                            "BR-PE", "BR-SE", "BR-AL", "BR-BA")) %>%
    mutate(CSub_Code = str_sub(CSub_Code, -2, -1)) %>%
    left_join(fishNAMES, by = "SpecCode") %>%
    left_join(fishSPECIES, by = "SpecCode") %>%
    filter(Brack == 1 & Status == "native")

fishes <- fishBase %>%
    select("SpecCode", "Species", "CSub_Code", "Status") %>%
    group_by(Species) %>%
    summarise(
        SpecCode = paste(unique(SpecCode), collapse = ", "),
        States = paste(unique(CSub_Code), collapse = ", "),
        Status = paste(unique(Status), collapse = ", ")
    ) %>%
    separate(States, into = c("MA", "PI", "CE", "RN", "PB", "PE", "SE",
                              "AL", "BA"), sep = ", ") %>%
    pivot_longer(cols = c(MA:BA), names_to = "State") %>%
    mutate(value = ifelse(!is.na(value), 1, 0)) %>%
    pivot_wider(names_from = "State", values_from = "value")

#####

# write.xlsx(fishes, "FishBase.xlsx", rowNames = FALSE)

#####

fishes_sp <- as.vector(fishes$Species)

#####

# Number of Occurrences --------------------------------------------------------

## GBIF
   
    gbif_taxon_keys <- fishes$Species %>% 
      name_backbone_checklist() %>%
        filter(!matchType == "NONE") %>%
        pull(usageKey) 

    get_occurence_GBIF <- occ_download(
        pred_in("taxonKey", gbif_taxon_keys),
        pred("hasCoordinate", TRUE),
        format = "SIMPLE_CSV",
        user = "valentim_gabriela", pwd = 'L2U"7{3K:115%ngC', 
        email = "valentim.gabriela@gmail.com"
       )
    
    protocol_number = head(get_occurence_GBIF)

    occ_download_wait(protocol_number)

    data_GBIF <- occ_download_get(protocol_number) %>%
        occ_download_import() %>%
        select(verbatimScientificName, decimalLatitude, decimalLongitude) %>%
        mutate(source = "GBIF")
    
    data_GBIF_sf = st_as_sf(data_GBIF, 
                            coords = c("decimalLongitude","decimalLatitude"), 
                            crs = st_crs(4326))

## SpeciesLink

    apply_rspeciesLink <- function(fish_sp) {
        rspeciesLink(filename = "speciesLink_occ",
                     species = fish_sp)
    }
    
    speciesLink <- lapply(fishes_sp, apply_rspeciesLink) %>%
        bind_rows()
    
    speciesLink <- speciesLink %>%
        filter(!is.na(decimalLongitude)) %>%
        filter(!is.na(decimalLatitude))
        
    speciesLink <-  speciesLink %>% 
        select("scientificName", "decimalLongitude", "decimalLatitude") %>%
        mutate(source = "speciesLink")
    
    speciesLink_sf <- st_as_sf(speciesLink,
                               coords = c("decimalLongitude","decimalLatitude"),
                               crs = st_crs(4326))
    
    