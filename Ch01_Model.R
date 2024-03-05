################################################################################
# Script for Valentim et al. 2024
# Authors: Gabriela Valentim (valentim.gabriela@gmail.com), 
#          Leonardo Mesquita Pinto (leopinto.ca@gmail.com )
# Publication's Authors: Valentim GA, Pinto LM, Sánchez-Botero JI
# Last script update: Feb 26th 2024
# Data: --
################################################################################

# load libraries ---------------------------------------------------------------

library(rfishbase)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(rgbif)
library(sf)
library(devtools)
# devtools::install_github("liibre/Rocc")
library(Rocc)
# install Rtools
# install GPG4win http://www.gpg4win.org/
# remotes::install_github("cran/rcrypt")
# remotes::install_github("raquamaps/aquamapsdata", dependencies = TRUE)
library(aquamapsdata)
    # download_db(force = T)
    default_db("sqlite")
library(RSQLite)
library(sqldf)
library(stringr)
library(purrr)
library(CoordinateCleaner)
library(stringdist)

# load dataset -----------------------------------------------------------------

fishSPECIES <- species(species_list = NULL)
fishNAMES <- common_names(species_list = NULL)
fishCOUNTRY <- countrysub(species_list = NULL)

# wrangle data -----------------------------------------------------------------

fishNAMES <- fishNAMES %>%
    select("SpecCode", "Species")

fishSPECIES <- fishSPECIES %>%
    select("SpecCode", "Brack", "Saltwater", "AnaCat")

fishBase <- fishCOUNTRY %>%
    select("SpecCode", "C_Code", "CSub_Code", "Status", "country") %>%
    filter(C_Code == "076") %>%
    filter(CSub_Code %in% c("BR-MA", "BR-PI", "BR-CE","BR-RN","BR-PB",
                            "BR-PE", "BR-SE", "BR-AL", "BR-BA")) %>%
    mutate(CSub_Code = str_sub(CSub_Code, -2, -1)) %>%
    left_join(fishNAMES, by = "SpecCode") %>%
    left_join(fishSPECIES, by = "SpecCode") %>%
    filter(Status == "native") %>%
    filter(Saltwater == 1 &
               Brack == 1)

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
ordem <- c("SpecCode", "Species", "Lat", "Long", "geometry", "Source")

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
    
    protocol_number <- head(get_occurence_GBIF)
    occ_download_wait(protocol_number)
    data_GBIF_raw <- occ_download_get(protocol_number, overwrite = T) %>%
        occ_download_import()
    
    data_GBIF <- data_GBIF_raw %>%
        select(verbatimScientificName, decimalLatitude, decimalLongitude) %>%
        mutate(verbatimScientificName = as.character(verbatimScientificName)) %>%
        mutate(verbatimScientificName = str_extract(verbatimScientificName, "\\w+\\s+\\w+")) %>%
        mutate(decimalLongitude = as.numeric(decimalLongitude)) %>%
        mutate(decimalLatitude = as.numeric(decimalLatitude)) %>%
        mutate(SpecCode = fishes$SpecCode[match(verbatimScientificName, 
                                                fishes$Species)]) %>%
        rename(Species = verbatimScientificName) %>%
        mutate(Source = as.factor("GBIF")) %>%
        mutate(SpecCode = as.numeric(SpecCode))
    

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
        
    data_speciesLink <-  speciesLink %>% 
        select("scientificName", "decimalLongitude", "decimalLatitude") %>%
        mutate(SpecCode = fishes$SpecCode[match(scientificName, 
                                                fishes$Species)]) %>%
        rename(Species = scientificName) %>%
        mutate(Source = "speciesLink")
    
## AquaMaps
    
    am_sql <- RSQLite::dbConnect(SQLite(), 
                                 "D:\\Estudos\\PhD_PPGCMT_UFC\\Tese\\01_Model_CC_FishDistribution\\RProj_01_Model_CC_FishDistribution\\am.db")
    RSQLite::dbListTables(am_sql)
    
    am_db <- dbGetQuery(am_sql, "
                           
                           SELECT * 
                           FROM occurrencecells_r
                           
                           ")
    
    am_db <- as.data.frame(am_db)
    
    data_aquamaps <- am_db %>%
        filter(SpecCode  %in% fishes$SpecCode) %>%
        select(SpecCode, CenterLat, CenterLong) %>%
        rename(decimalLatitude = CenterLat) %>%
        rename(decimalLongitude = CenterLong) %>%
        mutate(Species = fishes$Species[match(SpecCode, fishes$SpecCode)]) %>%
        mutate(decimalLongitude = as.numeric(decimalLongitude)) %>%
        mutate(decimalLatitude = as.numeric(decimalLatitude)) %>%
        mutate(Source = as.factor("AquaMaps")) %>%
        mutate(SpecCode = as.numeric(SpecCode))
    
# create dataset and clean coordinates -----------------------------------------
    
    # data_GBIF
    # data_aquamaps
    
    coordFishes_toFlags <- bind_rows (data_GBIF, data_aquamaps, id = NULL) %>%
        arrange(Species)

    flagsSpatial <- CoordinateCleaner::clean_coordinates(
        x = coordFishes_toFlags,
        species = "Species",
        tests = c("duplicates",
                  "equal",
                  "validity",
                  "zeros", 
                  "centroids", 
                  "urban", 
                  "gbif", 
                  "institutions"))
        
    flagsSpatial %>% head
    summary(flagsSpatial)
    
    coordFishes_raw <- coordFishes_toFlags %>% 
        dplyr::filter(flagsSpatial$.summary == TRUE)
    
    correct_names <- function(dataframe, column, vector) {
        dataframe[[column]] <- as.character(dataframe[[column]])
        
        correspondencias <- amatch(dataframe[[column]], vector)
        
        dataframe[[column]] <- ifelse(!is.na(correspondencias),
                                      vector[correspondencias],
                                      dataframe[[column]])
        
        return(dataframe)
    }
    
    coordFishes <- correct_names(coordFishes_raw, "Species", fishes_sp) %>%
        drop_na()
    
    dataFishes_sf <-  st_as_sf(coordFishes,
                                remove = FALSE,
                                coords = c("decimalLongitude","decimalLatitude"),
                                crs = st_crs(4326))
    
# filter species ---------------------------------------------------------------
    
    dataFishes <- dataFishes_sf %>%
        group_by(Species) %>%
        summarise(n())
    