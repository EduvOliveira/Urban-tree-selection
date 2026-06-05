#' ---
# Title: Functional and climate-based selection of # tree  species for resilient urban afforestation 
# By Eduardo V. S. Oliveira
# June 5, 2026
#' ---

### Selecting species that maximize carbon storage and support urban fauna

install.packages(c('openxlsx', 'dplyr', 'tidyr','stringr', 'forcats'))

# Load libraries

library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)

# Load function

source("select_carbon_species.R")

# Load data

setwd("insert_the_path")

spp_att<-read.table("traits_final.txt", header =TRUE)

spp_city<-read.table("list_spp.txt",header =TRUE)

# Standardize the species list

mun_long <- spp_city %>% 
  separate_rows(spp, sep = ",\\s*") %>%
  mutate(species = str_replace_all(spp, "_", " ")) %>%
  select(city, species)

mun_long

# Compile trait data

traits <- spp_att %>% mutate(species = str_replace_all(species, "_", " ")) %>%
  rename(
    Height_max = H,
    DAP_max = DBH,
    Wood_density = Dwood
  )

traits2<-traits[1:369,]

# Merge occurrence and trait datasets

mun_traits <- mun_long %>%
  left_join(traits2, by = "species")


# Apply the function

ranking_mun <- mun_traits %>%
  group_by(city) %>%
  group_modify(~ select_carbon_species(.x)) %>%
  ungroup()

write.xlsx(ranking_mun, "ranking_carbon.xlsx")

# Select the top 50 species

top_carbon <- ranking_mun %>%
  group_by(city) %>%
  slice_max(carbon_score, n = 50) %>%
  ungroup()

# Include fauna-attractiveness criteria

ranking_biotic <- ranking_mun %>%
  filter(SD == "biotic") %>%
  arrange(city, desc(carbon_score))

# Select the top 50 species

top_biotic <- ranking_biotic %>%
group_by(city) %>%
  slice_head(n = 50) %>%
  ungroup()


##THE END##

rm(list=ls())
