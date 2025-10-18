library(shiny)
library(tidyverse)
library(readr)
library(haven)       # Pour lire les fichiers .sas7bdat
library(lubridate)   # Pour gérer les dates
library(leaflet)     # Pour la cartographie
library(sf)          # Pour les données géographiques
library(plotly)      # Pour des graphiques interactifs

achats <- read_delim("Achats_csv.csv", delim=';')
clients <- read_sas("clients.sas7bdat")
correspondance <- read_delim("Correspondance_sites.csv", delim=';')

# Merge des données

achats <- achats %>% rename(NUM_SITE = `Num Site`,
                            DATE_ACHAT = `Date Achat`,
                            MNT_ACHAT = `Mnt Achat`,
                            ID_CLIENT = `Id Client`,
                            NB_ACHAT = `Nb Achat`)

# refactorer COD_SEXE


data <- achats %>%
  left_join(clients, by = "ID_CLIENT") %>%
  left_join(correspondance, by = "NUM_SITE")

# Exclusion du site numéro 7
data <- data %>% filter(NUM_SITE != 7) %>% mutate(COD_SEXE = 
                                                    case_when(COD_SEXE == "2" ~ "Femme",
                                                              COD_SEXE == "1" ~ "Homme"))

# Formatage des dates
data$DATE_ACHAT <- as.Date(data$DATE_ACHAT, format = "%d/%m/%Y")

# Ajout des tranches d'âge
data <- data %>%
  mutate(TrancheAge = case_when(
    2025 - year(data$DATE_NAIS) < 30 ~ "Moins de 30",
    2025 - year(data$DATE_NAIS) >= 30 & 2025 - year(data$DATE_NAIS) <= 45 ~ "Entre 30 et 45",
    2025 - year(data$DATE_NAIS) > 45 ~ "Plus de 45"
  ))
