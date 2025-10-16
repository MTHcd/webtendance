library(dplyr) # data processing tydiverse
library(readr) # csv files
library(haven) # sas files
library(rsconnect) # shinyapps

df <- readr::read_csv2("Achats_csv.csv", col_names = TRUE)
df2 <- readr::read_csv2("Correspondance_sites.csv")
df3 <- read_sas("clients.sas7bdat")
df2 <- df2 %>% rename(`Num Site` = `NUM_SITE`,
                      `Nom Site` = `NOM_SITE`)
DF <- df %>% left_join(df2)