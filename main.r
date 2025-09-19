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
df3 <- df3 %>% rename(`Id Client` = ID_CLIENT)
DFF <- DF %>% left_join(df3)
DFF <- DFF %>% rename(NUM_SITE = `Num Site`,
                      DATE_ACHAT = `Date Achat`,
                      NOM_SITE = `Nom Site`,
                      MNT_ACHAT = `Mnt Achat`,
                      ID_CLIENT = `Id Client`,
                      NB_ACHAT = `Nb Achat`)
DFF <- DFF %>% mutate(DATE_ACHAT = as.Date(DATE_ACHAT, 
                                           format = "%d/%m/%Y"),
                      ID_CLIENT = as.character(ID_CLIENT),
                      NUM_SITE = as.character(NUM_SITE))

