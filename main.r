library(dplyr) # data processing tydiverse
library(readr) # csv files
library(haven) # sas files
library(rsconnect) # shinyapps
library(lubridate)


#observation et traitement des données préalable

df <- readr::read_csv2("Achats_csv.csv", col_names = TRUE)
df2 <- readr::read_csv2("Correspondance_sites.csv")
df3 <- haven::read_sas("clients.sas7bdat")
head(df3, 5)
dim(df3)
df<- df %>% rename(NUM_SITE = `Num Site`,
                      DATE_ACHAT = `Date Achat`,
                      MNT_ACHAT = `Mnt Achat`,
                      ID_CLIENT = `Id Client`,
                      NB_ACHAT = `Nb Achat`)
head(df, 5)
DF <- df %>% left_join(df2, by="NUM_SITE")
head(DF, 5)
DF <- DF %>% filter(NUM_SITE != 7)
DF <- DF %>% mutate(DATE_ACHAT = as.Date(DATE_ACHAT, format = "%d/%m/%Y"))
DF
indicateur <- "MNT_ACHAT"
#choix <- "GamersAddicts.com"
date_debut <- as.Date("2013-01-02")
date_fin <-as.Date("2013-01-07")
choix <- c(date_debut, date_fin)
#choix<- NA
  
if(is.Date(choix)){
  DF_date<- DF %>% filter(between(DATE_ACHAT, date_debut, date_fin))
  if(indicateur=="NB_ACHAT"){
    DF_voulu<- DF_date%>%select(NB_ACHAT)%>%summarize(TOTAL=sum(NB_ACHAT, na.rm= TRUE))
  }else{
    DF_voulu<- DF_date%>%select(MNT_ACHAT)%>%summarize(TOTAL=sum(MNT_ACHAT, na.rm= TRUE))
  }
}else{
  if(!is.na(choix)){
    DF_site <- DF%>% filter(NOM_SITE==site_choisi)
    if(indicateur=="NB_ACHAT"){
      DF_voulu <- DF_site%>%select(NB_ACHAT)%>%summarize(TOTAL=sum(NB_ACHAT, na.rm= TRUE))
    }else{
      DF_voulu <- DF_site%>%select(MNT_ACHAT)%>%summarize(TOTAL=sum(MNT_ACHAT, na.rm= TRUE))
    }
  }else{
    if(indicateur=="NB_ACHAT"){
      DF_voulu <- DF%>%select(NB_ACHAT)%>%summarize(TOTAL=sum(NB_ACHAT, na.rm= TRUE))
    }else{
      DF_voulu <- DF%>%select(MNT_ACHAT)%>%summarize(TOTAL=sum(MNT_ACHAT, na.rm= TRUE))
    }
  }
}
DF_voulu

#vision cartographique de la répartition des ventes par département ou par région
library(leaflet)
library(plyr)

regions_wgs84 <- read_sf(
  dsn = "data/contours-geographiques-des-nouvelles-regions-metropole", 
  layer = "contours-geographiques-des-nouvelles-regions-metropole") %>% 
  mutate(region = str_conv(region, "utf-8"))
regions_wgs84
