library(shiny)
library(tidyverse)
library(readr)
library(haven)       
library(lubridate)   
library(leaflet)    
library(sf)          
library(plotly)      

achats <- read_delim("Achats_csv.csv", delim=';')
clients <- read_sas("clients.sas7bdat")
correspondance <- read_delim("Correspondance_sites.csv", delim=';')


achats <- achats %>% rename(NUM_SITE = `Num Site`,
                            DATE_ACHAT = `Date Achat`,
                            MNT_ACHAT = `Mnt Achat`,
                            ID_CLIENT = `Id Client`,
                            NB_ACHAT = `Nb Achat`)



data <- achats %>%
  left_join(clients, by = "ID_CLIENT") %>%
  left_join(correspondance, by = "NUM_SITE")

data <- data %>% filter(NUM_SITE != 7) %>% mutate(COD_SEXE = 
                                                    case_when(COD_SEXE == "2" ~ "Femme",
                                                              COD_SEXE == "1" ~ "Homme"))

data$DATE_ACHAT <- as.Date(data$DATE_ACHAT, format = "%d/%m/%Y")

data <- data %>%
  mutate(TrancheAge = case_when(
    2025 - year(data$DATE_NAIS) < 30 ~ "Moins de 30",
    2025 - year(data$DATE_NAIS) >= 30 & 2025 - year(data$DATE_NAIS) <= 45 ~ "Entre 30 et 45",
    2025 - year(data$DATE_NAIS) > 45 ~ "Plus de 45"
  ))


ui <- fluidPage(
  titlePanel("Suivi des ventes e-commerce - Groupe Webtendance"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("indicateur", "Indicateur à suivre:",
                  choices = c("Nombre d'achats", "Montant des achats")),
      
      selectInput("site", "Choisir un site:",
                  choices = c("Tous", unique(data$NOM_SITE))),
      
      dateRangeInput("dates", "Plage de dates:",
                     start = min(data$DATE_ACHAT),
                     end = max(data$DATE_ACHAT)),
      
      selectInput("filtre", "Filtrer selon:",
                  choices = c("Aucun", "Sexe", "Tranche d'âge"))
    ),
    
    mainPanel(
      plotlyOutput("evolution_plot"),
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  
  data_filtre <- reactive({
    df <- data
    
    if (input$site != "Tous") {
      df <- df %>% filter(NOM_SITE == input$site)
    }
    
    df <- df %>%
      filter(DATE_ACHAT >= input$dates[1],
             DATE_ACHAT <= input$dates[2])
    
    return(df)
  })
  
  output$resultat <- renderTable({
    df_mod <- df
    
    if (input$filtre == "Sexe") {
      df_mod <- df_mod %>%
        group_by(Mois = floor_date(DATE_ACHAT, "month"), COD_SEXE) %>%
        summarise(Total = sum(MNT_ACHAT), .groups = "drop")
    } else if (input$filtre == "Tranche d'âge") {
      df_mod <- df_mod %>%
        group_by(Mois = floor_date(DATE_ACHAT, "month"), TrancheAge) %>%
        summarise(Total = sum(MNT_ACHAT), .groups = "drop")
    } else {
      df_mod <- df_mod %>%
        group_by(Mois = floor_date(DATE_ACHAT, "month")) %>%
        summarise(Total = sum(MNT_ACHAT), .groups = "drop")
    }
    df_mod
  })
  
    output$evolution_plot <- renderPlotly({
    df <- data_filtre()
    df <- df %>% mutate(Mois = floor_date(DATE_ACHAT, "month"))
    
    if (input$filtre == "Sexe") {
      df <- df %>% 
        mutate(Sexe=recode(COD_SEXE, "1" = "Homme", "2"="Femme")) %>% 
        group_by(Mois, Sexe) %>% 
        summarise(
          Valeur = if (input$indicateur == "Nombre d'achats")
            sum(NB_ACHAT) else sum(MNT_ACHAT), .groups="drop"
          )
      p <- ggplot(df, aes(x = Mois, y = Valeur, color = Sexe)) +
        geom_line(size = 1.2) +
        labs(title = paste("Évolution des ", tolower(input$indicateur), " par sexe"),
             x = "Mois", y = input$indicateur) +
        theme_minimal()
    } else if (input$filtre == "Tranche d'âge") {
      df <- df %>%
        group_by(Mois, TrancheAge) %>%
        summarise(
          Valeur = if (input$indicateur == "Nombre d'achats") sum(NB_ACHAT) else sum(MNT_ACHAT),
          .groups = "drop"
        )
      
      p <- ggplot(df, aes(x = Mois, y = Valeur, color = TrancheAge)) +
        geom_line(size = 1.2) +
        labs(title = paste("Évolution des", tolower(input$indicateur), "par tranche d'âge"),
             x = "Mois", y = input$indicateur) +
        theme_minimal()
    } else {
      df <- df %>%
        group_by(Mois) %>%
        summarise(
          Valeur = if (input$indicateur == "Nombre d'achats") sum(NB_ACHAT) else sum(MNT_ACHAT),
          .groups = "drop"
        )
      
      p <- ggplot(df, aes(x = Mois, y = Valeur)) +
        geom_line(color = "#254", size = 1.2) +
        labs(title = paste("Évolution du", tolower(input$indicateur)),
             x = "Mois", y = input$indicateur) +
        theme_minimal()
    }
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
    })
  
    output$map <- renderLeaflet({
      df <- data_filtre() %>%
        mutate(CODE_DEPT = substr(COD_POSTAL, 1, 2))
      
      ventes_dept <- df %>%
        group_by(CODE_DEPT) %>%
        summarise(
          NbAchats = sum(NB_ACHAT, na.rm = TRUE),
          MontantTotal = sum(MNT_ACHAT, na.rm = TRUE),
          .groups = "drop"
        )
      
      france_dept <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson",
                             quiet = TRUE)
      
      france_dept <- france_dept %>%
        left_join(ventes_dept, by = c("code" = "CODE_DEPT"))
      
      if (input$indicateur == "Nombre d'achats") {
        variable <- france_dept$NbAchats
        legend_title <- "Nombre d'achats"
      } else {
        variable <- france_dept$MontantTotal
        legend_title <- "Montant des achats (€)"
      }
      
      palette <- colorQuantile("YlOrRd", variable, n = 5, na.color = "gray")
      
      leaflet(france_dept) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~palette(variable),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          label = ~paste0(nom, "<br>", legend_title, " : ",
                          format(round(variable, 0), big.mark = " ")),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#123",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          pal = palette,
          values = variable,
          opacity = 0.7,
          title = legend_title,
          position = "bottomright"
        )
    })
}

shinyApp(ui = ui, server = server)
