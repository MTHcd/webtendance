library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(leaflet)
library(sf)
library(plotly)
library(RColorBrewer)

# ====== Données ======
achats <- read_delim("Achats_csv.csv", delim=';')
clients <- read_sas("clients.sas7bdat")
correspondance <- read_delim("Correspondance_sites.csv", delim=';')

achats <- achats %>% rename(
  NUM_SITE = `Num Site`,
  DATE_ACHAT = `Date Achat`,
  MNT_ACHAT = `Mnt Achat`,
  ID_CLIENT = `Id Client`,
  NB_ACHAT = `Nb Achat`
)

df_data <- achats %>%
  left_join(clients, by = "ID_CLIENT") %>%
  left_join(correspondance, by = "NUM_SITE") %>%
  filter(NUM_SITE != 7)

df_data$DATE_ACHAT <- as.Date(df_data$DATE_ACHAT, format="%d/%m/%Y")

df_data <- df_data %>%
  mutate(
    TrancheAge = case_when(
      2025 - year(DATE_NAIS) < 30 ~ "Moins de 30",
      2025 - year(DATE_NAIS) >= 30 & 2025 - year(DATE_NAIS) <= 45 ~ "Entre 30 et 45",
      2025 - year(DATE_NAIS) > 45 ~ "Plus de 45"
    )
  )

# ====== UI ======
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Dashboard e-commerce - Groupe Webtendance"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("indicateur", "Indicateur :", choices = c("Nombre d'achats", "Montant des achats")),
      selectInput("site", "Site :", choices = c("Tous", unique(df_data$NOM_SITE))),
      dateRangeInput("dates", "Plage de dates :", start = min(df_data$DATE_ACHAT), end = max(df_data$DATE_ACHAT)),
      checkboxGroupInput("filtres", "Filtres :", choices = c("Sexe", "Tranche d'âge")),
      checkboxGroupInput("graphiques", "Graphiques :", choices = c("Évolution", "Carte"), selected = c("Évolution","Carte")),
      checkboxInput("show_points", "Afficher les points", value = TRUE),
      checkboxInput("show_smooth", "Afficher interpolation / lissage", value = TRUE)
    ),
    checkboxGroupInput("tranches", "Tranches d'âge :", 
                       choices = c("Moins de 30","Entre 30 et 45","Plus de 45"),
                       selected = c("Moins de 30","Entre 30 et 45","Plus de 45")),
    mainPanel(
      uiOutput("plots_ui")
    )
  )
)

# ====== Server ======
server <- function(input, output, session) {
  
  # --- Filtrage dynamique ---
  # --- Filtrage dynamique avec tranche d'âge ---
  df_data_filtre <- reactive({
    df <- df_data
    if(input$site != "Tous") df <- df %>% filter(NOM_SITE == input$site)
    df <- df %>% filter(DATE_ACHAT >= input$dates[1] & DATE_ACHAT <= input$dates[2])
    
    if("Sexe" %in% input$filtres) df <- df %>% mutate(Sexe = recode(COD_SEXE,"1"="Homme","2"="Femme"))
    
    # Filtrer tranches d'âge si sélectionnées
    if("Tranche d'âge" %in% input$filtres & !is.null(input$tranches)){
      df <- df %>% filter(TrancheAge %in% input$tranches)
    }
    df
  })
  
  
  # --- Graphique évolution ---
  output$evolution_plot <- renderPlotly({
    df <- df_data_filtre() %>%
      mutate(Mois = floor_date(DATE_ACHAT, "month"))
    
    # Déterminer les variables de regroupement
    group_vars <- "Mois"
    if("Sexe" %in% input$filtres) group_vars <- c(group_vars,"Sexe")
    if("Tranche d'âge" %in% input$filtres) group_vars <- c(group_vars,"TrancheAge")
    
    # Agrégation
    df <- df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        Valeur = if(input$indicateur=="Nombre d'achats") sum(NB_ACHAT, na.rm = TRUE) else sum(MNT_ACHAT, na.rm = TRUE),
        NbAchats = sum(NB_ACHAT, na.rm = TRUE),
        Montant = sum(MNT_ACHAT, na.rm = TRUE),
        .groups="drop"
      )
    
    color_var <- setdiff(group_vars,"Mois")
    
    # Ajouter la colonne text pour tooltip
    if(length(color_var) == 0){
      df <- df %>% mutate(text = paste0("Mois: ", Mois,
                                        "<br>Montant (€): ", Montant,
                                        "<br>Nb Achats: ", NbAchats))
      p <- ggplot(df, aes(x=Mois, y=Valeur, text=text))
      if(input$show_points) p <- p + geom_point(color="#254", size=3)
      p <- p + geom_line(color="#254", size=1.5)
      if(input$show_smooth) p <- p + geom_smooth(method="loess", se=FALSE, linetype="dashed", color="#254")
    } else {
      # Interaction pour couleur
      df <- df %>% mutate(group_color = interaction(!!!syms(color_var)))
      
      # Tooltip
      df <- df %>% mutate(text = paste0("Mois: ", Mois,
                                        "<br>", paste(paste(color_var, df[,color_var], sep=": "), collapse="<br>"),
                                        "<br>Montant (€): ", Montant,
                                        "<br>Nb Achats: ", NbAchats))
      
      n_colors <- length(unique(df$group_color))
      palette_colors <- RColorBrewer::brewer.pal(min(n_colors,8),"Set2")
      
      p <- ggplot(df, aes(x=Mois, y=Valeur, color=group_color, text=text))
      if(input$show_points) p <- p + geom_point(size=3)
      p <- p + geom_line(size=1.5)
      if(input$show_smooth) p <- p + geom_smooth(method="loess", se=FALSE,
                                                 linetype="dashed",
                                                 aes(group=group_color))
      p <- p + scale_color_manual(values=palette_colors)
    }
    
    # Labels et thème
    p <- p + labs(title=paste("Évolution du", tolower(input$indicateur)),
                  x="Mois", y=input$indicateur) +
      theme_minimal(base_size=14) +
      theme(plot.title=element_text(size=18, face="bold", hjust=0.5),
            legend.title=element_blank())
    
    # Convertir en plotly
    ggplotly(p, tooltip="text") %>%
      layout(legend=list(orientation="h", x=0.5, xanchor="center", y=-0.2))
  })
  
  # --- Carte ---
  output$map <- renderLeaflet({
    df <- df_data_filtre() %>% mutate(CODE_DEPT = substr(COD_POSTAL,1,2))
    
    ventes_dept <- df %>%
      group_by(CODE_DEPT) %>%
      summarise(
        NbAchats = sum(NB_ACHAT, na.rm = TRUE),
        MontantTotal = sum(MNT_ACHAT, na.rm = TRUE),
        .groups="drop"
      )
    
    france_dept <- st_read(
      "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson",
      quiet = TRUE
    ) %>% left_join(ventes_dept, by=c("code"="CODE_DEPT"))
    
    variable <- if(input$indicateur=="Nombre d'achats") france_dept$NbAchats else france_dept$MontantTotal
    variable[is.na(variable)] <- 0
    legend_title <- if(input$indicateur=="Nombre d'achats") "Nombre d'achats" else "Montant (€)"
    
    palette <- colorBin("YlGnBu", domain = variable, bins=5, na.color = "lightgray")
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s : %s",
      france_dept$nom, legend_title, format(variable, big.mark=" ", scientific = FALSE)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(france_dept) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=2.2137, lat=46.2276, zoom=5) %>%
      addPolygons(
        fillColor = ~palette(variable),
        weight=1,
        color="white",
        dashArray="3",
        fillOpacity=0.7,
        label=labels,
        highlightOptions = highlightOptions(weight=3, color="#123", fillOpacity=0.9, bringToFront=TRUE)
      ) %>%
      addLegend(
        pal=palette,
        values=variable,
        opacity=0.7,
        title=legend_title,
        position="bottomright"  # position valide
      )
  })
  
  # --- UI dynamique ---
  output$plots_ui <- renderUI({
    plot_outputs <- list()
    
    if("Évolution" %in% input$graphiques){
      plot_outputs <- append(plot_outputs, list(plotlyOutput("evolution_plot", height="500px")))
    }
    
    if("Carte" %in% input$graphiques){
      plot_outputs <- append(plot_outputs, list(leafletOutput("map", height="500px")))
    }
    
    if(length(plot_outputs) == 0){
      return(tags$div("Aucun graphique sélectionné"))
    }
    
    tagList(plot_outputs)
  })
  
}

# ====== Lancer l'app ======
shinyApp(ui=ui, server=server)
