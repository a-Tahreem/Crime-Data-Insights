library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(lubridate)
# Installation et chargement du package scales
if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("scales")
}
library(scales)

# Interface utilisateur
# Interface utilisateur
ui <- fluidPage(
  # En-tête
  tags$head(
    tags$style(HTML("
      body { font-family: Arial, sans-serif; background-color: #f7f9fc; }
      .navbar { background-color: #4A90E2; color: white; }
      .navbar-brand, .navbar-nav .nav-link { color: white !important; font-weight: bold; }
      .navbar-nav .nav-link:hover { background-color: #357ABD; }
      .tab-content { background-color: #FFFFFF; padding: 20px; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); }
      h3, h4 { color: #4A90E2; font-weight: bold; }
      .panel { border: none; }
      .sidebar { background-color: #eaf1f8; border-radius: 8px; padding: 15px; }
    "))
  ),
  
  titlePanel(
    title = div(
      h1("Analyse des Victimes et Crimes", style = "color: #357ABD; font-size: 24px;"),
      h4("Tahreem, Assa, Inès, Sofia", style = "font-style: italic; color: #666;")
    )
  ),
  
  navbarPage(
    "Exploration et Analyse",
    
    # Onglet Prétraitement des données
    tabPanel("Prétraitement des données",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 h3("Étapes du Prétraitement"),
                 p("1. Importation et Résumé des Données"),
                 p("2. Suppression des Valeurs Manquantes"),
                 p("3. Transformation des Dates"),
                 p("4. Filtrage des Années")
               ),
               
               mainPanel(
                 h4("Dimensions des données initial"),
                 verbatimTextOutput("text_output"),
                 
                 h4("Résumé initial des données"),
                 verbatimTextOutput("initial_summary"),
                 
                 h4("Valeurs manquantes par colonne avant nettoyage"),
                 plotOutput("na_plot"),
                 
                 h4("Dimensions après suppression des valeurs manquantes"),
                 verbatimTextOutput("data_dimensions"),
                 
                 h4("Nombre d'incidents par année"),
                 plotOutput("incident_plot")
               )
             )),
    
    # Onglet Visualisation des variables
    tabPanel("Visualisation des variables",
             mainPanel(
               h4("1. Statistiques descriptives des variables"),
               verbatimTextOutput("summary_stats"),
               
               h4("2. Histogramme de l'âge des victimes"),
               plotOutput("age_histogram"),
               
               h4("3. Boxplot de l'âge selon le sexe"),
               plotOutput("age_boxplot"),
               
               h4("4. Test de Wilcoxon sur l'âge par sexe"),
               verbatimTextOutput("wilcoxon_test"),
               
               h4("5. Matrice de Corrélation"),
               plotOutput("correlation_matrix"),
               
               h4("6. Analyse en Composantes Principales (ACP)"),
               plotOutput("pca_plot"),
               
               h4("7. Répartition des victimes par sexe"),
               plotOutput("sex_distribution"),
               
               h4("8. Répartition des types de crimes"),
               plotOutput("crime_type_distribution"),
               
               h4("9. Visualisation des incidents dans le temps"),
               plotOutput("time_series_plot")
             )),
    
    # Onglet Corrélation
    tabPanel("Corrélation",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 h3("Analyse de la Corrélation"),
                 p("Analyse de la corrélation entre les périodes de la journée et le nombre d'incidents.")
               ),
               
               mainPanel(
                 h4("1. Répartition des incidents par moment de la journée"),
                 plotOutput("accidents_barplot"),
                 
                 h4("2. Résultats du test du Chi-carré"),
                 verbatimTextOutput("chi2_test"),
                 
                 h4("3. Résidus Standardisés du Chi-carré"),
                 verbatimTextOutput("residuals"),
                 
               )
             )),
    
    # Onglet ANOVA
    tabPanel("ANOVA",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 h3("Répartition des Types de Crimes par Moment de la Journée et Sexe"),
                 p("Exploration des variations dans la répartition des types de crimes en fonction du moment de la journée et du sexe.")
               ),
               
               mainPanel(
                 h4("1. Catégorisation des types de crimes"),
                 plotOutput("crime_category_barplot"),
                 
                 h4("2. Résultats de l'ANOVA"),
                 verbatimTextOutput("anova_summary"),
                 
                 h4("3. Test post hoc pour Time_Period"),
                 verbatimTextOutput("tukey_time_period"),
                 
                 h4("4. Test post hoc pour Vict.Sex"),
                 verbatimTextOutput("tukey_vict_sex")
               )
             )),
    
    # Onglet Test du Chi-2
    tabPanel("Test du Chi-2",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 h3("Analyse du Test Chi-2 d'indépendance"),
                 p("Examen des différences dans le nombre de victimes selon les tranches d'âge et les zones géographiques.")
               ),
               
               mainPanel(
                 h4("1. Nombre de victimes par tranche d'âge"),
                 plotOutput("age_distribution"),
                 verbatimTextOutput("age_chi2_test"),
                 
                 h4("2. Résultats du test post-hoc (tranches d'ages)"), 
                 verbatimTextOutput("post_hoc_results"), 
                 
                 h4("3. Fréquence des victimes par Zone Géographique"),
                 plotOutput("geo_distribution"),
                 verbatimTextOutput("geo_chi2_test"),
                 
                 h4("4. Résultats du test post-hoc (zones géographiques)"),
                 verbatimTextOutput("post_hoc_results1"),
                 verbatimTextOutput("text_output1"),
               ),
             )),
    
    # Onglet Régression logistique
    tabPanel("Régression Logistique",
             mainPanel(
               h4("Table des catégories d'armes"),
               verbatimTextOutput("weapon_category_table"),
               
               h4("Table des catégories de crimes"),
               verbatimTextOutput("crime_category_table"),
               
               h4("Résumé du modèle de régression logistique"),
               verbatimTextOutput("logistic_regression_summary"),
               
               h4("Visualisation des coefficients du modèle"),
               plotOutput("logistic_regression_plot"),
               verbatimTextOutput("text_output2"),
             )), 
    
    # Onglet CCL
    tabPanel("Conclusion",
             mainPanel(
               h4("Conlusion générale"),
               verbatimTextOutput("ccl"),
             )), 
    
  )
)

# Définir les fonctions de catégorisation en dehors des sorties pour qu'elles soient disponibles globalement

categorize_weapon <- function(description) {
  description <- tolower(description)
  if (grepl("gun|pistol|revolver|rifle|shotgun|firearm|semi-automatic|automatic|uzi|sub-machine|assault|starter pistol|bb gun|heckler|m-14|mac-10|antique firearm|relic firearm", description)) {
    return("Armes à feu")
  } else if (grepl("knife|blade|razor|machete|scissors|switch blade|dagger|sword|ice pick|cleaver|dirk|bowie|axe|martial arts", description)) {
    return("Armes blanches")
  } else if (grepl("strong-arm|hands|fist|feet|bodily force|physical presence|brass knuckles", description)) {
    return("Mains/Poings")
  } else if (grepl("bomb|explosive|grenade", description)) {
    return("Explosifs")
  } else {
    return("Autres")
  }
}

categorize_crime <- function(description) {
  if (grepl('ASSAULT|BATTERY|THREATS|KIDNAPPING', description)) {
    return('Aggression')
  } else if (grepl('THEFT|BURGLARY|ROBBERY|SHOPLIFTING', description)) {
    return('Vol')
  } else if (grepl('IDENTITY|FORGERY|EMBEZZLEMENT', description)) {
    return('Fraude et usurpation')
  } else if (grepl('RESTRAINING ORDER|COURT ORDER', description)) {
    return('Violations d\'ordonnance')
  } else if (grepl('VANDALISM', description)) {
    return('Dommages à la propriété')
  } else if (grepl('SEX OFFENDER|RAPE|SEXUAL CONTACT', description)) {
    return('Crimes sexuels')
  } else if (grepl('CHILD NEGLECT', description)) {
    return('Crimes contre les enfants')
  } else if (grepl('FIREARMS|WEAPON', description)) {
    return('Armes')
  } else if (grepl('DRUG|NARCOTIC|CONTROLLED SUBSTANCE', description)) {
    return('Crimes liés aux drogues')
  } else if (grepl('HARASSMENT|STALKING', description)) {
    return('Harcèlement')
  } else if (grepl('TERRORISM|BOMBING|EXPLOSIVE', description)) {
    return('Terrorisme')
  } else if (grepl('FRAUD|SCAM', description)) {
    return('Escroquerie')
  } else if (grepl('TRESPASSING', description)) {
    return('Intrusion')
  } else if (grepl('ARSON', description)) {
    return('Incendie criminel')
  } else if (grepl('PROSTITUTION|SOLICITATION', description)) {
    return('Prostitution')
  } else if (grepl('DISORDERLY CONDUCT', description)) {
    return('Conduite désordonnée')
  } else if (grepl('PUBLIC INTOXICATION', description)) {
    return('Ivresse publique')
  } else {
    return('Autres')
  }
}


# Serveur
server <- function(input, output, session) {
  
  # Fonction pour catégoriser les crimes
  categorize_crime <- function(description) {
    if (grepl('ASSAULT|BATTERY|THREATS|KIDNAPPING', description)) {
      return('Aggression')
    } else if (grepl('THEFT|BURGLARY|ROBBERY|SHOPLIFTING', description)) {
      return('Vol')
    } else if (grepl('IDENTITY|FORGERY|EMBEZZLEMENT', description)) {
      return('Fraude et usurpation')
    } else if (grepl('RESTRAINING ORDER|COURT ORDER', description)) {
      return('Violations d\'ordonnance')
    } else if (grepl('VANDALISM', description)) {
      return('Dommages à la propriété')
    } else if (grepl('SEX OFFENDER|RAPE|SEXUAL CONTACT', description)) {
      return('Crimes sexuels')
    } else if (grepl('CHILD NEGLECT', description)) {
      return('Crimes contre les enfants')
    } else if (grepl('FIREARMS|WEAPON', description)) {
      return('Armes')
    } else if (grepl('DRUG|NARCOTIC|CONTROLLED SUBSTANCE', description)) {
      return('Crimes liés aux drogues')
    } else if (grepl('HARASSMENT|STALKING', description)) {
      return('Harcèlement')
    } else if (grepl('TERRORISM|BOMBING|EXPLOSIVE', description)) {
      return('Terrorisme')
    } else if (grepl('FRAUD|SCAM', description)) {
      return('Escroquerie')
    } else if (grepl('TRESPASSING', description)) {
      return('Intrusion')
    } else if (grepl('ARSON', description)) {
      return('Incendie criminel')
    } else if (grepl('PROSTITUTION|SOLICITATION', description)) {
      return('Prostitution')
    } else if (grepl('DISORDERLY CONDUCT', description)) {
      return('Conduite désordonnée')
    } else if (grepl('PUBLIC INTOXICATION', description)) {
      return('Ivresse publique')
    } else {
      return('Autres')
    }
  }
  
  # Prétraitement des données
  
  output$text_output <- renderText({
    "Data initial : 752 911 crimes repertoriés et 28 variables"
  })
  
  data_new <- reactive({
    df <- read.csv("donnes.csv", header = TRUE, sep = ",")
    df[df == ""] <- NA
    
    # Suppression des colonnes inutiles
    df <- df[,-c(1,2,5,8,11,21,22,23,24,26,27,28)]
    
    # Suppression des individus avec 4 valeurs manquantes ou plus
    na_count <- rowSums(is.na(df))
    df <- df[na_count < 4, ]
    
    # Transformation de la variable DATE.OCC
    df$DATE.OCC <- as.POSIXct(df$DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
    df$Year <- as.numeric(format(df$DATE.OCC, "%Y"))
    
    # Filtrage pour conserver uniquement les années postérieures à 2021
    df <- df %>% filter(Year > 2021)
    
    return(df)
  })
  
  output$initial_summary <- renderPrint({
    df <- read.csv("donnes.csv", header = TRUE, sep = ",")
    str(df)
  })
  
  output$na_plot <- renderPlot({
    df <- read.csv("donnes.csv", header = TRUE, sep = ",")
    df[df == ""] <- NA
    na_counts <- colSums(is.na(df))
    na_data <- data.frame(Variable = names(na_counts), MissingValues = na_counts)
    ggplot(na_data, aes(x = reorder(Variable, -MissingValues), y = MissingValues)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Nombre de Valeurs Manquantes par Colonne", x = "Variable", y = "Nombre de NA") +
      theme_minimal()
  })
  
  output$data_dimensions <- renderPrint({
    df <- data_new()
    dim(df)
  })
  
  output$incident_plot <- renderPlot({
    df <- data_new()
    incident_counts <- df %>%
      group_by(Year) %>%
      summarise(Count = n())
    ggplot(incident_counts, aes(x = Year, y = Count)) +
      geom_col(fill = "skyblue") +
      labs(title = "Nombre d'Incidents par Année", x = "Année", y = "Nombre d'Incidents") +
      theme_minimal()
  })
  
  # Visualisation des variables
  output$summary_stats <- renderPrint({
    df <- data_new()
    capture.output(str(df))
  })
  
  output$age_histogram <- renderPlot({
    df <- data_new()
    
    # Filtrer les données pour exclure les victimes avec Vict.Sex = "X"
    df_filtered <- df[df$Vict.Sex != "X", ]
    
    hist(df_filtered$Vict.Age,
         main = "Distribution des âges des victimes",
         xlab = "Âge des victimes",
         col = "lightblue",
         breaks = 30)
    
    # Ajouter les axes
    axis(1, at = seq(0, 100, by = 5))
  })
  
  output$age_boxplot <- renderPlot({
    df <- data_new()
    df_filtered <- subset(df, Vict.Sex != "H")
    boxplot(df_filtered$Vict.Age ~ df_filtered$Vict.Sex,
            main = "Âge des victimes selon le sexe",
            xlab = "Sexe",
            ylab = "Âge",
            col = "skyblue")
  })
  
  output$wilcoxon_test <- renderPrint({
    df <- data_new()
    df_filtered <- subset(df, Vict.Sex %in% c("F", "M") & !is.na(Vict.Age))
    wilcox.test(df_filtered$Vict.Age ~ df_filtered$Vict.Sex)
  })
  
  output$correlation_matrix <- renderPlot({
    df <- data_new()
    data_numeric <- df %>% select_if(is.numeric)
    cor_matrix <- cor(data_numeric, use = "complete.obs")
    corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 45)
  })
  
  output$pca_plot <- renderPlot({
    df <- data_new()
    data_numeric <- df %>% select_if(is.numeric)
    data_numeric <- data.frame(lapply(data_numeric, function(x) {
      ifelse(is.na(x), mean(x, na.rm = TRUE), x)
    }))
    pca_res <- prcomp(data_numeric, scale. = TRUE)
    plot(pca_res$x[, 1], pca_res$x[, 2],
         xlab = "PC1", ylab = "PC2",
         col = "white")
    points(pca_res$x[, 1], pca_res$x[, 2], pch = 19, col = "blue")
    arrows(0, 0, pca_res$rotation[, 1] * max(pca_res$x[, 1]),
           pca_res$rotation[, 2] * max(pca_res$x[, 2]), col = "red", length = 0.1)
    text(pca_res$rotation[, 1] * max(pca_res$x[, 1]),
         pca_res$rotation[, 2] * max(pca_res$x[, 2]),
         labels = rownames(pca_res$rotation), col = "red", pos = 4)
  })
  
  output$sex_distribution <- renderPlot({
    df <- data_new()
    
    # Créer le tableau de fréquences en excluant les valeurs "H" et "X"
    sex_table <- table(df$Vict.Sex[df$Vict.Sex != "H" & df$Vict.Sex != "X"])
    
    # Tracer le barplot
    barplot(sex_table,
            main = "Répartition des victimes par sexe",
            xlab = "Sexe",
            ylab = "Nombre de victimes",
            col = c("aquamarine2", "cyan3"),
            ylim = c(0, max(sex_table) * 1.1))  # Ajouter un espace au-dessus des barres
  })
  
  output$crime_type_distribution <- renderPlot({
    df <- data_new()
    barplot(table(df$Crm.Cd.Desc),
            main = "Répartition des types de crimes",
            col = "lightblue", las = 2, cex.names = 0.7)
  })
  
  output$time_series_plot <- renderPlot({
    df <- data_new()
    incident_counts <- df %>%
      group_by(Year) %>%
      summarise(Count = n())
    plot(incident_counts$Year, incident_counts$Count,
         type = "b",
         col = "darkslategray4",
         xlab = "Année",
         ylab = "Nombre d'incidents",
         main = "Nombre d'incidents par année")
  })
  
  # Partie ANOVA
  data_anova <- reactive({
    df <- data_new()
    
    # Création des périodes de la journée
    df$Time_Period <- cut(df$TIME.OCC, breaks = c(0, 600, 1200, 1800, 2400),
                          labels = c("N", "M", "AM", "S"), include.lowest = TRUE)
    
    # Application de la catégorisation des crimes
    df$Crime_Category <- sapply(df$Crm.Cd.Desc, categorize_crime)
    
    # Nettoyage des valeurs NA dans les colonnes pertinentes
    df_clean <- na.omit(df %>% select(Crime_Category, Time_Period, Vict.Sex))
    df_clean$Crime_Category <- as.factor(df_clean$Crime_Category)
    df_clean$Time_Period <- as.factor(df_clean$Time_Period)
    df_clean <- df_clean[df_clean$Vict.Sex != "H", ]
    df_clean$Vict.Sex <- as.factor(df_clean$Vict.Sex)
    df_clean$Crime_Category_numeric <- as.numeric(as.factor(df_clean$Crime_Category))
    
    return(df_clean)
  })
  
  output$crime_category_barplot <- renderPlot({
    df <- data_anova()
    crime_counts <- table(df$Crime_Category)
    barplot(crime_counts,
            main = "Nombre de crimes par catégorie",
            ylab = "Nombre de crimes",
            col = "skyblue2",
            las = 2,
            cex.names = 0.8)
    title(line = 2)
  })
  
  output$anova_summary <- renderPrint({
    df <- data_anova()
    anova_model <- aov(Crime_Category_numeric ~ Time_Period + Vict.Sex + Time_Period:Vict.Sex, data = df)
    summary(anova_model)
  })
  
  output$tukey_time_period <- renderPrint({
    df <- data_anova()
    anova_model <- aov(Crime_Category_numeric ~ Time_Period + Vict.Sex + Time_Period:Vict.Sex, data = df)
    tukey_time_period <- TukeyHSD(anova_model, "Time_Period")
    print(tukey_time_period)
  })
  
  output$tukey_vict_sex <- renderPrint({
    df <- data_anova()
    anova_model <- aov(Crime_Category_numeric ~ Time_Period + Vict.Sex + Time_Period:Vict.Sex, data = df)
    tukey_vict_sex <- TukeyHSD(anova_model, "Vict.Sex")
    print(tukey_vict_sex)
  })
  
  # Partie Corrélation
  data_correlation <- reactive({
    df <- data_new()
    
    # Création des périodes de la journée
    df$moment <- with(df, ifelse(TIME.OCC >= 600 & TIME.OCC < 1200, "matin",
                                 ifelse(TIME.OCC >= 1200 & TIME.OCC < 1800, "aprem",
                                        ifelse(TIME.OCC >= 1800 & TIME.OCC < 2400, "soir", "nuit"))))
    return(df)
  })
  
  output$accidents_barplot <- renderPlot({
    df <- data_correlation()
    nb_accidents <- table(df$moment)
    barplot(nb_accidents, col = "turquoise4",
            main = "Nombre d'accidents par tranche horaire",
            xlab = "Tranches horaires", ylab = "Nombre d'accidents")
  })
  
  output$chi2_test <- renderPrint({
    df <- data_correlation()
    nb_accidents <- table(df$moment)
    test_chi2 <- chisq.test(nb_accidents)
    print(test_chi2)
  })
  
  output$residuals <- renderPrint({
    df <- data_correlation()
    nb_accidents <- table(df$moment)
    test_chi2 <- chisq.test(nb_accidents)
    test_chi2$residuals
  })
  
  # Test du Chi-2 d'indépendance
  output$geo_distribution <- renderPlot({
    df <- data_new()
    frequence_victimes <- df %>%
      group_by(AREA.NAME) %>%
      summarise(FREQUENCE_VICTIMES = n()) %>%
      ungroup()
    
    # Utilisation de rescale avec le package scales
    frequence_victimes$COLOR_SCALE <- rescale(frequence_victimes$FREQUENCE_VICTIMES)
    
    ggplot(frequence_victimes, aes(x = reorder(AREA.NAME, -FREQUENCE_VICTIMES), y = FREQUENCE_VICTIMES)) +
      geom_bar(stat = "identity", aes(fill = COLOR_SCALE)) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Fréquence des Victimes par Zone Géographique",
           x = "Zone Géographique",
           y = "Fréquence des Victimes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  ##
  output$age_distribution <- renderPlot({
    df <- data_new()
    df <- df[df$Vict.Age > 0, ]
    df$tranche_age <- cut(df$Vict.Age, 
                          breaks = c(0, 17, 30, 45, 60, 100),
                          labels = c("0-17", "18-30", "31-45", "46-60", "61+"),
                          include.lowest = TRUE)
    
    ggplot(df, aes(x = tranche_age, fill = tranche_age)) +
      geom_bar() +
      scale_fill_manual(values = c("0-17" = "#FFD1DC", "18-30" = "#A4D3EE", 
                                   "31-45" = "#B3EBA0", "46-60" = "#FFE4B5", "61+" = "#FDFD96")) +
      labs(title = "Répartition des victimes par tranche d'âge",
           x = "Tranche d'âge",
           y = "Nombre de victimes") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  })
  
  output$age_chi2_test <- renderPrint({
    df <- data_new()
    df <- df[df$Vict.Age > 0, ]
    df$tranche_age <- cut(df$Vict.Age, 
                          breaks = c(0, 17, 30, 45, 60, 100),
                          labels = c("0-17", "18-30", "31-45", "46-60", "61+"),
                          include.lowest = TRUE)
    
    table_victimes_age <- table(df$tranche_age)
    test_chi2 <- chisq.test(table_victimes_age)
    test_chi2
  })
  
  output$post_hoc_results <- renderPrint({
    data_age <- data.frame(
      tranche_age = c("0-17", "18-30", "31-45", "46-60", "61+"),
      nombre_victimes = c(9412, 76298, 91869, 52511, 29401)
    )
    total_victimes <- sum(data_age$nombre_victimes)
    post_hoc_results <- pairwise.prop.test(
      x = data_age$nombre_victimes,
      n = rep(total_victimes, length(data_age$nombre_victimes)),
      p.adjust.method = "bonferroni"
    )
    print(post_hoc_results)
  })
  
  output$geo_distribution <- renderPlot({
    df <- data_new()
    frequence_victimes <- df %>%
      group_by(AREA.NAME) %>%
      summarise(FREQUENCE_VICTIMES = n()) %>%
      ungroup()
    
    frequence_victimes$COLOR_SCALE <- rescale(frequence_victimes$FREQUENCE_VICTIMES)
    
    ggplot(frequence_victimes, aes(x = reorder(AREA.NAME, -FREQUENCE_VICTIMES), y = FREQUENCE_VICTIMES)) +
      geom_bar(stat = "identity", aes(fill = COLOR_SCALE)) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Fréquence des Victimes par Zone Géographique",
           x = "Zone Géographique",
           y = "Fréquence des Victimes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  output$geo_chi2_test <- renderPrint({
    df <- data_new()
    frequence_victimes <- df %>%
      group_by(AREA.NAME) %>%
      summarise(FREQUENCE_VICTIMES = n()) %>%
      ungroup()
    
    observed_frequencies <- frequence_victimes$FREQUENCE_VICTIMES
    test_chi_carre <- chisq.test(observed_frequencies)
    test_chi_carre
  })
  
  output$post_hoc_results1 <- renderPrint({
    df <- data_new()
    frequence_victimes <- df %>%
      group_by(AREA.NAME) %>%
      summarise(FREQUENCE_VICTIMES = n()) %>%
      ungroup()
    
    observed_frequencies <- frequence_victimes$FREQUENCE_VICTIMES
    results <- list()
    
    for (i in 1:(length(observed_frequencies) - 1)) {
      for (j in (i + 1):length(observed_frequencies)) {
        test_prop <- prop.test(c(observed_frequencies[i], observed_frequencies[j]),
                               n = c(sum(observed_frequencies), sum(observed_frequencies)))
        results[[paste0("Comparaison ", i, " vs ", j)]] <- test_prop$p.value
      }
    }
    
    p_values <- unlist(results)
    p_values_bonferroni <- p.adjust(p_values, method = "bonferroni")
    p_values_bonferroni
  })
  
  output$text_output1 <- renderText({
    "Interprétation : 
    Groupes 1 et 2 sont significativement différents de tous les autres groupes.
    Top des villes avec le plus de crimes : 
    Central	(200 690)
    Southwest	(160 592)
    77th Street	(160 366)"
  })
  
  # Définir data_new() avec la création de tranche_age
  data_new <- reactive({
    df <- read.csv("donnes.csv", header = TRUE, sep = ",")
    df[df == ""] <- NA
    
    # Nettoyage et transformations
    df <- df[,-c(1,2,5,8,11,21,22,23,24,26,27,28)]
    df$DATE.OCC <- as.POSIXct(df$DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
    df$Year <- as.numeric(format(df$DATE.OCC, "%Y"))
    df <- df %>% filter(Year > 2021)
    
    # Créer les tranches d'âge
    df <- df[df$Vict.Age > 0, ]
    df$tranche_age <- cut(df$Vict.Age, breaks = c(0, 17, 30, 45, 60, 100),
                          labels = c("0-17", "18-30", "31-45", "46-60", "61+"),
                          include.lowest = TRUE)
    
    return(df)
  })
  
  # Régression Logistique
  output$weapon_category_table <- renderPrint({
    df <- data_new()
    df$Weapon.Category <- sapply(df$Weapon.Desc, categorize_weapon)
    table(df$Weapon.Category)
  })
  
  output$crime_category_table <- renderPrint({
    df <- data_new()
    df$Crime_Category <- sapply(df$Crm.Cd.Desc, categorize_crime)
    table(df$Crime_Category)
  })
  
  output$logistic_regression_summary <- renderPrint({
    df <- data_new()
    
    # Création des variables pour la régression
    df$Vict.Sex.Encoded <- ifelse(df$Vict.Sex == "F", 0, ifelse(df$Vict.Sex == "M", 1, 2))
    df$moment <- with(df, ifelse(TIME.OCC >= 600 & TIME.OCC < 1200, "Matin",
                                 ifelse(TIME.OCC >= 1200 & TIME.OCC < 1800, "Aprem",
                                        ifelse(TIME.OCC >= 1800 & TIME.OCC < 2359, "Soir", "Nuit"))))
    df$Status.Binary <- ifelse(df$Status %in% c("AA", "CC", "JA"), 1, 0)
    
    # Appliquer les fonctions de catégorisation
    df$Weapon.Category <- sapply(df$Weapon.Desc, categorize_weapon)
    df$Crime_Category <- sapply(df$Crm.Cd.Desc, categorize_crime)
    
    # Sélection des variables pour le modèle
    data_regression <- df %>%
      select(Status.Binary, tranche_age, Vict.Sex.Encoded, moment, AREA.NAME, Weapon.Category, Crime_Category) %>%
      na.omit()
    
    # Transformation des variables en facteurs
    data_regression$moment <- as.factor(data_regression$moment)
    data_regression$AREA.NAME <- as.factor(data_regression$AREA.NAME)
    data_regression$Weapon.Category <- as.factor(data_regression$Weapon.Category)
    data_regression$Crime_Category <- as.factor(data_regression$Crime_Category)
    
    # Modèle de régression logistique
    modele <- glm(Status.Binary ~ tranche_age + Vict.Sex.Encoded + moment + AREA.NAME + Weapon.Category + Crime_Category, 
                  data = data_regression, 
                  family = binomial)
    
    # Résumé du modèle
    summary(modele)
  })
  
  # Visualisation des coefficients de la régression logistique
  output$logistic_regression_plot <- renderPlot({
    df <- data_new()
    df$Vict.Sex.Encoded <- ifelse(df$Vict.Sex == "F", 0, ifelse(df$Vict.Sex == "M", 1, 2))
    df$moment <- with(df, ifelse(TIME.OCC >= 600 & TIME.OCC < 1200, "Matin",
                                 ifelse(TIME.OCC >= 1200 & TIME.OCC < 1800, "Aprem",
                                        ifelse(TIME.OCC >= 1800 & TIME.OCC < 2359, "Soir", "Nuit"))))
    df$Status.Binary <- ifelse(df$Status %in% c("AA", "CC", "JA"), 1, 0)
    
    # Appliquer les fonctions de catégorisation
    df$Weapon.Category <- sapply(df$Weapon.Desc, categorize_weapon)
    df$Crime_Category <- sapply(df$Crm.Cd.Desc, categorize_crime)
    
    data_regression <- df %>%
      select(Status.Binary, tranche_age, Vict.Sex.Encoded, moment, AREA.NAME, Weapon.Category, Crime_Category) %>%
      na.omit()
    
    data_regression$moment <- as.factor(data_regression$moment)
    data_regression$AREA.NAME <- as.factor(data_regression$AREA.NAME)
    data_regression$Weapon.Category <- as.factor(data_regression$Weapon.Category)
    data_regression$Crime_Category <- as.factor(data_regression$Crime_Category)
    
    modele <- glm(Status.Binary ~ tranche_age + Vict.Sex.Encoded + moment + AREA.NAME + Weapon.Category + Crime_Category, 
                  data = data_regression, 
                  family = binomial)
    
    # Extraire et afficher les coefficients sous forme de barre
    coef_data <- as.data.frame(summary(modele)$coefficients)
    coef_data$Variable <- rownames(coef_data)
    ggplot(coef_data[-1,], aes(x = reorder(Variable, Estimate), y = Estimate)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Coefficients de la Régression Logistique", x = "Variable", y = "Coefficient") +
      theme_minimal()
  })
  
}

# Exécution de l'application
shinyApp(ui = ui, server = server)