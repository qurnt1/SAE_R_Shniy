library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(readr)

# Chargement des données
data <- read_csv2("data.csv", skip = 2)  # Suppression des 2 premières lignes
colnames(data)[1] <- "Code"  # Renommer la première colonne si nécessaire

data$Code <- as.numeric(data$Code)

# Exclure les DOM-TOM (codes département > 95 ou NA)
data <- data %>% filter(!is.na(Code) & Code <= 95)

# Identifier les top 5 départements les plus pauvres et les plus riches
top_pauvres <- data %>% 
  arrange(desc(`Taux de pauvreté 2021`)) %>% 
  slice(1:5)

top_riches <- data %>% 
  arrange(`Taux de pauvreté 2021`) %>% 
  slice(1:5)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse de la pauvreté en France"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Choisissez une catégorie :",
                  choices = list(
                    "Profil de la pauvreté" = "profil",
                    "Accès à l’éducation et au logement" = "education_logement",
                    "Conditions de logement et type d’habitat" = "habitat",
                    "Revenus et aides sociales" = "revenus_aides",
                    "Démographie et emploi public" = "demographie_emploi",
                    "Économie et activité professionnelle" = "economie",
                    "Services et infrastructures publiques" = "services",
                    "Infrastructures sportives et éducatives" = "infrastructures"
                  ))
    ),
    mainPanel(
      uiOutput("tabs")
    )
  )
)

# Serveur
server <- function(input, output) {
  output$tabs <- renderUI({
    if (input$category == "profil") {
      tabsetPanel(
        tabPanel("Répartition des pauvres", 
                 radioButtons(
                   inputId = "filter_choice",
                   label = "Afficher :",
                   choices = c("Top 5 départements les plus pauvres" = "pauvres",
                               "Top 5 départements les plus riches" = "riches"),
                   selected = "pauvres"
                 ),
                 DTOutput("top_table"),
                 plotOutput("plot_age")),
        tabPanel("Familles monoparentales", plotOutput("plot_familles")),
        tabPanel("Locataires", plotOutput("plot_locataires"))
      )
    } else if (input$category == "education_logement") {
      tabsetPanel(
        tabPanel("Niveau d'éducation", "Graphique / Tableau du niveau de formation"),
        tabPanel("Résidences principales", "Graphique / Tableau de la part des résidences principales"),
        tabPanel("Logements vacants", "Graphique / Tableau de la part des logements vacants")
      )
    } else if (input$category == "habitat") {
      tabsetPanel(
        tabPanel("Locataires HLM", "Graphique / Tableau de la part des locataires HLM"),
        tabPanel("Maisons/Appartements", "Graphique / Tableau de la part des maisons et appartements")
      )
    } else if (input$category == "revenus_aides") {
      tabsetPanel(
        tabPanel("Ménages fiscaux imposés", "Graphique / Tableau de la part des ménages fiscaux imposés"),
        tabPanel("Prestations sociales", "Graphique / Tableau de la part des prestations sociales")
      )
    } else if (input$category == "demographie_emploi") {
      tabsetPanel(
        tabPanel("Démographie", "Graphique / Tableau sur la démographie"),
        tabPanel("Effectifs publics", "Graphique / Tableau sur l'administration publique, santé, éducation")
      )
    } else if (input$category == "economie") {
      tabsetPanel(
        tabPanel("Secteurs d'activité", "Graphique / Tableau de la répartition des entreprises par secteur d'activité")
      )
    } else if (input$category == "services") {
      tabsetPanel(
        tabPanel("Police/Gendarmerie, Banques, Grandes surfaces", "Graphique / Tableau du nombre de services"),
        tabPanel("Urgences/Médecins", "Graphique / Tableau du nombre de services d'urgence et médecins"),
        tabPanel("Pharmacies/Infirmiers", "Graphique / Tableau du nombre de pharmacies et infirmiers")
      )
    } else if (input$category == "infrastructures") {
      tabsetPanel(
        tabPanel("Bassins/Salles multisports", "Graphique / Tableau du nombre d'infrastructures sportives"),
        tabPanel("Écoles/Collèges/Lycées", "Graphique / Tableau du nombre d'établissements scolaires")
      )
    }
  })
  
  output$top_table <- renderDT({
    if (input$filter_choice == "pauvres") {
      datatable(top_pauvres, options = list(pageLength = 5))
    } else {
      datatable(top_riches, options = list(pageLength = 5))
    }
  })
  
  output$plot_age <- renderPlot({
    selected_data <- if (input$filter_choice == "pauvres") top_pauvres else top_riches
    ggplot(selected_data, aes(x = Libellé, y = Population_municipale_2022, fill = Libellé)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      ggtitle("Répartition par tranche d'âge") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$plot_familles <- renderPlot({
    selected_data <- if (input$filter_choice == "pauvres") top_pauvres else top_riches
    ggplot(selected_data, aes(x = Libellé, y = `Taux de pauvreté - familles monoparentales 2021`, fill = Libellé)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme_minimal() +
      ggtitle("Taux de pauvreté chez les familles monoparentales") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$plot_locataires <- renderPlot({
    selected_data <- if (input$filter_choice == "pauvres") top_pauvres else top_riches
    ggplot(selected_data, aes(x = Libellé, y = `Taux de pauvreté - locataires 2021`, fill = Libellé)) +
      geom_bar(stat = "identity", fill = "green") +
      theme_minimal() +
      ggtitle("Taux de pauvreté chez les locataires") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Exécution de l'application
shinyApp(ui = ui, server = server)
