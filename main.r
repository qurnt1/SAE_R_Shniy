library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(readr)

# Chargement des données
data <- read_csv2("data.csv", skip = 2)  # Suppression des 2 premières lignes
colnames(data)[1] <- "Code"  # Renommer la première colonne si nécessaire

# Convertir 'Code' en numérique et gérer les erreurs de conversion
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

# UI de l'application
ui <- fluidPage(
  titlePanel("Suivi de la Pauvreté en France"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "filter_choice",
        label = "Afficher :",
        choices = c("Top 5 départements les plus pauvres" = "pauvres",
                    "Top 5 départements les plus riches" = "riches"),
        selected = "pauvres"
      )
    ),
    
    mainPanel(
      # Tableau du top 5 sélectionné
      DTOutput("top_table"),
      
      # Analyses supplémentaires
      h3("Caractérisation des départements"),
      h4("Combien de pauvres ? Quelle répartition par tranche d’âge ?"),
      plotOutput("plot_age"),
      
      h4("Quel taux de pauvreté chez les familles monoparentales ?"),
      plotOutput("plot_familles"),
      
      h4("Quel taux de pauvreté chez les locataires ?"),
      plotOutput("plot_locataires")
    )
  )
)

# Serveur de l'application
server <- function(input, output) {
  
  # Tableau dynamique basé sur le choix de l'utilisateur
  output$top_table <- renderDT({
    if (input$filter_choice == "pauvres") {
      datatable(top_pauvres, options = list(pageLength = 5))
    } else {
      datatable(top_riches, options = list(pageLength = 5))
    }
  })
  
  # Répartition par tranche d'âge
  output$plot_age <- renderPlot({
    selected_data <- if (input$filter_choice == "pauvres") top_pauvres else top_riches
    ggplot(selected_data, aes(x = Libellé, y = Population_municipale_2022, fill = Libellé)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      ggtitle("Répartition par tranche d'âge") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Taux de pauvreté chez les familles monoparentales
  output$plot_familles <- renderPlot({
    selected_data <- if (input$filter_choice == "pauvres") top_pauvres else top_riches
    ggplot(selected_data, aes(x = Libellé, y = `Taux de pauvreté - familles monoparentales 2021`, fill = Libellé)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme_minimal() +
      ggtitle("Taux de pauvreté chez les familles monoparentales") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Taux de pauvreté chez les locataires
  output$plot_locataires <- renderPlot({
    selected_data <- if (input$filter_choice == "pauvres") top_pauvres else top_riches
    ggplot(selected_data, aes(x = Libellé, y = `Taux de pauvreté - locataires 2021`, fill = Libellé)) +
      geom_bar(stat = "identity", fill = "green") +
      theme_minimal() +
      ggtitle("Taux de pauvreté chez les locataires") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)