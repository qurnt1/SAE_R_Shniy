library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(readr)
#zizi
# Chargement des données
data <- read_csv2("data.csv", skip = 2)  # Suppression des 2 premières lignes
colnames(data)[1] <- "Code"  # Renommer la première colonne si nécessaire

# Convertir 'Code' en apigné et gérer les erreurs de conversion
data$Code <- as.numeric(data$Code)

# Exclure les DOM-TOM (codes département > 95 ou NA)
data <- data %>% filter(!is.na(Code) & Code <= 95)

# UI de l'application
ui <- fluidPage(
  titlePanel("Suivi de la Pauvreté en France"),
  
  # Table avec les données
  DTOutput("table"),
  
  # Graphique du taux de pauvreté
  plotOutput("plot_pauvrete")
)

# Serveur de l'application
server <- function(input, output) {
  
  # Tableau des données
  output$table <- renderDT({
    datatable(data, options = list(pageLength = 10))
  })
  
  # Graphique du taux de pauvreté
  output$plot_pauvrete <- renderPlot({
    ggplot(data, aes(x = Libellé, y = `Taux de pauvreté 2021`)) +
      geom_bar(stat = "identity", fill = "red") +
      theme_minimal() +
      ggtitle("Taux de Pauvreté par Département") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

# ta mere la pute
shinyApp(ui, server)

