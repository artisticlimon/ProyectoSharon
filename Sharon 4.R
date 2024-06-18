
library(shiny)
library(dplyr)
library(countrycode)


ui <- fluidPage(
  titlePanel("Análisis de Votos -  Datos"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "tabla_select",
        label = "Selecciona la tabla a mostrar:",
        choices = list(
          "Votos Sí" = "votes_si", 
          
          
          "Votos por Año" = "votes_year",
          "Votos por País" = "votes_country"
        )
      )
    ),
    mainPanel(
      tableOutput("tabla_output")
    )
  )
)


server <- function(input, output) {
  
  
  votes <- readRDS("C:/Users/Emilio/Downloads/votes.rds")
  
  
  votes <- votes %>% mutate(year = session + 1945)
  
  votes <- votes %>%
    mutate(country = countrycode(ccode, origin = "cown", destination = "country.name")) %>%
    mutate(country = recode(country,
                            "United States of America" = "United States",
                            "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom"))
  
  
  votes_si <- filter(votes, vote == 1)
  
  votes_year <- votes %>%
    mutate(yes = ifelse(vote == 1, 1, 0)) %>%
    group_by(year) %>%
    summarise(
      suma = sum(yes),
      porcentaje = (sum(yes) / n()) * 100,
      promedio = (sum(yes) / n())
    )
  


