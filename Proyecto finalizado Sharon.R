#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


library(dplyr)
library(lubridate)
library(countrycode)
library(ggplot2)

ui <- fluidPage(                                                                #parte visual
  titlePanel("Análisis de Votos - Algunos Datos"),                              #titulo
  sidebarLayout(                                                                #tabla donde veo informaciones
    sidebarPanel(
      checkboxGroupInput(                                                       #boton de seleccion
        inputId = "tabla_select",                                               #caja de check
        label = "Selecciona la tabla a mostrar:",
        choices = list("Votos Sí" = "votes_si","Votos por País" = "votes_country")
      ),
      checkboxGroupInput(
        inputId = "grafico_select",
        label = "Selecciona el gráfico a mostrar:",
        choices = list( "Gráfico por País" = "grafico_country")
      )
    ),
    mainPanel(
      tableOutput("tabla_output"),
      plotOutput("grafico_output")
    )
  )
)

server <- function(input, output) {
  
  
  votes <- readRDS("C:/Users/araya/Downloads/votes.rds")
  
  
  votes <- head(votes, 10000)
  
  
  votes_si <- filter(votes, vote == 1)
  
  
  
  votes <- mutate(votes, country = countrycode(ccode, origin = "cown", destination = "country.name"))
  
  votes <- votes %>%
    mutate(country = recode(country,
                            "United States of America" = "United States",
                            "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom"))
  
  
  
  votes_country <- votes %>%
    mutate(yes = ifelse(vote == 1, 1, 0)) %>%
    group_by(country) %>%
    summarise(total_yes = sum(yes),
              percentage_yes = (sum(yes) / n()) * 100) %>%
    arrange(desc(percentage_yes)) %>%
    slice_head(n = 20)  
  
  
  output$tabla_output <- renderTable({
    if (length(input$tabla_select) == 0) {    return(NULL) 
    }
    selected_tables <- lapply(input$tabla_select, function(tbl) {
      switch(tbl,
             "votes_si" = votes_si,
             "votes_year" = votes_year,
             "votes_country" = votes_country)
    })
    do.call(rbind, selected_tables) })
  
  
  output$grafico_output <- renderPlot({
    if (length(input$grafico_select) == 0) {
      return(NULL) }
    
    
    if ("grafico_country" %in% input$grafico_select) {
      ggplot(data = votes_country, aes(x = reorder(country, -percentage_yes), y = percentage_yes)) +
        geom_bar(stat = "identity", fill = "pink") +
        labs(title = "Porcentaje de Votos Sí por País (Primeros 20 Países)", x = "País", y = "Porcentaje") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })
}
shinyApp(ui = ui, server = server)