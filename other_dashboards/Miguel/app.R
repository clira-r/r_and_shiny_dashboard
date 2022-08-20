# install.packages("shinycssloaders")
library(shiny)
library(ggplot2)
library(dplyr)
library(shinycssloaders)

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  title = "Proyecto final",
  lang = "es",
  pageWithSidebar(
    headerPanel("Datos de los equipos de la liga española de fútbol"),
    sidebarPanel(
      p("<Drescripcion>")
    ),
    mainPanel(
      tabsetPanel(
        type = "pills",
        tabPanel(
          title = "Gráficas de goles",
          icon = icon("glyphicon glyphicon glyphicon-stats", lib = "glyphicon"),
          br(),
          fluidRow(
            column(
              4,
              selectInput(
                inputId = "team.type",
                label = "Seleccione el tipo de equipo",
                choice = c("Local", "Visitante")
              )
            ),
            column(
              4,
              uiOutput("team.select")
            )
          ),
          textOutput("description"),
          shinycssloaders::withSpinner(
            plotOutput("bar.grafics", "auto", 700)
          )
        ),
        tabPanel(
          title = "Predicciones",
          icon = icon("glyphicon glyphicon glyphicon-signal", lib = "glyphicon")
        ),
        tabPanel(
          title = "Summary",
          icon = icon("glyphicon glyphicon glyphicon-stats", lib = "glyphicon"),
          dataTableOutput("match.data")
        ),
        tabPanel(
          title = "Table",
          icon = icon("glyphicon glyphicon-list-alt", lib = "glyphicon")
        ),
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- read.csv("./www/match.data.csv", header = T)
  data <- na.omit(data)
  data <- mutate(
    data,
    FTR = ifelse(
      home.score > away.score,
      "Local",
      ifelse(
        home.score < away.score,
        "Visitante",
        "Empate"
      )
    )
  )
  output$team.select = renderUI({
    teams <- unique(select(data, home.team))
    teams <- teams[order(teams),]
    teams <- append(c("Todos los equipos"), teams)
    selectInput(
      inputId = "team.name",
      label = "Seleccione el equipo",
      choice = teams
    )
    
  })
  
  output$bar.grafics <- renderPlot({
    team <- ifelse("Local" == input$team.type, "home.score", "away.score")
    show.one.team <- !is.null(input$team.name) && input$team.name != "Todos los equipos"

    
    if (show.one.team) {
       data <- filter(data, home.team == input$team.name)
    }
    
    x <- data[, team]
    
    data %>% ggplot(aes(x, fill = FTR)) + 
      geom_bar() + 
      { if (!show.one.team) facet_wrap("away.team") } +
      labs(x =input$team.type, y = "Goles") + 
      ylim(0,50)
  })

  output$description <- renderText({
    team.name <- ifelse(
      !is.null(input$team.name),
      input$team.name,
      "Todos los equipos"
    )
    
    team.txt <- ifelse(
      team.name != "Todos los equipos",
      paste(team.name, "participó"),
      paste(tolower(team.name), "participaron")
    )
    
    paste(
      "Total de goles anotados donde",
      team.txt,
      "como equipo",
      tolower(input$team.type),
      "."
    )
  })
  
  output$match.data <- renderDataTable(
    { data },
    options = list(
      processing = TRUE,
      lengthMenu = c(10, 30, 50, 100),
      pageLength = 10,
      columns = list(
        list(title = "Fecha"),
        list(title = "Equipo local"),
        list(title = "Goles local"),
        list(title = "Equipo visitante"),
        list(title = "Goles visitante"),
        list(title = "Resultado partido")
      )
    )
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
