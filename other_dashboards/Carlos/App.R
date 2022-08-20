# Load R packages
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
  #test de Git-push
  # Define UI
  ui <- fluidPage(
    theme = bs_theme(
      bg = "#0c45a6", fg = "white", primary = "#FCC780",
      base_font = font_google("Space Mono"),
      code_font = font_google("Space Mono")
    ),     
    navbarPage(
      #"DS_F2 | Team#19",
      span("DS_F2|Team#19", style="color:black;font-weight: bold;"),
      #tabPanel_1
      tabPanel("Stats", icon = icon('bar-chart'),
               titlePanel(h4("Estadísticas de Visitante vs Local")),
               fluidRow(div(style = "height:15px;")),
               
               sidebarPanel(
                 selectInput("x", "Seleccione el valor del eje de las X",
                             choices = c("home.score", "away.score"))
                 , width = 7
               ), # sidebarPanel
               mainPanel( 
                 fluidRow(div(style = "height:25px;")),
                 plotOutput("plotGoles", height = 450, width = 750),
                 br("")
               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      
      #tabPanel_2
      tabPanel("PW3|Graphs", icon = icon('images'), 
               titlePanel(h4("Número de goles a favor y en contra por equipo:")),
               "This panel is intentionally left blank"),
      
      #tabPanel_3
      tabPanel("RawData", icon = icon('table'), 
               titlePanel(h4("Datos en Bruto")),
               "This panel is intentionally left blank"),
      
      #tabPanel_4
      tabPanel("Gains", icon = icon('chart-line'),
               titlePanel(h4("Ganancias estimadas")),
               "This panel is intentionally left blank")
      
  
    ) # navbarPage
  ) # fluidPage

  
  
  
  # Define server function  
  server <- function(input, output) {

    #carga de datos
    datosLiga <- read.csv("match.data.csv", header = T)
    
    #para tabPanel_1
    output$plotGoles <- renderPlot({
      data <-  read.csv("match.data.csv", header = T)
      #Los codigos de ganador: 
      #H <- equipo de casa
      #A <- equipo visitante
      #D <- empate
      data <- mutate(data, FTR = ifelse(home.score > away.score, "H", 
                                        ifelse(home.score < away.score, 
                                               "A", "D")))
      x <- data[,input$x]
      
      data %>% ggplot(aes(x, fill = FTR)) + 
        geom_bar() + 
        facet_wrap("away.team") +
        labs(x =input$x, y = "Goles") + 
        ylim(0,50)
    })
    
  }
  
  # Create Shiny object
  shinyApp(ui = ui, server = server)
