# Load R packages
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

setwd("C:/Users/clira/Documents/_TR In Progress/Curso Data Science (BEDU)/Fase 2/Módulo 1/Sesión 8/Post-Work")


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
    tabPanel("Stats", icon = icon('chart-column'),
             titlePanel(h4("Estadísticas de Visitante vs Local", align = "center")),
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
             
    ), #tabPanel_1
    
    #tabPanel_2
    tabPanel("PW3|Graphs", icon = icon('images'), 
             titlePanel(h4("Gráficas Postwork 3", align = "center")),
             fluidRow(
               h3("Probabilidades marginales de los goles del equipo local", align = "left"),
               tags$figure(align = "center",
                           tags$img(src = "fotos/Pw31.png")),
               h3("Probabilidades marginales de los goles del equipo visitante", align = "left"),
               tags$figure(align = "center",
                           tags$img(src = "fotos/Pw32.png")),
               h2("Heatmap probabilidades conjuntas", align = "center"),
               tags$figure(align = "center",
                           tags$img(src = "fotos/Pw33.png"))
             )
    ),#tabPanel_2
    
    #tabPanel_3
    tabPanel("RawData", icon = icon('table'), 
             titlePanel(h4("Datos en Bruto", align = "center")),
             "This panel is intentionally left blank"),
    
    #tabPanel_4
    tabPanel("Gains", icon = icon('chart-line'),
             titlePanel(h4("Ganancias estimadas", align = "center")),
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