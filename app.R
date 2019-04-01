#-------------------------------------------------------------------------
# APPLICACIÓN SHINY PARA LOESS
#-------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# Interfaz del usuario (User Interface - UI)
ui <- fluidPage(

    # Diseño con múltiples pestañas, la barra lateral es distinta en cada una
    navbarPage(
        
        # Título de la aplicación
        "Evaluación de parámetros en un ajuste loess",
        
        # La primera pestaña es para poner los datos de ejemplos
        tabPanel(
            "Datos de ejemplo",
            
            # Barra lateral con span y degree
            sidebarPanel(
                sliderInput(
                    inputId = "span",
                    label = HTML("Parámetro de suavizado (&alpha;)"),
                    min = 0,
                    max = 1,
                    value = 0.3
                ),
                radioButtons(
                    inputId = "degree",
                    label = HTML("Grado del polinomio ajustado (&lambda;)"),
                    choiceNames = 1:2,
                    choices = 1:2,
                    selected = 2
                )
            ),
            
            # Pestaña principal con el gráfico
            mainPanel(
                tabPanel(
                    "Datos de ejemplo",
                    plotOutput("ejemplo")
                )
            )
        ),
        
        # La segunda pestaña es para cargar un archivo de datos
        tabPanel(
            "Cargar archivo de texto con datos",
            
            # Barra lateral con opciones para leer el archivo, span y degree
            sidebarPanel(
                
                # Herramienta para seleccionar archivo
                h4("Opciones para importar archivo"),
                fileInput("file1", "Elegir archivo de texto",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),

                # Input: Checkbox if file has header
                checkboxInput("header", "Encabezado", TRUE),

                # Input: Select separator
                radioButtons("sep", "Separador",
                             choices = c(Espacio = " ",
                                         Coma = ",",
                                         `Punto y coma` = ";",
                                         Tabulación = "\t"),
                             selected = " "),

                # Input: Select quotes
                radioButtons("quote", "Comillas",
                             choices = c(No = "",
                                         "Dobles" = '"',
                                         "Simples" = "'"),
                             selected = '"'),

                # Input: Select number of rows to display
                radioButtons("disp", "Mostrar",
                             choices = c(Inicio = "head",
                                         Todo = "all"),
                             selected = "head"),
                
                # Seleccionar variables x e y, esto es dinamico, depende del dataset leido
                uiOutput("varX"),
                uiOutput("varY"),
                
                # Linea horizontal para separar
                tags$hr(),
                
                # Controles para el loess como en la otra pestaña
                h4("Parámetros para loess"),
                sliderInput(
                    inputId = "span2",
                    label = HTML("Parámetro de suavizado (&alpha;)"),
                    min = 0,
                    max = 1,
                    value = 0.3
                ),
                radioButtons(
                    inputId = "degree2",
                    label = HTML("Grado del polinomio ajustado (&lambda;)"),
                    choiceNames = 1:2,
                    choices = 1:2,
                    selected = 2
                )
            ),
            
            # Pestaña principal con el gráfico
            mainPanel(
                tabPanel(
                    "Gráfico",
                    tableOutput("contents"),
                    plotOutput("propios")
                )
            )
        ),
        
        # La tercera pestaña es para poner la animación
        tabPanel(
            "Cómo funciona loess",
            p("La curva azúl es el ajuste loess con span = 0.3 y degree = 1, es decir, para realizar el ajuste, en cada punto del eje x se busca el 30% de los valores observados de x más cercanos y se ajusta con ellos una recta."),
            p("La recta se ajusta con mínimos cuadrados ponderados, de modo que el 30% de puntos a usar, no todos tienen la misma importancia. Los pesos son proporcionales a la distancia con respecto al valor a evaluar y están representados en el esquema con una escala de color gris."),
            p("El valor ajustado por loess es el valor estimado por la recta obtenida de esa forma para cada valor de x."),
            div(img(img(src='loess1.gif', height = 500, width = 800)), style="text-align: center;"),
            div(img(img(src='loess2.png', height = 500, width = 800)), style="text-align: center;")
            
        )
    ),
    
    hr(),
    print("~~~ Shiny App creada por Marcos Prunello para el Taller de Análisis Exploratorio - LEST/FCEYE/UNR 2019 ~~~")
)

# Servidor: código de R que tiene que ejecutar el servidor
server <- function(input, output) {
   

    # Crear datos de ejemplo y el gráfico
    output$ejemplo <- renderPlot({
        
        # Crear dataset de prueba
        set.seed(20)
        x = seq(5, 10, length = 150)
        z = seq(0, 1, length = 50)
        y = 5 + c(-(1 - z)^.5, sin(z * 2 * pi), .5 * z^2)
        fit = loess.smooth(x, y, degree = 2, family = "g", span = .5, evaluation = 150)
        datos2 = tibble(
            y = fit$y + rnorm(150, 0, .2),
            x = fit$x
        )
        
        ggplot(datos2, aes(x = x, y = y)) + 
            geom_point() + 
            stat_smooth(method = "loess", se = FALSE, span = input$span, method.args = list(degree = input$degree))
    }, height = 400, width = 600)
    
    # Cargar datos propios
    # Leer el archivo
    df <- reactive({
        req(input$file1)
        read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)  
        
    })
    
    # Dynamically generate UI input when data is uploaded
    output$varX <- renderUI({
        varSelectInput("vbleX", "Variable X", df())
    })
    output$varY <- renderUI({
        varSelectInput("vbleY", "Variable Y", df())
    })

    # Preparar para mostrar el dataset
    output$contents <- renderTable({
        if(input$disp == "head") {
            return(head(df()))
        }
        else {
            return(df())
        }
    })
    
    # El grafico
    output$propios <- renderPlot({
        ggplot(df(), aes_string(x = input$vbleX, y = input$vbleY)) +
            geom_point() +
            stat_smooth(method = "loess", se = FALSE, span = input$span2, method.args = list(degree = input$degree2))

    }, height = 400, width = 600)
    
}

# Run the application 
shinyApp(ui = ui, server = server)

