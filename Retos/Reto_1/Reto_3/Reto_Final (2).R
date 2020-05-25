library(shiny)
library(plotly)
library(deSolve)
library(csv)


datos <- read.csv("../Downloads/EEUU coronavirus.csv", header = TRUE, sep = ";")


if (interactive()) {
  ui <- fluidPage(
    tabsetPanel(
       tabPanel("SIR", 
       headerPanel("Modelo SIR"),
       sliderInput("beta",
                   "Tasa de Transmision(beta):",
                   min = 0,
                   max = 3,
                   value = 1.5,
                   step=0.05),
       sliderInput("gamma",
                   "Tasa de recuperacion(gamma):",
                   min = 0,
                   max = 3,
                   value = 0.3,
                   step=0.05),
       sliderInput("bins",
                   "Tiempo max:",
                   min = 1,
                   max = 58,
                   value = 58,
                   step = 0.5),
       sliderInput("pob",
                   "Población(En millones):",
                   min = 100,
                   max = 331,
                   value = 331,
                   step = 1),
       selectInput("sel", label = "Metodo", choices = list("euler", "rk4","adams","radau")),
       tabPanel("Datos", plotOutput("distplot")),
       tabPanel("sir", dataTableOutput("pl1"))
    ),
    tabPanel("SI", 
             headerPanel("Modelo SI"),
             sliderInput("betas",
                         "Tasa de Transmision(beta):",
                         min = 0,
                         max = 3,
                         value = 1.5,
                         step=0.05),
             sliderInput("binss",
                         "Tiempo max(En dias):",
                         min = 1,
                         max = 58,
                         value = 58,
                         step = 0.5),
             sliderInput("pobs",
                         "Población(En millones):",
                         min = 100,
                         max = 331,
                         value = 331,
                         step = 1),
             selectInput("sels", label = "Metodo", choices = list("euler", "rk4","adams","radau")),
             tabPanel("Datoss", plotOutput("plot2")),
             tabPanel("si", dataTableOutput("pl2"))
    ),
    tabPanel("Graficas",
             headerPanel('Coronavirus USA'),
             sidebarPanel(
             selectInput('opc','escoja una opcion', names(datos)),
             selected = names(datos)[3]),
             tabPanel("grap",plotlyOutput('plot')),
            ),
    tabPanel("Errores",
             headerPanel('Errores'),
             sidebarPanel()
               
          )
    )
  )
  
  server <- function(input, output) {

    output$distplot <- renderPlot({
      #x = faithful[, 2]
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      #plot(x, breaks = bins)
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      library(deSolve)
      #tamaño poblacional
      N = input$pob
      #estado inicial de los compartimentos
      init <- c(S = 1-1e-6,
                I = 1e-6,
                R = 0)
      #parámetros del modelo (coeficientes de las variables)
      print(input$bins)
      
      param <- c(beta = input$beta, #infectados
                 gamma = input$gamma) #recuperacion
      
      
      #crear la función con las ODE
      sir <- function(times, init, param) {
        with(as.list(c(init, param)), {
          #ecuaciones diferenciales
          dS <- -beta * S * I
          dI <-  beta * S * I - gamma * I
          dR <-                 gamma * I
          #resultados de las tasas de cambio
          return(list(c(dS, dI, dR)))
        })
      }
      #intervalo de tiempo y resolución
      times <- seq(0, input$bins, by = 1)
      #resolver el sistema de ecuaciones con función 'ode'
      out <- ode(y = init, times = times, func = sir, parms = param, method = input$sel)
      #cambiar out a un data.frame
      out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
      #eliminar la variable 'time' en out
      out$time <- NULL
      #mostrar 10 primeros datos
      head(out, 10)
      
      #gráfica
      matplot(x = times, y = out, type = "l",
              xlab = "Tiempo", ylab = "S, I, R", main = "Modelo SIR básico",
              lwd = 2, lty = 1, bty = "l", col = c("deepskyblue4","firebrick3","green2"))
      #añadir leyenda de líneas
      legend(50, 0.7, c("Susceptibles", "Infectados", "Recuperados"),
             pch = 16, col = c("deepskyblue4","firebrick3","green2"), bty = "n", cex = 1)
    })
    
      output$plot2 <- renderPlot({
      #x = faithful[, 2]
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      #plot(x, breaks = bins)
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      library(deSolve)
      #tamaño poblacional
        #tamaño poblacional
        N = input$pobs
        #estado inicial de los compartimentos
        init <- c(S = 1-1e-6,
                  I = 1e-6
        )
        #parámetros del modelo (coeficientes de las variables)
        param <- c(beta = input$betas,
                   gamma = 0.14286)
        #crear la función con las ODE
        sir <- function(times, init, param) {
          with(as.list(c(init, param)), {
            #ecuaciones diferenciales   
            dS <- -beta * S * I
            dI <-  beta * S * I
            #resultados de las tasas de cambio    
            return(list(c(dS, dI)))
          })
        }
        #intervalo de tiempo y resolución
        times <- seq(0, input$binss, by = 1)
        #resolver el sistema de ecuaciones con función 'ode'
        out <- ode(y = init, times = times, func = sir, parms = param, method = input$sels)
        #cambiar out a un data.frame
        out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
        #eliminar la variable 'time' en out
        out$time <- NULL
        #mostrar 10 primeros datos
        head(out, 100)
        
        tablita = out[, - c(1,2)]
        
        matplot(x = times, y = out, type = "l",
                xlab = "Tiempo", ylab = "S, I", main = "Modelo SI básico",
                lwd = 1, lty = 1, bty = "l", col = 2:3)
        #añadir leyenda de líneas
        legend(40, 0.7, c("Susceptibles", "Infectados"),
               pch = 1, col = 2:4, bty = "n", cex = 1)
    })
    
    x <- reactive({
      datos[,"Dia"]
    })
    
    y <- reactive({
      datos[,input$opc]
    })
    
    output$plot <- renderPlotly(
      plot1 <- plot_ly(
        x = x(),
        y = y(), 
        type = 'scatter',
        mode = 'lines')%>% 
        layout(title = input$opc)
    )
    
    
    output$pl1 <-renderDataTable({
      
      N = input$pob
      #estado inicial de los compartimentos
      init <- c(S = 1-1e-6,
                I = 1e-6,
                R = 0)
      #parámetros del modelo (coeficientes de las variables)
      print(input$bins)
      
      param <- c(beta = input$beta,
                 gamma = input$gamma)
      
      
      #crear la función con las ODE
      sir <- function(times, init, param) {
        with(as.list(c(init, param)), {
          #ecuaciones diferenciales
          dS <- -beta * S * I
          dI <-  beta * S * I - gamma * I
          dR <-                 gamma * I
          #resultados de las tasas de cambio
          return(list(c(dS, dI, dR)))
        })
      }
      #intervalo de tiempo y resolución
      times <- seq(0, input$bins, by = 1)
      #resolver el sistema de ecuaciones con función 'ode'
      out <- ode(y = init, times = times, func = sir, parms = param, method = input$sel)
      #cambiar out a un data.frame
      out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
      #eliminar la variable 'time' en out
      out$time <- NULL
      #mostrar 10 primeros datos
      aux = datos[,-c(1,2,4,7)]
      errs = abs(out[,"S"] - (aux[,"Susceptibles"]/1000000))
      erri = abs(out[,"I"] - (aux[,"Casos.nuevos"]/1000000))
      errr = abs(out[,"R"] - (aux[,"Recuperados"]/1000000))
      tabla = cbind(out,aux/1000000,errs,erri,errr)
        #colnames(tabla) = c("Tiempo", "Suceptibles (% de poblacion)")
        tabla
    })
    
    output$pl2 <-renderDataTable({
      #x = faithful[, 2]
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      #plot(x, breaks = bins)
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      #tamaño poblacional
      #tamaño poblacional
      N = input$pobs
      #estado inicial de los compartimentos
      init <- c(S = 1-1e-6,
                I = 1e-6
      )
      #parámetros del modelo (coeficientes de las variables)
      param <- c(beta = input$betas,
                 gamma = 0.14286)
      #crear la función con las ODE
      sir <- function(times, init, param) {
        with(as.list(c(init, param)), {
          #ecuaciones diferenciales   
          dS <- -beta * S * I
          dI <-  beta * S * I
          #resultados de las tasas de cambio    
          return(list(c(dS, dI)))
        })
      }
      #intervalo de tiempo y resolución
      times <- seq(0, input$binss, by = 1)
      #resolver el sistema de ecuaciones con función 'ode'
      out <- ode(y = init, times = times, func = sir, parms = param, method = input$sels)
      #cambiar out a un data.frame
      out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
      #eliminar la variable 'time' en out
      out$time <- NULL
      #mostrar 10 primeros datos
      aux = datos[,-c(1,2,4,6,7)]
      errs = abs(out[,"S"] - (aux[,"Susceptibles"]/1000000))
      erri = abs(out[,"I"] - (aux[,"Casos.nuevos"]/1000000))
      tabla = cbind(out,aux/1000000, errs,erri)
      #colnames(tabla) = c("Tiempo", "Suceptibles (% de poblacion)")
      tabla
    })
    
  }
  shinyApp(ui,server)
}

#REFERENCIAS UTILIZADAS
#https://coronavirus.1point3acres.com/en
#PREENTADO POR:
#VICTOR BARON - OSCAR FALLA - NICOLAS GIL - ESTEFANIA ARISTIZABAL
