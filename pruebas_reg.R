library(shiny)
library(dplyr)
library(ggplot2)
library(car)

datos <- read.csv()
base_manofactura <- datos %>% 
    select(A111A.Producción.bruta.total..millones.de.pesos., 
           A131A.Valor.agregado.censal.bruto..millones.de.pesos.,
           K000A.Total.de.gastos.por.consumo.de.bienes.y.servicios..millones.de.pesos.,
           Q000A.Acervo.total.de.activos.fijos..millones.de.pesos.,
           A221A.Formación.bruta.de.capital.fijo..millones.de.pesos.,
           UE.Unidades.económicas, H001A.Personal.ocupado.total,
           H001D.Horas.trabajadas.por.personal.ocupado.total..miles.de.horas.,
           J000A.Total.de.remuneraciones..millones.de.pesos.)

colnames(base_manofactura) <- c('PBT', 'VAC', 'TGC', 'ATA', 'FBC', 'UE',
                                             'POT', 'HTP', 'TR')

base_m2 <- base_manofactura %>% filter(POT < 30000,
                                       TGC < 100000,
                                       VAC < 4000)        
mod <- lm(formula = PBT ~ . - 1, data = base_m2)


correr_modelo <- function(datos, parametros){
    modelo <- lm(formula = PBT ~ . - 1, data = datos[ ,1:(parametros + 1)])
    return(modelo)
}

grafico_htc <- function(modelo_selec){
    marco_datos <- tibble(Ajustados = modelo_selec$fitted.values, 
                          Residuos = modelo_selec$residuals)
    ggplot(marco_datos) + geom_point(aes(x = Ajustados, y = Residuos), col = 'magenta') + 
        ggtitle('Gráfico de heterocedasticidad') + theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
}



grafico_lin <- function(datos, posicion){
    datos_graf <- datos[,c(1, posicion)]
    ggplot(data = datos_graf) + 
        geom_point(aes(x = datos_graf[,2], y = datos_graf[,1]), col = 'turquoise') + 
        ggtitle('Gráfico de linealidad') + xlab(names(datos_graf)[2]) + 
        ylab(names(datos_graf[1])) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) 
}

#grafico_mul <- function(datos, posicion){
 #    datos_graf <- datos[, 1:(posicion + 1)]
  #  scatterplotMatrix(datos_graf)
     #}


grafico_nores <- function(modelo){
    vector_res <- tibble(Residuos = modelo$residuals)
    ggplot(data = vector_res) + geom_histogram(aes(Residuos), 
                                               col = 'powderblue', fill = 'slategray1',
                                               bins = 10) + 
        theme_bw() + scale_fill_manual(values=c( "powderblue")) +
        ggtitle('Histograma de residuos') + xlab('') +
        ylab('Frecuencia') +
        theme(plot.title = element_text(hjust = 0.5))
}



#user interface
ui <- fluidPage(
    #a row on top of the plot
    fluidRow(
        #within the row two columns. shiny apps always based on a width of 12.
        #slider in the second column in top row based
        column(
            width = 6,
            #a box of all the possilbe countries that the user can select from
            selectInput(inputId = 'prueba', label = 'Seleccionar prueba',
                        choices = unique(c('','Heterocedasticidad', 'Linealidad',
                                           'Normalidad de errores')))
        ),
        
        
        column(
            width = 6,
            sliderInput(inputId = "parametros", label = "Numero de parametros",
                        value = 1, step = 1,
                        min = 1, max = 8, sep = "")
        )
    ),
    plotOutput(outputId = "grafica_inter")
)

server <- function(input, output){
    output$grafica_inter <- renderPlot({
        if(input$prueba == 'Heterocedasticidad'){
            modelo <- correr_modelo(datos = base_m2,
                                    parametros = input$parametros)
            
            grafico_htc(modelo_selec = modelo)
        }
        else if(input$prueba == 'Linealidad'){
            grafico_lin(datos = base_m2, 
                        posicion = input$parametros)
        
        }
        
        #else if(input$prueba == 'Multcolinealidad'){
         #   grafico_mul(datos = base_m2,
          #              posicion = input$parametros)
           #    }
        
        else if(input$prueba == 'Normalidad de errores'){
            modelo <- correr_modelo(datos = base_m2, 
                                    parametros = input$parametros)
            grafico_nores(modelo = modelo)
        }
        else {
            
        }
        
    })
}

shinyApp(ui = ui, server =  server)
