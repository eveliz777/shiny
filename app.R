library(shiny)
library(readr)
library(reshape2)
library(reactable)
library(dplyr)
library(ggplot2)
library(forcats)
library(DT)
library(formattable)

#Separador de miles europeo
sep.miles <- function(x){format(x,big.mark=".",decimal.mark=",",digits = 1)}
SCA_SO <- read_csv(paste0("3_Base_SCA_SO.csv"), col_types = cols(FechaCreacionSGDP = col_character(),fecha_nacimiento_usuario = col_character(), fecha_registro_actividad_sistema = col_character(),fecha_registro_actividad = col_character(),fecha_asignacion_sistema = col_character()),locale = locale(encoding = "WINDOWS-1252"))

ui <- fluidPage(
    # Application title
    titlePanel("Tipo de Servicio por Materia"),
    sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput(
            inputId = "provincia",
            label = "Seleccione la Provincia",
            choices = c(
              "NACIONAL",
#              "REGION 1",
#              "REGION 2",
#              "REGION 3",
              "AZUAY",
              "BOLIVAR",
              "CAÑAR",
              "CARCHI",
              "COTOPAXI",
              "CHIMBORAZO",
              "EL ORO",
              "ESMERALDAS",
              "GUAYAS",
              "IMBABURA",
              "LOJA",
              "LOS RIOS",
              "MANABI",
              "MORONA SANTIAGO",
              "NAPO",
              "PASTAZA",
              "PICHINCHA",
              "TUNGURAHUA",
              "ZAMORA CHINCHIPE",
              "GALAPAGOS",
              "SUCUMBIOS",
              "ORELLANA",
              "SANTO DOMINGO DE LOS TSACHILAS",
              "SANTA ELENA"
            ), 
            selected = "NACIONAL",
            multiple = FALSE
          )
        ),

        mainPanel(
          fluidRow(htmlOutput('texto')),
          fluidRow(dataTableOutput('table')),
          fluidRow(plotOutput("plot")),
          fluidRow(plotOutput("plot2")),
        )
    )
)

# Define server logic required
server <- function(input, output) {
  
  output$texto <- renderUI({
    HTML("<b><h3>",paste0(input$provincia,": ASESORIA Y PATROCINIO EN MATERIAS PENAL Y NO PENAL"),"</h3></b>")
  })

  output$table <- DT::renderDataTable ({
    if (input$provincia!="NACIONAL") SCA_SO1 <- SCA_SO[SCA_SO$provincia==input$provincia,]
    else SCA_SO1=SCA_SO
    tab1 = dcast(SCA_SO1, Tipo_servicio~materia,value.var = "sgdp",fun.aggregate = length)
    tab1$TOTAL = tab1$`MATERIA NO PENAL` + tab1$`MATERIA PENAL` 
    newdato = data.frame("TOTAL",sum(tab1$`MATERIA NO PENAL`),sum(tab1$`MATERIA PENAL`),sum(tab1$TOTAL))
    colnames(newdato)=colnames(tab1)
    tab1A=rbind(sep.miles(tab1),sep.miles(newdato))
    
    DT::datatable(tab1A,options = list(pageLength = 50,paging = FALSE,searching = FALSE))
  })

  output$plot <- renderPlot({
    if (input$provincia!="NACIONAL") SCA_SO1 <- SCA_SO[SCA_SO$provincia==input$provincia,]
    else SCA_SO1=SCA_SO
    
    ggplot(SCA_SO1, aes(x = Tipo_servicio, fill = materia)) + 
      geom_bar() +
      scale_fill_brewer() +
      labs(title="Servicios por Materia",x = "Tipo de Servicio",y = "Número de Prestaciones") +
      geom_text(aes(label = sep.miles(..count..)), stat = "count", position = position_stack(vjust = 0.5), size = 3,colour = "black") +
      guides(fill = guide_legend(title = "Materia")) +
      theme(legend.position = "right")
  })
  
  output$plot2 <- renderPlot({
    if (input$provincia!="NACIONAL") SCA_SO1 <- SCA_SO[SCA_SO$provincia==input$provincia,]
    else SCA_SO1=SCA_SO
    tab1 = dcast(SCA_SO1, Tipo_servicio~materia,value.var = "sgdp",fun.aggregate = length)
    tab1$TOTAL = tab1$`MATERIA NO PENAL` + tab1$`MATERIA PENAL` 
    newdato = data.frame("TOTAL",sum(tab1$`MATERIA NO PENAL`),sum(tab1$`MATERIA PENAL`),sum(tab1$TOTAL))
    colnames(newdato)=colnames(tab1)
    tab1A=rbind(tab1,newdato)
    suma = as.numeric(tab1A$TOTAL[3])
    
    ggplot(SCA_SO1, aes(x = Tipo_servicio, fill = materia)) + 
      geom_bar() +
      scale_fill_brewer() +
      labs(title="Servicios por Materia",x = "Tipo de Servicio",y = "Porcentaje de Prestaciones") +
      geom_text(aes(label = percent(..count../suma)), stat = "count", position = position_stack(vjust = 0.5), size = 3,colour = "black") +
      guides(fill = guide_legend(title = "Materia")) +
      theme(legend.position = "right")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
