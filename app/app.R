# load libraries
library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(data.table)

#setwd("")

Sys.setlocale(category = "LC_ALL", locale = "pt_PT.UTF-8")

data_super <- as.data.frame(fread("/Users/vianaj/Desktop/projecto_precos/repo/app/Data/precos_historial.csv"))

data_super$data <- factor(format(as.Date(data_super$data, tryFormats = "%d.%m.%Y"), "%d/%m/%Y"), levels=unique(format(as.Date(data_super$data, tryFormats = "%d.%m.%Y"), "%d/%m/%Y")))


data_super$colour <- "dark red"
data_super$colour[which(data_super$supermercado=="Pingo_doce")]<-"dark blue"


ui <- navbarPage(
  
  title = "Observatório de preços",
  
  theme = bslib::bs_theme(version=4, bootswatch='minty'),
  
  tabPanel(
    title = "Graficos",
    
    sidebarLayout(
      
      sidebarPanel(
        width = 3,
    #    h1("Explore a Dataset"),
        
        shiny::selectInput(
          inputId='supermercado',
          label='Seleccionar supermercado',
          choices=c('Continente', 'Pingo_doce', 'Ambos')
        ),
        

#        hr(),
#       p("Código disponível aqui") %>%
#          a(
#            href = 'https://www.business-science.io/',
#            target = "_blank",
#            class = "btn btn-lg btn-primary"
#          ) %>%
#          div(class = "text-center")
        
        
      ),
      
      mainPanel(
#      h1("Correlation"),
        plotOutput("plot", height = 700, , width = 1050)
      )
    )
    
  )
  
  
)



  
server <- function(input, output){
  
  rv <- reactiveValues()
  
  observe({
    
    if(input$supermercado=='Ambos'){
      
      rv$data <- data_super

      
    }else{
      
      if(input$supermercado=='Continente'){
      
        rv$data <- data_super[which(data_super$supermercado=="Continente"),]

        
                
      }else{
        rv$data <- data_super[which(data_super$supermercado=="Pingo_doce"),]
 
      }
      
    }  
  })
    
    output$plot <- renderPlot({
    
    
   if(length(unique(rv$data$supermercado))==1 && unique(rv$data$supermercado)=="Pingo_doce"){
     
     
      ggplot(data=rv$data, aes(x=data, y=preco_euros, group=producto_2, color=supermercado, shape=promocao)) +
        geom_line(aes(linetype=marca_2))+
        geom_point(aes(size=4)) +
        ylab("Preço (euros)") +
        xlab("Data") +
        scale_shape_manual(values=c(20, 8), labels=c("não", "sim"), name="Estava em promoção?") +
        scale_color_manual(values = c("red"), name="Supermercado") +
        scale_linetype_manual(values=c("solid", "dashed"), name="Marca branca ou de marca?") +
        theme_minimal() +
        scale_y_continuous(breaks = seq(0,20, by=0.5)) +
        guides(size="none") +
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12), axis.title.x=element_text(size=14,face="bold", vjust=0), axis.title.y=element_text(size=14,face="bold", vjust=4), plot.margin = unit(c(1,0,0,1), "cm"), legend.text=element_text(size=14), legend.title=element_text(size=14)) 
     
   }else{
     
     ggplot(data=rv$data, aes(x=data, y=preco_euros, group=producto_2, color=supermercado, shape=promocao)) +
       geom_line(aes(linetype=marca_2))+
       geom_point(aes(size=4)) +
       ylab("Preço (euros)") +
       xlab("Data") +
       scale_shape_manual(values=c(20, 8), labels=c("não", "sim"), name="Estava em promoção?") +
       scale_color_manual(values = c("dark blue", "red"), name="Supermercado") +
       scale_linetype_manual(values=c("solid", "dashed"), name="Marca branca ou de marca?") +
       theme_minimal() +
       scale_y_continuous(breaks = seq(0,20, by=0.5)) +
       guides(size="none") +
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12), axis.title.x=element_text(size=14,face="bold", vjust=0), axis.title.y=element_text(size=14,face="bold", vjust=4), plot.margin = unit(c(1,0,0,1), "cm"), legend.text=element_text(size=14), legend.title=element_text(size=14)) 
     
   }
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
  