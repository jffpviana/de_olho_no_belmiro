# load libraries
library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(data.table)
library(stringr)

#setwd("")

Sys.setlocale(category = "LC_ALL", locale = "pt_PT.UTF-8")

data_super <- as.data.frame(fread("/Users/vianaj/Desktop/projecto_precos/repo/app/Data/precos_historial.csv"))

data_super$data <- factor(format(as.Date(data_super$data, tryFormats = "%d.%m.%Y"), "%d/%m/%Y"), levels=unique(format(as.Date(data_super$data, tryFormats = "%d.%m.%Y"), "%d/%m/%Y")))



productos_df <-  as.data.frame(cbind(c("Todos", gsub("Ovos m 6", "Ovos M (6)", str_to_sentence(gsub("lata ", "", gsub("medio", "médio", gsub("oleo", "óleo", gsub("feijao", "feijão", gsub("grao", "grão", gsub("maca", "maçã", gsub("_", " ", unique(data_super$producto_1))))))))))), c("todos", unique(data_super$producto_1))))

colnames(productos_df) <- c("labels", "producto_1")


for(i in 1:nrow(productos_df)){
    
  if(productos_df$producto_1[i] %in% c("alface", "arroz_agulha", "atum_conserva", "banana", "bifes_frango", "bifinhos_lombo", "carapau_medio", "cebola", "cenoura", "esparguete", "iogurte_natural", "laranja", "lata_feijao_vermelho", "lata_grao", "maca_gala", "manteiga_magra", "queijo_flamengo", "tomate_redondo")){
    
    productos_df$labels[i] <- paste0(productos_df$labels[i], " (€/Kg)")
    
  }else{
    
    if(productos_df$producto_1[i] %in% c("azeite", "leite", "oleo_alimentar")){
      
      productos_df$labels[i] <- paste0(productos_df$labels[i], " (€/L)")
    } else {
    }
  }  
}


ui <- navbarPage(
  
  title = "Observatório de preços",
  
  theme = bslib::bs_theme(version=4, bootswatch='minty'),
  
  tabPanel(
    title = "Gráficos",
    
    sidebarLayout(
      
      sidebarPanel(
        width = 3,
    #    h1("Explore a Dataset"),
        
        shiny::selectInput(
          inputId='supermercado',
          label='Seleccionar supermercado',
          choices=c('Continente', 'Pingo_doce', 'Ambos')
        ),
        

    shiny::selectInput(
      inputId='producto_1',
      label='Seleccionar producto',
      choices= productos_df$labels
    )
        
        
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
      rv$max_eu <- max(na.omit(data_super$preco_euros))+1
      
      
    }else{
      
      if(input$supermercado=='Continente'){
      
        rv$data <- data_super[which(data_super$supermercado=="Continente"),]
        rv$max_eu <- max(na.omit(data_super$preco_euros))+1
        
        
                
      }else{
        rv$data <- data_super[which(data_super$supermercado=="Pingo_doce"),]
        rv$max_eu <- max(na.omit(data_super$preco_euros))+1
        
 
      }
      
    }  
  })
    
    output$plot <- renderPlot({
    
   if(length(unique(rv$data$supermercado))==1 && unique(rv$data$supermercado)=="Pingo_doce"){
     

      ggplot(data=rv$data, aes(x=data, y=preco_euros, group=producto_2, color=supermercado, shape=promocao)) +
        geom_line(aes(linetype=marca_2))+
        geom_point(aes(size=4)) +
        ylab("Preço (euros)") +
        xlab("") +
        scale_shape_manual(values=c(20, 8), labels=c("não", "sim"), name="Estava em promoção?") +
        scale_color_manual(values = c("red"), name="Supermercado") +
        scale_linetype_manual(values=c("solid", "dashed"), name="Marca branca ou de marca?") +
        theme_minimal() +
        scale_y_continuous(breaks = seq(0, rv$max_eu, by=0.5)) +
        guides(size="none") +
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12), axis.title.x=element_text(size=14,face="bold", vjust=0), axis.title.y=element_text(size=14,face="bold", vjust=4), plot.margin = unit(c(1,0,0,1), "cm"), legend.text=element_text(size=14), legend.title=element_text(size=14)) 
     
   }else{
    
     
     ggplot(data=rv$data, aes(x=data, y=preco_euros, group=producto_2, color=supermercado, shape=promocao)) +
       geom_line(aes(linetype=marca_2))+
       geom_point(aes(size=4)) +
       ylab("Preço (euros)") +
       xlab("") +
       scale_shape_manual(values=c(20, 8), labels=c("não", "sim"), name="Estava em promoção?") +
       scale_color_manual(values = c("dark blue", "red"), name="Supermercado") +
       scale_linetype_manual(values=c("solid", "dashed"), name="Marca branca ou de marca?") +
       theme_minimal() +
       scale_y_continuous(breaks = seq(0, rv$max_eu, by=0.5)) +
       guides(size="none") +
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12), axis.title.x=element_text(size=14,face="bold", vjust=0), axis.title.y=element_text(size=14,face="bold", vjust=4), plot.margin = unit(c(1,0,0,1), "cm"), legend.text=element_text(size=14), legend.title=element_text(size=14)) 
     
   }
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
  