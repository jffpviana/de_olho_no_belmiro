library(rvest)
library(stringr)
library(data.table)

source("~/Desktop/projecto_precos/repo/app/automat_func.R")

data_super <- as.data.frame(fread("/Users/vianaj/Desktop/projecto_precos/repo/app/Data/precos_historial.csv"))

sub_c <- which(data_super$data==unique(data_super$data)[1] & data_super$supermercado=="Continente")
new_c <- as.data.frame(matrix(NA, ncol=ncol(data_super), nrow=rep(length(sub_c))))
colnames(new_c) <- colnames(data_super)

new_c[, c("supermercado", "producto_1", "producto_2", "peso_gr_ml", "marca_1", "marca_2", "agente", "link")] <- data_super[sub_c, c("supermercado", "producto_1", "producto_2", "peso_gr_ml", "marca_1", "marca_2", "agente", "link")]


rownames(new_c)<-new_c$producto_2

new_c$data <- gsub(" ", ".", format(Sys.Date(), format="%d %m %Y"))
new_c$iva_perc <- 6


for(i in rownames(new_c)){
  
  new_c[i,"preco_euros"] <- precos_continente(producto=rownames(new_c[i,]), link=new_c[i,"link"])
  new_c[i,"promocao"] <- promocoes_continente(producto=rownames(new_c[i,]), link=new_c[i,"link"])
  
}

# add pingo doce rows for manual filling

sub_p <- which(data_super$data==unique(data_super$data)[1] & data_super$supermercado=="Pingo_doce")
new_p <- as.data.frame(matrix(NA, ncol=ncol(data_super), nrow=rep(length(sub_p))))
colnames(new_p) <- colnames(data_super)

new_p[, c("supermercado", "producto_1", "producto_2", "peso_gr_ml", "marca_1", "marca_2", "agente", "link")] <- data_super[sub_p, c("supermercado", "producto_1", "producto_2", "peso_gr_ml", "marca_1", "marca_2", "agente", "link")]


rownames(new_p)<-new_p$producto_2

new_p$data <- gsub(" ", ".", format(Sys.Date(), format="%d %m %Y"))
new_p$iva_perc <- 6


data_super_tmp <- rbind(data_super, new_c, new_p)

fwrite(data_super_tmp, "/Users/vianaj/Desktop/projecto_precos/repo/app/Data/precos_historial_tmp.csv", row.names=FALSE)


