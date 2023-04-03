#funcoes para preencher os precos automaticamente



precos_continente <- function(producto, link){
  
  if(length(grep("ovos_M_6", producto))>0){
    
    popParse<- read_html(link)
    popNodes <- html_nodes(popParse, "body div")
    
    popNodes[2] %>% html_children
    
    txt <- html_text(html_children(html_children(popNodes[2])[2])[4])
    
    preco <- gsub(",", ".", gsub("€", "", unlist(str_split(txt, "\n"))[grep("€", unlist(str_split(txt, "\n")))][1]))
    
  }else{
    
    popParse<- read_html(link)
    popNodes <- html_nodes(popParse, "body div")
    

    txt <- html_text(html_children(html_children(popNodes[2])[2])[4])
    
    pos <- intersect(grep("€", unlist(str_split(txt, "\n")))+3, grep("kg|lt", unlist(str_split(txt, "\n"))))-3
    
    preco <- gsub(",", ".", gsub("€", "", unlist(str_split(txt, "\n"))[pos] ))
    
  }
  
  return(preco)
  
}


promocoes_continente <- function(producto, link){
  
  promocao <- "n"
  
  popParse<- read_html(link)
  popNodes <- html_nodes(popParse, "body div")
  
  text <- (((((((popNodes[2] %>% html_children)[2] %>% html_children)[4] %>% html_children %>% html_children)[2] %>% html_children)[2] %>% html_children)[1] %>% html_children) %>% html_text)[1]
  
  if(length(grep("%", text))>0){
    
    promocao <- "y"
    
  }
  
  return(promocao)
}

