Sys.setlocale(category = "LC_ALL", locale = "pt_PT.UTF-8")

data_super$data <- as.Date(data_super$data, tryFormats = "%d.%m.%Y")
max_eu <- max(na.omit(data_super$preco_euros))+1

ggplot(data=data_super, aes(x=data, y=preco_euros, group=producto_2, color=supermercado, shape=promocao)) +
  geom_line(aes(linetype=marca_2))+
  geom_point(aes(size=4)) +
  ylab("Preço (euros)") +
  xlab("Data") +
  scale_shape_manual(values=c(20, 8), labels=c("não", "sim"), name="Estava em promoção?") +
  scale_color_manual(values = c("dark blue", "red"), name="Supermercado") +
  scale_linetype_manual(values=c("solid", "dashed"), name="Marca branca ou de marca?") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,max_eu, by=0.5)) +
  guides(size="none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12), axis.text.y = element_text(size = 12), axis.title.x=element_text(size=14,face="bold", vjust=0), axis.title.y=element_text(size=14,face="bold", vjust=4), plot.margin = unit(c(1,0,0,1), "cm"), legend.text=element_text(size=14), legend.title=element_text(size=14)) 