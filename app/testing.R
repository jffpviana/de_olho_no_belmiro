Sys.setlocale(category = "LC_ALL", locale = "pt_PT.UTF-8")

data_super$data <- as.Date(data_super$data, tryFormats = "%d.%m.%Y")

ggplot(data=data_super, aes(x=data, y=preco_euros, group=producto_2, color=supermercado, shape=promocao)) +
  geom_line(aes(linetype=marca_2))+
  geom_point() +
  ylab("Preço (euros)") +
  xlab("Data") +
  scale_shape_manual(values=c(20, 8), labels=c("não", "sim")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


