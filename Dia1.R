setwd("Downloads/")
library(tidyverse)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

datos<-read.csv("registro-de-femicidios-20200109.csv", row.names = NULL, header=T, 
                na.strings = "")

datos %>% filter(!is.na(victima_identidad_genero )&
                            !is.na(hecho_provincia) &
                            !is.na(hecho_modalidad_comisiva)&
                            !is.na(hecho_fecha) &
                            !is.na(victima_edad)& victima_edad< 150
                     ) %>%  droplevels() -> datos
datos %>% separate(hecho_fecha, c("anio"), sep="-",remove = F)-> datos

mostFreq=names(sort(table(datos[,"hecho_modalidad_comisiva"]), decreasing = T))[1:6]

levels(datos[,"hecho_modalidad_comisiva"])[!levels(datos[,"hecho_modalidad_comisiva"]) %in% mostFreq]="OTROS"
datos[,"hecho_modalidad_comisiva"]=factor(as.character(datos[,"hecho_modalidad_comisiva"]))
table(datos[,"victima_identidad_genero"],datos[,"hecho_modalidad_comisiva"])
mostCases=names(sort(table(datos[,"hecho_provincia"]), decreasing = T))
datos[,"hecho_provincia"]=factor(as.character(datos[,"hecho_provincia"]), levels=mostCases)

geom_bar(aes(y = (..count..)/sum(..count..))) + 
    scale_y_continuous(labels=scales::percent) 

g=ggplot(datos, aes(x=hecho_provincia))+geom_bar(aes(y = (..count..)/sum(..count..),fill=hecho_modalidad_comisiva),
    position = "fill")+ scale_fill_brewer(palette = "Dark2")+theme(legend.position = "bottom", 
    strip.background =element_blank(),strip.text.x = element_blank(),panel.background = element_blank(),
    panel.grid.minor = element_line(colour="gray"), axis.line = element_line(colour="gray"), 
    axis.text.x = element_text(angle=90), legend.text = element_text(size=8), plot.title = element_text(size=9)
    )+labs(y="Porcentaje de casos", x="Provincia", title="Número de casos",
    fill="Modalidad comisiva")+geom_text(stat="count",aes(label=ifelse((..count..)>0, ..count.., "")), 
    position = position_fill(vjust=1.05), size=3) +ylim(c(0,1.05))+scale_y_continuous(labels=scales::percent)
ggsave(g, file="30dias_dia1.jpeg", height = 6,width=8, dpi = 500)
    
