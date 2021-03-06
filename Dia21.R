library(tidyverse)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
my_theme_black<- function (base_size = 14, base_family = ""){
    theme_minimal() +
        theme(line = element_line(colour = "black", size = 0.5, linetype = 1, 
                                  lineend = "butt"), 
              rect = element_rect(fill = "black", 
                                  colour = "black", size = 0.5, linetype = 1), 
              text = element_text(family = base_family, 
                                  face = "plain", colour = "lightseagreen", size = base_size,
                                  angle = 0, lineheight = 0.9, hjust = 0.5, vjust = 0.5),
              plot.background = element_rect(colour = 'black', fill = 'black'),
              plot.title = element_text(size = rel(1.2)),
              panel.border = element_rect(fill = NA, colour = "lightseagreen"), 
              panel.grid.major = element_line(colour = "black", size = 0.2), 
              panel.grid.minor = element_line(colour = "black", size = 0.5),
              strip.background = element_rect(fill = "black", colour = "black"),
              axis.text = element_text(family = base_family, 
                                       face = "plain", colour = "lightseagreen", size = base_size-4,
                                       angle = 0, lineheight = 0.9, hjust = 0.5, vjust = 0.5)
        )
}

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

datos%>%
    group_by(hecho_provincia,hecho_modalidad_comisiva) %>%
    summarize(n=n()) %>% 
    group_by(hecho_provincia) %>%
    mutate(perc=sum(n)) %>%
    mutate(perc=round(n*100/perc,2)) ->sumDatos

++geom_text(stat="count",aes(label=ifelse((..count..)>0, ..count.., "")), 
                                                                                             position = position_fill(vjust=1.05), size=3) +ylim(c(0,1.05))+scale_y_continuous(labels=scales::percent)

g2=ggplot(sumDatos,aes(x=hecho_provincia))+
    geom_bar(aes(y = perc, fill=hecho_modalidad_comisiva), stat = "identity",
             position = position_fill(reverse=T), alpha=0.5)+
    scale_fill_brewer(palette = "Dark2")+my_theme_black()+
    labs(y="Porcentaje de casos", x="Provincia", title="Número de casos",
         fill="Modalidad comisiva")
g2=g2+geom_text_repel(aes(y=(perc),label=perc,colour=hecho_modalidad_comisiva), 
                   stat="identity", position=position_fill(reverse = T),
                   size=3)+
    scale_y_continuous(labels=scales::percent)+
    scale_colour_brewer(palette = "Dark2")+guides(colour=FALSE)+
    theme(legend.position = "bottom", axis.text.x = element_text(angle=90))

ggsave(g2,file="Dia21.jpeg", height = 7,width=10)

