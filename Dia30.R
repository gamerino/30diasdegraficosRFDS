library(tidyverse)
library(lubridate)
library(ggplot2)
library(cowplot)
library(extrafont) 

loadfonts()
font<- "Trebuchet MS" 
datos<-read_csv("datasets_530327_1183622_ebola_2014_2016_clean.csv")

datos%>%
    mutate(mes = month(Date))%>%
    mutate(anio = year(Date))->datos
datos$mes=factor(datos$mes)
levels(datos$mes)=c("Ene","Feb","Mar","Abr","May","Jun","Jul",
                    "Ago","Sep","Oct","Nov","Dic")

datos%>%
    rename("Casos"="Cumulative no. of confirmed, probable and suspected cases",
           "Muertes"="Cumulative no. of confirmed, probable and suspected deaths")->datos
datos%>%
    group_by(mes,anio,Country)%>%
    # como son cantidades acumuladas, tomamos el máximo que asumimos son al
    #último mes del año en que hubo registro
    summarise(nCasos=max(Casos),nMuertes=max(Muertes))%>%
    drop_na()%>%
    mutate(fecha=paste(mes,"-", anio,sep=""))%>%
    arrange(anio,mes,Country)->datosPlot
datosPlot$fecha=factor(datosPlot$fecha, levels=unique(datosPlot$fecha))
g1<-ggplot(datosPlot, aes(x=fecha, y=nCasos, fill=Country))+
    geom_col()+coord_polar()+labs(x="",y="", fill="País",
    title="Número de casos acumulados")+
    scale_fill_brewer(palette="Set3")+
    theme_minimal()+theme(legend.position="bottom",
                          text=element_text(family=font))+
    guides(fill=guide_legend(nrow=2))


g2=ggplot(datosPlot, aes(x=fecha, y=nMuertes, fill=Country))+
    geom_col()+coord_polar()+labs(x="",y="", 
    title="Número de muertes acumuladas")+
    scale_fill_brewer(palette="Set3")+
    theme_minimal()+theme(legend.position="None",
                          text=element_text(family=font))+
    guides(fill=guide_legend(nrow=2))
leyenda<-get_legend(g1)
title <- ggdraw() + 
    draw_label("Brote epidémico de Ébola 2014-2016", fontface='bold', fontfamily = font)

plot_grid(title,plot_grid(g1+theme(legend.position="None"),g2, nrow=1), leyenda, 
          ncol=1,rel_heights = c(0.1,1,0.2))
