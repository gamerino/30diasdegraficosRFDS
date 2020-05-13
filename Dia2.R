setwd("Dropbox/R4DS/")
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

datos<-read.csv("multiTimelineRecetaGimnasiaArg.csv", row.names = NULL,
                header=T,skip = 2)
names(datos)=c("Dia","Receta de medialunas", "Ejercicios en casa")
datos %>% mutate(Dia=as.Date(datos$Dia) )%>% 
    mutate(DiaSem=weekdays(Dia) )%>%
    pivot_longer( cols = c("Receta de medialunas", "Ejercicios en casa"),
             names_to="Busqueda",values_to= "Ocurrencias") -> datos
datos$DiaSem=factor(datos$DiaSem)
levels(datos$DiaSem)=c("Viernes", "Lunes", "Sábado", "Domingo","Jueves", "Martes", "Miércoles")
g=ggplot(datos)+geom_rect(
    aes(xmin =as.Date("2020-03-10"), xmax = as.Date("2020-05-09"),ymin = -Inf, ymax = Inf),
    alpha = 0.3, fill="#FFFFCC")+geom_line(aes(x=Dia, y=Ocurrencias, color=Busqueda, 
    group=Busqueda), size=2)
g=g+geom_rect(data = data.frame(xmin = as.Date(datos$Dia[datos$DiaSem=="Sábado" & datos$Dia!="2020-05-09"]),
  xmax = as.Date(datos$Dia[datos$DiaSem=="Domingo"& datos$Dia!="2020-03-01"]),
  ymin = -Inf, ymax = Inf),aes(xmin = xmin, xmax = xmax,ymin = ymin, ymax = ymax),
  alpha = 0.1, fill="skyblue", colour="navy")+geom_vline(xintercept = as.Date("2020-03-10"), 
  color="#999900", linetype="dashed")
g=g+scale_color_brewer(palette = "Set2")+theme(legend.position = "bottom", 
 strip.background =element_blank(),strip.text.x = element_blank(),panel.background = element_blank(),
 panel.grid.minor = element_line(colour="gray"), axis.line = element_line(colour="gray"), 
 axis.text.x = element_text(angle=90), legend.text = element_text(size=8), 
 plot.title = element_text(size=9),legend.key=element_blank())+labs(y="Número de Búsquedas", x="Fecha",
 title="",color="Frase")+scale_x_date(date_breaks = "2 day", expand = c(0,0)
 )+scale_y_continuous(expand = c(0,0))+annotate("text", label="Inicio cuarentena", 
 x=as.Date("2020-03-11"), y=80, angle=90)+annotate("text", 
 label="Fin de semana", x=as.Date("2020-03-07")+0.5, y=80, angle=90, colour="navy")
ggsave(g, file="30dias_dia2.jpeg", height = 5,width=8.5, dpi = 500)
    
