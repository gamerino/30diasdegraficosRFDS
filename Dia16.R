library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)
library(cowplot)
library(waffle)
library(RColorBrewer)
#### my black theme

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

datos=read.csv("Covid19arData - seriesFallecidos.csv",sep=",", header=T)
str(datos)
datos$provincia=factor(datos$provincia)
levels(datos$provincia)[c(1:2,12:15)]=c("No reportado","No reportado",
                                        "Santa Fe","Santa Fe",
                                        "Tucumán" ,"Tucumán")
datos %>% filter(sexo=="FEM") -> datosFem
datos %>% filter(sexo!="FEM") -> datosMasc
#FemPlot
freq=table(datosFem$provincia)
myColors=brewer.pal(n=length(freq), name="Set3")
g1=waffle(freq, colors =myColors)+labs(x="",y="",
    title="", 
    subtitle = "Mujeres", fill="")+
    theme(legend.position = "None")
   
#MascPlot
freq=table(datosMasc$provincia)
g2=waffle(freq, colors =myColors)+labs(x="",y="",
    title="", subtitle = "Hombres",fill="")

leyenda=get_legend(g2)
plot_grid(plot_grid(g1,g2+theme(legend.position = "None"),nrow=2),
          leyenda, ncol=2, rel_widths = c(1,0.1), 
          labels = c("# fallecidos COVID-19",""))
