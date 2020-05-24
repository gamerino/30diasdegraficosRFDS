library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)

#### my black theme

my_theme_black<- function (base_size = 14, base_family = ""){
    theme_minimal() +
    theme(line = element_line(colour = "black", size = 0.5, linetype = 1, 
    lineend = "butt"), 
            rect = element_rect(fill = "black", 
            colour = "black", size = 0.5, linetype = 1), 
            text = element_text(family = base_family, 
            face = "plain", colour = "wheat", size = base_size,
            angle = 0, lineheight = 0.9, hjust = 0.5, vjust = 0.5),
            plot.background = element_rect(colour = 'black', fill = 'black'),
            plot.title = element_text(size = rel(1.2)),
            panel.border = element_rect(fill = NA, colour = "black"), 
            panel.grid.major = element_line(colour = "black", size = 0.2), 
            panel.grid.minor = element_line(colour = "khaki", size = 0.5),
            strip.background = element_rect(fill = "black", colour = "black"),
            axis.text = element_text(family = base_family, 
            face = "plain", colour = "wheat", size = base_size-4,
            angle = 0, lineheight = 0.9, hjust = 0.5, vjust = 0.5)
        )
}

datos=read.csv("PoblacionPorArea.csv",sep=",", header=T)
unique(datos$Área.geográfica)
datos %>%
    filter(Área.geográfica %in% c("Gran Buenos Aires",
                                "Ciudad Autónoma de Buenos Aires ( 2)",
                               "Cuyo","Noreste","Noroeste","Pampeana",
                               "Patagonia")) %>%
    select(c(Área.geográfica, Población.total, Población.que.utilizó.computadora,
              Población.que.utilizó.internet))%>% 
    pivot_longer(cols=Población.que.utilizó.computadora:Población.que.utilizó.internet, 
                 names_to="TIC", values_to="pob")-> datosGenerales

datosGenerales$Área.geográfica=factor(datosGenerales$Área.geográfica)
levels(datosGenerales$Área.geográfica)[c(1,3)]=c("CABA", "GBA")
datosGenerales$TIC=factor(datosGenerales$TIC)
levels(datosGenerales$TIC)=c("Uso compu", "Uso internet")
g1<- ggplot(datosGenerales, aes(x=pob, y=Área.geográfica, color=TIC)) + 
    geom_linerange(aes(xmin=0, xmax=pob),position = position_dodge(.5))+
    geom_point(position = position_dodge(.5),size=2) +
    my_theme_black()+theme(legend.position = "bottom")+
    labs(x="Población", y="Gran región", color="") +
    scale_color_brewer(palette = "Dark2")

datos %>%
    filter(!Área.geográfica %in% c("Gran Buenos Aires",
                                   "Ciudad Autónoma de Buenos Aires ( 2)",
                                  "Cuyo","Noreste","Noroeste","Pampeana",
                                  "Patagonia")) %>%
    select(c(Área.geográfica, Población.total, Población.que.utilizó.computadora,
              Población.que.utilizó.internet))%>% 
    pivot_longer(cols=Población.que.utilizó.computadora:Población.que.utilizó.internet, 
                 names_to="TIC", values_to="pob")-> datosParticulares

datosParticulares$Área.geográfica=factor(datosParticulares$Área.geográfica)
levels(datosParticulares$Área.geográfica)=do.call(rbind,strsplit(levels(
    datosParticulares$Área.geográfica), split="[(]"))[,1]
datosParticulares$TIC=factor(datosParticulares$TIC)
levels(datosParticulares$TIC)=c("Uso compu", "Uso internet")
g2<- ggplot(datosParticulares, aes(x=pob, y=Área.geográfica, color=TIC)) + 
    geom_linerange(aes(xmin=0, xmax=pob),position = position_dodge(.5))+
    geom_point(position = position_dodge(.5),size=2) +
    my_theme_black()+theme(legend.position = "bottom")+
    labs(x="Población", y="Área geográfica desagregada", color="") +
    scale_color_brewer(palette = "Dark2")
plot_grid(g1,g2,nrow=1, rel_widths = c(0.5,1))
    