library(dplyr)
library(png)
library(grid)
library(ggridges)
library(tidyr)
datos=read.csv("series-hist-ricas-anac.csv", sep=",",header=F, skip=446, nrows=44)
datos=datos[,-c(1:5,7:8)]
datos=as.data.frame(t(datos))
colnames(datos)=datos[1,]
colnames(datos)[1:2]=c("Anio", "Mes")
datos=datos[-1,]

datos %>% 
    pivot_longer( cols = 3:ncol(datos), names_to="Aerolinea",values_to= "Pasajeros") %>% 
    filter(Pasajeros !="")-> datos
mostFreq= (datos %>%   
    group_by(Aerolinea) %>% 
    summarize (n = mean(as.numeric(Pasajeros)))%>% 
    arrange(desc(n))%>% select(Aerolinea)%>% unlist(use.names = FALSE))[1:4]
datos %>%
    filter(Aerolinea %in% mostFreq)-> datos
g=ggplot(datos,aes(x=as.numeric(Pasajeros),y=Anio, fill = as.factor(Aerolinea))) +
    geom_density_ridges() +
    scale_fill_brewer(palette = "Dark2") +
    labs(x="# pasajeros por mes", y="Año", fill="Aerolíneas más frecuentes",
         title="Evolución del número de pasajeros mensuales en vuelos internacionales")+
    theme_ridges() + 
    theme(legend.position = "bottom", axis.text = element_text(size=7), 
          legend.text = element_text(size=7), title = element_text(size=8),
          plot.title = element_text(size=8))
ggsave(g, file="Dia7.jpeg", height = 5,width=7)
