library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)
library(lubridate)

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

datos=read.csv("GlobalTemperatures.csv",sep=",", header=T)
datos %>% drop_na() ->datos
datos %>%
    mutate(dt=as.Date(datos$dt)) %>%
    mutate(anio=year(dt)) %>%
    group_by(anio) %>%
    summarise(mean=mean(LandAndOceanAverageTemperature))->datos
g1<- ggplot(datos, aes(x=anio, y=mean)) + 
    geom_line(color="olivedrab1")+geom_smooth(color="deeppink")+
    my_theme_black()+
    labs(x="Año", y="Temperatura promedio de océanos", color="")