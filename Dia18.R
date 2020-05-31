library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(gganimate)
library(maps)
library(rnaturalearth)
require(transformr)
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

datos=read.csv("GlobalLandTemperaturesByCountry.csv",sep=",", header=T)
datos %>% 
    filter(Country%in% c("Argentina","Chile",
                         "Uruguay", "Brazil", "Bolivia",
                         "Peru", "Paraguay",
                         "Colombia", "Venezuela","Ecuador"))%>%
    drop_na() ->datos
datos %>%
    mutate(dt=as.Date(datos$dt)) %>%
    mutate(anio=year(dt)) %>%
    group_by(anio, Country) %>%
    summarise(tempProm=mean(AverageTemperature))->datosFilt
datosFilt%>% filter(anio %in% seq(1910,2010, by=10)) ->datosFilt
datosFilt %>% filter(anio==1910) %>% 
    rename(tempRef=tempProm) -> datosRef

mapa <- fortify(ne_countries(country = c("Argentina","Chile",
                               "Uruguay", "Brazil", "Bolivia",
                               "Peru", "Paraguay",
                               "Colombia", "Venezuela","Ecuador")))
mapa$id=factor(mapa$id)
levels(mapa$id)=c("Peru","Paraguay",
                  "Uruguay","Venezuela", 
                  "Bolivia", "Brazil", "Chile","Colombia",
                  "Argentina","Ecuador")

table(datosFilt$Country %in% mapa$id)
table(mapa$id %in% datosFilt$Country )

mapa %>% full_join(datosFilt, 
                        by = c("id"="Country" ))->plotData

plotData %>% full_join(datosRef[,2:3], 
                   by = c("id"="Country" )) %>%
    mutate(tempDif=tempProm-tempRef)->plotData

limites=c(min(plotData$tempDif), max(plotData$tempDif))
g=ggplot(plotData, aes(x = long, y = lat, frame=anio)) +
    geom_polygon(aes(group=group,fill = tempDif))+
    theme_minimal()+theme(panel.grid = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          axis.title = element_blank(),
                          legend.position = "bottom")+
    scale_fill_gradient2("Temperatura media",low="white",
                         mid="yellow",high="red", 
                         limits =limites )+
    ggtitle("Año: {closest_state}")
g=g + transition_states(states=anio)
animate(g, fps = 10, width = 600, height = 450)
anim_save("TempAnual.gif")

