library(plyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(cowplot)
library(gganimate)
library(dplyr)
library(lubridate)
library(ggmosaic)

my_theme<- function (base_size = 14, base_family = ""){
    theme_minimal() +
    theme(line = element_line(colour =  "slategray1", size = 0.5, linetype = 1, 
    lineend = "butt"), 
            rect = element_rect(fill = "slategray1", 
            colour =  "slategray1", size = 0.5, linetype = 1), 
            text = element_text(family = base_family, 
            face = "plain", colour = "deeppink3", size = base_size,
            angle = 0, lineheight = 0.9, hjust = 0.5, vjust = 0.5),
            plot.background = element_rect(colour =  "slategray1", 
                                           fill =  "slategray1"),
            plot.title = element_text(size = rel(1.2)),
            panel.border = element_rect(fill = NA, colour = "deeppink3"), 
            panel.grid.major = element_line(colour =  "slategray1", size = 0.2), 
            panel.grid.minor = element_line(colour =  "slategray1", size = 0.5),
            strip.background = element_rect(fill =  "slategray1", 
                                            colour =  "slategray1"),
            strip.text = element_text(family = base_family, 
                                      face = "plain", colour = "deeppink3", 
                                      size = base_size-4),
            axis.text = element_text(family = base_family, 
            face = "plain", colour = "deeppink3", size = base_size-4,
            angle = 0, lineheight = 0.9, hjust = 0.5, vjust = 0.5)
        )
}

datos=read.csv("ATP.csv",sep=",", header=T)
datos %>% 
    mutate(Date=as.Date(Date, format="%d/%m/%Y"),anio=year(Date))%>%
    select(anio, Location,Tournament,Date,Series,Surface,Winner,Loser) %>% 
    drop_na() ->datos
# Filtramos datos de Nadal, Federer, Djokovic
datos%>%
    filter(Winner %in% c("Nadal R.", "Federer R.", "Djokovic N."))%>%
    filter(anio >=2009)%>%
    filter(Series %in% c("ATP250", "ATP500", "Grand Slam", "Masters 1000"))->datosFilt
datosFilt$Series=factor(datosFilt$Series, 
                        levels=sort(unique(datosFilt$Series)),
                        labels=sort(unique(datosFilt$Series)))
datosFilt$Winner=factor(datosFilt$Winner,
                        levels=sort(unique(datosFilt$Winner)),
                        labels=sort(unique(datosFilt$Winner)))
datosFilt %>%
    group_by(anio, Series, Winner) %>%
    summarise(n=n())->datosFilt

datosFilt%>%
    group_by(Series, Winner)%>%
    summarise(n=sum(n))%>%
    ggplot() +
    geom_mosaic(aes(weight = n,x = product(Winner,Series),
                    fill = Winner), na.rm = T)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid = element_blank(),legend.position = "None")+
    labs(x = "",y="",title = "# Partidos ganados entre 2009-2016")+
    scale_fill_brewer(palette="Dark2")

g=datosFilt%>%
    ggplot() +
    geom_col(aes(x = Series,y=n,fill = Winner), na.rm = T)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid = element_blank(),legend.position = "bottom")+
    labs(x = "",y="")+
    ggtitle("# partidos ganados en: {closest_state}")+
    scale_fill_brewer(palette="Dark2")+transition_states(anio)

animate(g, fps = 10, width = 600, height = 450)
anim_save("ATP.gif")
    
    