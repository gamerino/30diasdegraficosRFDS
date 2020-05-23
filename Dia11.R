library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

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
            panel.grid.minor = element_line(colour = "black", size = 0.5),
            strip.background = element_rect(fill = "black", colour = "black"),
            axis.text = element_text(family = base_family, 
            face = "plain", colour = "wheat", size = base_size-4,
            angle = 0, lineheight = 0.9, hjust = 0.5, vjust = 0.5)
        )
}

datos <- read.csv('top50contry.csv',header=T, sep=',',encoding = "Latin-1")

datos %>%
    filter(country!="world") %>%
    group_by(artist) %>%
    mutate(nArtist=n()) %>%
    group_by(artist, country) %>%
    mutate(nCountry=n()) %>%
    select(country, artist, nArtist, nCountry) %>%
    filter(nArtist>4) %>% unique()->datosFilt

# si es necesario...
datosFilt$artist[datosFilt$artist=="ROSAL\xcdA"]="ROSALIA"
datosFilt$artist[datosFilt$artist=="Michael Bubl\xe9"]="Michael Buble"
datosFilt$artist[datosFilt$artist=="Jos\xe9 Feliciano"]="Jose Feliciano"

g <- ggplot(datosFilt, aes(x=artist, y=country)) + geom_tile(aes(
    fill=as.factor(nCountry)))+
    scale_fill_viridis(discrete=T)+
    my_theme_black()+theme(legend.position = "bottom",axis.text.x = 
    element_text(angle=90))+
    labs(x="Artista", y="País", 
    title="# de canciones del top 50 de Spotify de distintos países",
    fill="Número de canciones en el top50")+guides(fill=guide_legend(nrow=1))
