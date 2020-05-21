library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

#### my black theme

my_theme_black<- function (base_size = 16, base_family = ""){
    theme_minimal() +
        theme(line = element_line(colour = "khaki1", size = 0.5, linetype = 1, 
                                lineend = "butt"), 
            rect = element_rect(fill = "khaki1", 
                                colour = "khaki1", size = 0.5, linetype = 1), 
            text = element_text(family = base_family, 
                                face = "plain", colour = "khaki1", size = base_size,
                                angle = 0, lineheight = 0.9, hjust = 0.5, vjust = 0.5),
            plot.background = element_rect(colour = 'black', fill = 'black'),
            plot.title = element_text(size = rel(1.2)),
            panel.border = element_rect(fill = NA, colour = "white"), 
            panel.grid.major = element_line(colour = "khaki1", size = 0.2), 
            panel.grid.minor = element_line(colour = "khaki2", size = 0.5),
            strip.background = element_rect(fill = "khaki3", colour = "khaki3")
        )
}

datos <- read.csv('Dia9.csv',header=T, sep=',',skip=2,
                 stringsAsFactors=TRUE)
colnames(datos)[2:6]=do.call(rbind,strsplit(colnames(datos)[2:6],
                                            split="[.]"))[,2]

datos %>%
    mutate(fecha = parse_date_time(Semana, "%Y-%m-%d")) %>%
    pivot_longer(cols=Smart:Depiladora,names_to = "Item",
    values_to = "Busquedas") %>%
    select(-Semana) ->datos
datos$Item<-factor(datos$Item, levels = unique(datos$Item))
levels(datos$Item)[1]="Smart TV"
g <- ggplot(datos, aes(x=fecha, y=Busquedas)) + geom_area(aes(
    fill=Item), alpha=1)+
    scale_fill_brewer(palette = "PuRd")+
    theme(legend.position = "bottom",
          legend.text = element_text(face=c(rep("plain",4), "bold")),
          plot.background = element_rect(fill="black"))+
    labs(x="Fecha", y="# Búsquedas en Google")+my_theme_black()

