library(readr)
library(dplyr)
library(tidytext)
library(widyr)
library(ggpubr)
library(sunburstR)
library(ggrepel)

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
              panel.border = element_rect(fill = NA, colour = "black"), 
              panel.grid.major = element_line(colour = "black", size = 0.2), 
              panel.grid.minor = element_line(colour = "black", size = 0.5),
              strip.background = element_rect(fill = "black", colour = "black"),
              axis.text = element_text(family = base_family, 
                                       face = "plain", colour = "black", size = base_size-4,
                                       angle = 0, lineheight = 0.9, hjust = 0.5, vjust = 0.5)
        )
}

# Retomando lo hecho para el Datos de miércoles!!!

la_casa_de_papel <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv")
# obtener palabras por separado
la_casa_de_papel_subs <- la_casa_de_papel %>%
    unnest_tokens(word,texto) %>%
    anti_join(stop_words)

# definir los personajes, teniendo en cuenta los "búscados" luego de T2 (https://github.com/gamerino/DSwR-DDM/blob/master/LCDPPersonajes.png)
personajes<-c("profesor","profesor", "tokio", "tokio","rio","rio",
              "denver", "helsinki", "nairobi", 
              "lisboa", "lisboa", "lisboa", "estocolmo", "estocolmo")
# teniendo en cuenta que raquel=inspectora=lisboa y que mónica=estocolmo
names(personajes)<-c("sergio","profesor", "tokio", "silene","río","aníbal", 
                     "denver", "helsinki", "nairobi", 
                     "inspectora", "raquel", "lisboa", "mónica", "estocolmo")

#agrupamos los personajes según el número de veces que se los nombra por episodio por temporada
la_casa_de_papel_pers=la_casa_de_papel_subs %>% 
    filter(word %in%names(personajes)) %>% 
    group_by(word, episodio,temporada) %>% tally()

#agrupamos los personajes según su denominación como capital de país ( a parte del profesor :P) 
la_casa_de_papel_pers=mutate(la_casa_de_papel_pers, 
                             wordSumm = personajes[word])
# definimos los niveles
nivel_0 <- data.frame(label = "LCdP", n = 0, nivel = 0, fill = NA)
head(nivel_0)
# definimos los niveles
nivel_1 <- la_casa_de_papel_pers %>%
    group_by(wordSumm) %>%                     
    summarise(n=sum(n)) %>%
    rename("label"="wordSumm") %>%
    mutate(nivel=1, fill=label)

nivel_2 <- la_casa_de_papel_pers %>%
    group_by(wordSumm, temporada) %>%                     
    summarise(n=sum(n)) %>%
    rename("label"="temporada", fill="wordSumm") %>%
    mutate(nivel=2, label=as.factor(as.character(label))) %>%
    select(label,n,nivel,fill)
levels(nivel_2$label)=c("I","II","III")
nivel_3 <- la_casa_de_papel_pers %>%
    group_by(wordSumm, temporada,episodio) %>%                     
    summarise(n=sum(n)) %>%
    rename("label"="episodio", fill="wordSumm") %>%
    mutate(nivel=3,label=as.character(label)) %>%
    ungroup() %>%
    select(label,n,nivel,fill)
datos <- bind_rows(nivel_0,nivel_1,nivel_2,nivel_3) %>%
    mutate(label = as.factor(label) %>% 
    fct_reorder2(fill, n)) %>%
    arrange(fill, label) %>%
    mutate(nivel = as.factor(nivel)) 

ggplot(datos, aes(x = nivel, y = n, fill = fill)) +
    geom_col(width = 1, color = "lightseagreen", 
             size = 0.5, position = position_stack()) +
    geom_text(aes(label = label,size=5.5-as.numeric(as.character(nivel))), 
              position = position_stack(vjust = 0.5),color="deeppink4") +
    coord_polar(theta = "y") +
    scale_x_discrete(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    scale_fill_brewer(palette = "Set3", 
                      na.translate = F) +
    labs(x = NULL, y = NULL, fill = "Personaje buscado" ) +
    my_theme_black()+guides(size=F)

