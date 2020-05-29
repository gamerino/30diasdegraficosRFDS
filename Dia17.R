library(tidyverse)
library(ggalluvial)
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

datos=read.csv("CompleteDataset.csv",sep=",", header=T)
datos %>% select(Nationality,Club,
                 Preferred.Positions) %>% 
    drop_na() %>% head(n=30)->datos
datos %>% separate(Preferred.Positions, 
                   c("Pos1","Pos2", "Pos3", "Pos4")) %>%
    pivot_longer(Pos1:Pos4, names_to="aux", 
                 values_to="Position") %>% 
    select(-aux) %>% 
    filter(Position !="") %>% drop_na() ->datosFilt
datosFilt%>%
    group_by(Nationality,Club, Position)%>%
    summarise(n = n())%>% arrange(desc(n))->datosPlot
ggplot(datosPlot,aes(axis1 = reorder(Nationality,n), 
               axis2 = reorder(Club,n), 
               axis3 = reorder(Position,n),
               y = n))  +
    geom_stratum(aes(fill = Position), color="navy") +
    geom_alluvium(aes(fill = Position)) +
    geom_text(stat = "stratum", infer.label = TRUE, size=3) +
    theme_minimal() + scale_fill_brewer(palette = "Set3")+
    annotate("text", x = 1, y = -2, label ="País")+
    annotate("text", x = 2, y = -2, label ="Club")+
    annotate("text", x = 3, y = -2, label ="Posición preferida")+
    theme(legend.position = "None", axis.text = element_blank(),
    panel.grid = element_blank())+labs(x="", y ="")
