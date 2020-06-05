library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)
library(cowplot)
library(ggcorrplot)
library(ggdendro)
library(extrafont)

font_import()
loadfonts(quiet = TRUE)

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

datos=read.csv("CompleteDataset.csv",sep=",", header=T)
datos %>% select(Name, Age,Nationality,Overall:Club,Acceleration:Volleys,
                 Preferred.Positions) %>% drop_na() %>% 
    filter(Nationality=="Argentina")->datos
mostFreq=names(head(sort(table(datos$Preferred.Positions), 
                         decreasing = T), n=4))
datos %>%
    filter(Preferred.Positions %in% mostFreq) -> datosFilt

datosFilt %>% 
    mutate_at(names(datosFilt)[7:40], as.numeric)%>%
    mutate_at(names(datosFilt)[7:40], scale) %>% drop_na()-> datosFilt

datosFilt %>% pivot_longer(cols = Acceleration:Volleys, 
    names_to="Feature",values_to= "Value") ->datosFiltLong
# Exploring the predictors
g1=ggcorrplot(cor(datosFilt[,7:40]))
g2=ggplot(datosFiltLong) + geom_density(aes(x=Value, fill=Preferred.Positions), 
    alpha=0.5)+facet_wrap(~Feature)
plot_grid(g1,g2)
datosFilt %>% select(Acceleration:Agility, Ball.control, Crossing:Finishing,
       Long.passing:Long.shots,Penalties,Short.passing:Sliding.tackle,
       Volleys, Preferred.Positions) ->datosFiltRed
datosFiltRed %>% 
    pivot_longer(cols = Acceleration:Volleys, 
                 names_to="Feature",values_to= "Value") ->datosFiltRedLong
datosFiltRedLong$Preferred.Positions=factor(datosFiltRedLong$Preferred.Positions)
ggplot(datosFiltRedLong) + 
    geom_violin(aes(x=Preferred.Positions,y=Value, fill=Preferred.Positions))+
    facet_wrap(~Feature, nrow = 3)+my_theme()+theme(legend.position = "bottom")+
    scale_fill_manual(values = c("seagreen1","navy","deeppink","lightsalmon"))+
    labs(x="")
