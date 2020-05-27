library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)
library(cowplot)
library(ggcorrplot)
library(ggdendro)
library(factoextra)
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
g1=ggcorrplot(cor(datosFiltRed[,1:15]))
g2=ggplot(datosFiltRedLong) + geom_density(aes(x=Value, fill=Preferred.Positions), 
                                        alpha=0.5)+facet_wrap(~Feature)
plot_grid(g1,g2)
clusteringJerarq=hclust(dist(datosFiltRed[,1:15]),
                        method = "single")
clusteringJerarq$labels=datosFilt$Name
clusteringJerarq %>% as.dendrogram ->dend

colors=as.factor(datosFiltRed$Preferred.Positions)
posiciones=levels(colors)
levels(colors)=c("seagreen1","navy", "deeppink", "lightsalmon")

g=fviz_dend(dend, k = 4,
          k_colors = c("navy", "seagreen1", "deeppink", "lightsalmon"),
          label_cols =  colors[clusteringJerarq$order],
          cex = 0.2, main="Jugadores argentinos FIFA 2018")
g+ annotate("text", x = 10, y = 3.8, label =posiciones[2], 
            color=levels(colors)[2])+
    annotate("text", x = 130, y = 3.8, label =posiciones[1], 
             color=levels(colors)[1])+
    annotate("text", x = 228, y = 3.8, label =posiciones[3], 
             color=levels(colors)[3])+
    annotate("text", x = 300, y = 3.8, label =posiciones[4], 
             color=levels(colors)[4])+ylab("")+
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())
