library(ggplot2)
library(treemapify)

datos=read.csv("Argentina.csv", header=T, sep=",")
datos$Poblacion=as.numeric(gsub(",", ".", gsub("\\.", "", datos$Poblacion)))
datos$Superficie=as.numeric(gsub(",", ".", gsub("\\.", "", datos$Superficie)))
ggplot(datos, aes(area = Superficie, fill = Poblacion,
                  label = Jurisdiccion, subgroup=Region.integrada)) +
    geom_treemap() +geom_treemap_subgroup_border() +
    geom_treemap_subgroup_text(place = "bottomright", grow = T, alpha = 0.8, 
    colour = "orange", fontface = "italic", min.size = 0) +
    geom_treemap_text(colour = "navyblue", place = "centre",
                      alpha=0.8, reflow = T, min.size = 0)+
    scale_fill_gradient2(low="thistle1", high="deeppink4")

