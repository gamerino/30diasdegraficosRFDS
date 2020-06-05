library(ggplot2)
library(sf)
library(tidyverse)

Argspdf <- read_sf("provincia.json")
dengueData<-read.csv("dengue.csv", sep=";")
Argspdf_crop=st_crop(Argspdf, c("xmin"=-74, "ymin"=-58,
                                "xmax"=-50,"ymax"=-21.7888))
Argspdf_simp = st_simplify(Argspdf_crop,preserveTopology =T,
                           dTolerance = 0)  # 2000 m
Argspdf_simp$nam[17]="Tierra del Fuego"
# calculate points at which to plot labels
centroids <- Argspdf_simp %>% 
    st_centroid() %>% 
    bind_cols(as_data_frame(st_coordinates(.)))    # unpack points to lat/lon columns

dengueData %>% 
    left_join(Argspdf_simp, ., by = c( 'nam'='Provincia')) ->dataPlot 

g=ggplot(dataPlot) + 
    geom_sf(aes(fill = Casos)) + 
    geom_text(aes(X, Y, label = nam), data = centroids, 
              size = 3, color = 'navy')+theme_minimal()+
    labs(x="",y="",title="Número de casos de dengue al 04/05/2020")+
    scale_fill_gradient2(low="mistyrose", mid="lightpink",
    high="deeppink4")
ggsave(g, file="casosDengue.jpeg", width = 6, height = 8)