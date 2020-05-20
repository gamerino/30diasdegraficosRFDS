library("malariaAtlas")
library(ggplot2)
library(dplyr)

#Seleccionamos el dataset
MDG_pr_data <- getPR(country = "Ghana", species = "both")
MDG_pr_data %>%
    filter(!is.na(pr))%>%
    select(-(rdt_type:citation3))%>%
    select(-c(dhs_id,country,country_id, continent_id))->MDG_pr_data
# Elijo esta especie que es la observada
MDG_pr_data %>% filter(species=="P. falciparum") ->MDG_pr_dataF
#construimos la grilla
latitude <- seq(min(MDG_pr_dataF$latitude), max(MDG_pr_dataF$latitude), by = .01)
longitude <- seq(min(MDG_pr_dataF$longitude), max(MDG_pr_dataF$longitude),by = .005)
map.grid <- expand.grid(longitude = longitude, latitude = latitude)
prF <- loess(pr ~ latitude * longitude, 
           degree = 2, data = MDG_pr_dataF)
MDG.fit.F <- predict(prF, map.grid)
#Graficamos
g=ggplot() +geom_contour_filled(data = mutate(map.grid, fit = as.numeric(MDG.fit.F)),
    aes(x = latitude, y = longitude, z = fit, fill =
    ..level..), 
    binwidth = 0.1) + scale_x_continuous(name = "Latitud", expand = c(0, 0)) +
    scale_y_continuous(name = "Longitud", expand = c(0, 0)) +
    coord_equal() + theme_minimal()+labs(fill="Pr", title=
    "Niveles de PR en Ghana para la especie P. falciparum")+theme(
    legend.position = "bottom")+guides(fill=guide_legend(nrow=2,byrow=TRUE))
#Anotamos las localidades con numero promedio de Pr mayor al 3er cuartil de 
#la distribucion de dicha variable
MDG_pr_dataF %>% group_by (latitude, longitude, site_name) %>%
    summarize(avgPr=mean(pr)) -> annotData

annotData %>%  filter(avgPr>quantile(annotData$avgPr, prob=0.75))->annotData

g<-g+geom_text(data=annotData,aes(x=latitude, y=longitude, 
            label=site_name), color="pink", size=3)

ggsave(g, file="ContourPlot.jpeg", width = 7,height = 5)



