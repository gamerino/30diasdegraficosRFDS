library(ggplot2)
library(dplyr)
library (tidyr)
#cargamos los datos
datos<-read.csv("Global_Mobility_Report.csv")
#nos quedamos solo con los datos de Arg
datos %>% filter(country_region=="Argentina") %>%
   select(-country_region_code,-sub_region_2) -> datos
datos$sub_region_1=factor(as.character(datos$sub_region_1)
# formateamos las fechas y definimos las semanas
datos %>%
   mutate(date = as.Date(date,format='%Y-%m-%d'),anio = as.numeric(format(date,format='%Y')), 
          mes = months(date), numDia = as.numeric(format(date,format='%d')),
          numSemana = as.numeric(format(date,format='%V'))) -> datos
datos %>%
  pivot_longer(cols=retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline,
               names_to = "Location", values_to = "Permanencia")->datos

datosMeanWeek = datos %>%
  group_by(country_region,numWeek,Location) %>%
  summarise(meanPermanencia = mean(Permanencia,na.rm=T),sdPermanencia = sd(Permanencia,na.rm=T))
datosMeanWeek

etiquetas.lugares = c(grocery_and_pharmacy_percent_change_from_baseline="Comercios Alimentos", 
                     parks_percent_change_from_baseline="Parques",
                     residential_percent_change_from_baseline="Residencial", 
                     retail_and_recreation_percent_change_from_baseline="Recreacion",
                     transit_stations_percent_change_from_baseline="Estaciones Transporte", 
                     workplaces_percent_change_from_baseline="Lugares de Trabajo")
                     
etiquetas.faceta

g = ggplot() +
  geom_line(data=datosMeanWeek, 
             aes(x=numWeek,y=meanPermanencia)) +
  geom_ribbon(data=datosMeanWeek,
              aes(x=numWeek,ymin=meanPermanencia-sdPermanencia,ymax=meanPermanencia+sdPermanencia,fill=Location),
              alpha=0.2) +
  facet_wrap(~Location, strip.position = "bottom", labeller = labeller(Location=etiquetas.facets)) +
  geom_vline(xintercept = 11, linetype="dashed") +
  scale_fill_brewer(palette = "Spectral") + 
  guides(fill=F) + xlab(NULL) + ylab(NULL) + ggtitle("Incidencia de la cuarentena sobre la movilidad de las personas") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold.italic",size=15),
        aspect.ratio = 1/2,
        text = element_text(size=15, face="italic"))
g

