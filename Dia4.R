library(ggplot2)
library(dplyr)
library (tidyr)
#cargamos los datos
datos<-read.csv("Global_Mobility_Report.csv")
#nos quedamos solo con los datos de Arg
datos %>% filter(country_region=="Argentina" &sub_region_1!="") %>%
  select(-country_region,-country_region_code,-sub_region_2) -> datos
datos$sub_region_1=factor(as.character(datos$sub_region_1))
# formateamos las fechas y definimos las semanas
datos %>%
         mutate(date = as.Date(date,format='%Y-%m-%d'),anio = as.numeric(format(date,format='%Y')), 
         mes = months(date), numDia = as.numeric(format(date,format='%d')),
         numSemana = as.numeric(format(date,format='%V'))) -> datos
datos %>%
         pivot_longer(cols=retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline,
         names_to = "Location", values_to = "Permanencia")->datos
                          
datos <- datos %>%
         group_by(sub_region_1,numSemana,Location) %>%
         summarise(meanPermanencia = mean(Permanencia,na.rm=T),sdPermanencia = sd(Permanencia,na.rm=T))

etiquetas.lugares <- c(grocery_and_pharmacy_percent_change_from_baseline="Comercios Alimentos", 
                      parks_percent_change_from_baseline="Parques",
                      residential_percent_change_from_baseline="Residencial", 
                      retail_and_recreation_percent_change_from_baseline="Recreacion",
                      transit_stations_percent_change_from_baseline="Estaciones Transporte", 
                      workplaces_percent_change_from_baseline="Lugares de Trabajo")
levels(datos$sub_region_1) <- c("CABA", "Buenos Aires", "Catamarca", "Chaco", "Chubut",
                                        "Cordoba","Corrientes", "Entre Rios", "Formosa", "Jujuy",
                                        "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquen", 
                                        "Rio Negro", "Salta", "San Juan", "San Luis", "Santa Cruz",
                                        "Santa Fe", "Santiago del Estero", "Tierra del Fuego", "Tucuman")
g <- ggplot(datos) + geom_line(aes(x=numSemana,y=meanPermanencia, color=Location)) +
     geom_ribbon(aes(x=numSemana,ymin=meanPermanencia-sdPermanencia,
     ymax=meanPermanencia+sdPermanencia,fill=Location), alpha=0.2) +
     facet_wrap(~sub_region_1, strip.position = "bottom", nrow = 4) +
     geom_vline(xintercept = 11, linetype="dashed") +
     scale_fill_brewer(palette = "Set3", labels=etiquetas.lugares, "")+
     scale_color_brewer(palette = "Set3", labels=etiquetas.lugares,"") + 
     guides(fill=guide_legend(nrow=1), color=guide_legend(nrow=1))+
     ggtitle("Incidencia de la cuarentena sobre la movilidad de las personas") +
     labs(x="", y="")+theme_minimal() + theme(axis.text.x = element_blank(),
     plot.title = element_text(hjust = 0.5, face = "bold.italic",size=9),
     text = element_text(size=9, face="italic"), legend.position="bottom",
     legend.margin=margin(0,0,0,0),legend.box.margin=margin(-5,-5,-5,-5))
g
ggsave(g, file="Dia4.jpeg", height = 5,width=8)
