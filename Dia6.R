library(webr)
library(reshape2)
library(dplyr)
library(cowplot)
datos=read.csv("series-hist-ricas-anac.csv", sep=",",header=F, skip=446, nrows=44)
datos=datos[,-c(1:5,7:8)]
datos=as.data.frame(t(datos))
colnames(datos)=datos[1,]
colnames(datos)[1]=c("Anio")
datos=datos[-1,]
datos=datos[,-2]
datosPlot=melt(datos[datos$Anio>=2015,], "Anio")
datosPlot$value=as.numeric(datosPlot$value)
datosPlot %>% filter(!is.na(value)) -> datosPlot
datosPlot  %>% 
    group_by(Anio,variable) %>%
    summarize(n=sum(value)) ->datosPlot

g1=PieDonut(datosPlot,aes(Anio,variable,count=n),
         title="Pasajeros vuelos comerciales internacionales",
         showPieName=FALSE, pieAlpha=1, donutAlpha=1)
g2=PieDonut(datosPlot[datosPlot$Anio=="2020",],aes(variable,count=n),
            title="Pasajeros vuelos comerciales internacionales 2020",
            showPieName=FALSE, pieLabelSize=3, ratioByGroup=F, pieAlpha=1, donutAlpha=1)
ggsave(plot_grid(g1,g2, nrow=1), filename="DonutR.jpeg", height = 6, width=12  )
