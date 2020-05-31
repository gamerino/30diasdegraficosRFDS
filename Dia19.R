library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
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

datos=read.csv("data.csv",sep=",", header=T)
datos %>% 
    drop_na() ->datos
datos %>%
    mutate(date=as.Date(datos$date)) %>%
    mutate(anio=year(date)) %>%
    filter(anio >=1999 & anio < 2019)-> datos 
#sacamos 2019 porque está solo hasta junio

datos %>% 
    separate(location, c("City","State", "Country"), 
             sep=", ") ->datos
datos$Country[is.na(datos$Country)]=datos$State[is.na(datos$Country)]
datos %>%
    group_by(anio, Country) %>% 
    summarise(n=n())->datosFilt
datosFilt %>%
streamgraph("Country", "n", "anio", interactive = T,
            top = 30, height = 500,width = 700) %>%
    sg_annotate("# combates UFC últimos 20 años",
                x = as.Date("2000-01-01"), y = 400,
                size = 20,color="Navy")%>%
    sg_axis_x(1, "year", "%Y") %>%
    sg_fill_brewer("Set3") %>%
    sg_legend(show=T, label="País: ")



