library(dplyr)
library(ggplot2)
library(tidyr)
library(circlize)
library(RColorBrewer)
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

datos=read.csv("cannabis.csv",sep=",", header=T)
datos %>% 
    select(Strain, Type,Effects,Flavor)%>%
    drop_na() ->datos
datos %>%
    separate(Flavor,c("F1","F2","F3","F4"), sep=",") %>%
    separate(Effects,c("E1","E2","E3","E4","E5"), sep=",") %>%
    filter(E1!="None"|F1!="None") %>%
    arrange(Type) -> datosProc

datosProc %>%
    pivot_longer(cols=F1:F4,names_to = "Flavor_order", values_to = "Flavor") %>%
    pivot_longer(cols=E1:E5,names_to = "Effect_order", values_to = "Effect") ->datosLong
datosLong %>%
    group_by(Type, Flavor, Effect) %>%
    summarise(value=n()) %>%
    filter(Flavor!="")%>%
    filter(Effect!="") %>%
    rename("from"="Flavor", "to"="Effect") ->datosPlot

# Filtramos los más populares
datosPlot %>%
    group_by(from) %>%
    summarise(n=sum(value)) %>%
    arrange(desc(n)) %>%
    head(n=10) %>%
    select(from) ->mostFreqFlavor
datosPlot %>%
    group_by(to) %>%
    summarise(n=sum(value)) %>%
    arrange(desc(n)) %>%
    head(n=10) %>%
    select(to)->mostFreqEffect

datosPlotFilt=datosPlot%>%
    filter(from %in% mostFreqFlavor$from)%>%
    filter(to %in%  mostFreqEffect$to)
circos.clear()

grid.col = c(brewer.pal(10, "Spectral"),brewer.pal(9, "Blues"),"navy")
names(grid.col) = c(unique(datosPlotFilt$from),unique(datosPlotFilt$to))

circos.clear()
#hybridPlot
chordDiagram(datosPlotFilt%>%
                 filter(Type=="hybrid")%>%
                 ungroup()%>%
                 select(-Type), grid.col = grid.col)
title("Híbrida")
circos.clear()
#sativaPlot
chordDiagram(datosPlotFilt%>%
                            filter(Type=="sativa")%>%
                            ungroup()%>%
                            select(-Type),grid.col = grid.col)
title("Sativa")
circos.clear()
#indicaPlot
chordDiagram(datosPlotFilt%>%
                            filter(Type=="indica")%>%
                            ungroup()%>%
                            select(-Type),grid.col = grid.col)
title("Indica")



