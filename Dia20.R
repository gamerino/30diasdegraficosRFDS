library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidygraph)
library(ggraph)
library(stringr)

thm <- theme_minimal() +
    theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
    ) 

theme_set(thm)
datos <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t")
#datos=read.csv("data.csv",sep=",", header=T)
datos %>% 
    drop_na() ->datos

datos %>% 
    filter(anio ==2018) %>%
    select(equipo_1,equipo_2,equipo_1_final,equipo_2_final) ->datosSelect
datosSelect %>%
    mutate(from=ifelse(equipo_1_final>=equipo_2_final,equipo_1,equipo_2),
           to=ifelse(equipo_1_final<equipo_2_final,equipo_1,equipo_2),
           difGoals=abs(equipo_1_final-equipo_2_final)) %>%
    select(from, to, difGoals )->dataMod
dataMod %>%
    group_by(from,to)%>%
    summarize(n=n()) ->aux
graph_routes <- as_tbl_graph(dataMod)

graph_routes <- graph_routes %>%
    activate(nodes) %>%
    mutate(
        title = str_to_title(name),
        label = str_replace_all(title, " ", "\n")
    )


graph_routes %>%
    ggraph(layout = "kk") +
    geom_node_text(aes(label = label) , color = "navy",
                   size = 4, show.legend = F) +
    geom_edge_diagonal(aes(colour=as.factor(difGoals)), alpha = 0.6) +
    theme(legend.position = "bottom")+
    scale_edge_color_brewer("Diferencia de goles", palette = "YlGn")


