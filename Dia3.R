setwd("Dropbox/R4DS/")
library(datasauRus)
library(ggplot2)
library(gganimate)
library(tidyverse)
# función para usar mutate basado en condiciones 
# tomada de https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-several-columns-on-a-subset-of-rows
mutate_cond <- function(.data, condition, ..., new_init = NA, envir = parent.frame()) {
    # Initialize any new variables as new_init
    new_vars <- substitute(list(...))[-1]
    new_vars %<>% sapply(deparse) %>% names %>% setdiff(names(.data))
    .data[, new_vars] <- new_init
    
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data %>% filter(condition) %>% mutate(...)
    .data
}

# Graficamos el dino para ubicar los puntos e identificar regiones
ggplot(subset(datasaurus_dozen, dataset =="dino"),aes(x=x,y=y)
       )+geom_point()+scale_x_continuous(breaks = seq(20,100,10)
       )+scale_y_continuous(breaks = seq(0,100,10))+geom_abline(
        intercept=-12)
datasaurus_dozen %>% mutate(region="cuerpo") -> datasaurus_dozen

# Definimos las regiones
# ojo
datasaurus_dozen %>% mutate_cond(x>45 & x<60 & y>75 &y<88,
                                 region="ojo") ->datasaurus_dozen

# nariz
datasaurus_dozen %>% mutate_cond(x>25 & x<35 & y>55 &y<65,
                                 region="nariz") ->datasaurus_dozen
# boca
datasaurus_dozen %>% mutate_cond((x>30 & x<40 & y>27 &y<35) | (
                                 x>41 & x<59.5 & y>x-12 & y <52 ),
                                 region="boca") ->datasaurus_dozen
# brazo
datasaurus_dozen %>% mutate_cond((x>=40 & x<67.5 & y>5 &y<21) | (
    x>67 & x<80 & y>10 & y < 24),
    region="brazo") ->datasaurus_dozen
datasaurus_dozen$region=factor(datasaurus_dozen$region)

datasaurus_plot=ggplot(datasaurus_dozen, aes(x=x, y=y, color=region))+
    geom_point()+
    theme_minimal() +
    transition_states(dataset, 3, 1) + 
    ease_aes('cubic-in-out')+scale_color_manual(
    values = c("cuerpo"="green4", "boca"="red", "ojo"="navy", 
               "nariz"="gold", "brazo"="orange4")
    )+theme(legend.position = "None")
animate(datasaurus_plot, fps = 10, width = 750, height = 450)
anim_save("datasaurus.gif")
