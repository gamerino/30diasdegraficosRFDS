library(tidyverse)
library(ggplot2)
library(cowplot)
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
                                   angle = 0, lineheight = 0.9, hjust = 0.5, vjust = 0.5),
          strip.text = element_text(family = base_family, 
                                    face = "plain", colour = "lightseagreen", size = base_size-3)
    )
}

datos<-read.csv("datasets_2038_3474_football.csv", header=T)

datos %>% 
    pivot_longer( cols = Christiano.Ronaldo:Neymar,
             names_to="Player",values_to= "Score") -> datosLong
datosLong$Player=factor(datosLong$Player)
levels(datosLong$Player)=c("Ronaldo", "Messi", "Neymar")
p=ggplot(datosLong, aes(x = Secondary.Skill, y = Score, 
                      group = Player, 
                      color = Player))+
  geom_line(size = 0.6) +
  facet_wrap(~Primary.Skill, scales="free", nrow=2)+
  geom_point(size = 2)+
  labs(title = "" ,
       x = "", color="") +
  my_theme_black()+
  theme(legend.position = c(1,-0.1),legend.justification = c(1,1),
        legend.direction="horizontal",  axis.text.x = element_text(angle=60, hjust = 1),
        legend.text = element_blank())+
  scale_color_brewer(palette = "Pastel1")

my_plot_2 <- ggdraw() +
  draw_plot(p)+
  draw_image("players2.png",  x = 0.4, y = -0.2, scale = 0.28) 
my_plot_2