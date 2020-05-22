library(raster)
library(sfsmisc)
library(ggplot2)
library(cowplot)
library(viridis)

color.image <-brick("Photos/20180805_155725.jpg")
color.values <- getValues(color.image)
bw.values <- color.values[,1]*0.21 + color.values[,1]*0.72 + color.values[,1]*0.07
imagen<-as.data.frame(xy.grid(1:nrow(color.image), 1:ncol(color.image)))
names(imagen)<-c("x","y")
imagen$bwValues<-bw.values 
g<-ggplot(imagen, aes(x=x,y=y, fill=bwValues))+geom_tile()+
    theme_minimal()+scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+theme(axis.text = element_blank(),
    axis.ticks = element_blank(),legend.position = "None")+
    labs(x="",y="")
g1<-ggdraw() +draw_image("Photos/20180805_155725_1.jpg")
g2<-g+scale_fill_gradient(low = "white", high = "black")+
    ggtitle("Black and white")
g3<-g+scale_fill_gradientn(colours=viridis(10))+
    ggtitle("Viridis")
g4<-g+scale_fill_gradientn(colours=magma(10))+
    ggtitle("Magma")
g5<-g+scale_fill_gradientn(colours=heat.colors(10))+
    ggtitle("Heat")
g6<-g+scale_fill_gradientn(colours=rainbow(10))+
    ggtitle("Rainbow")

ggsave(plot_grid(g1,g2,g3,g4,g5,g6, nrow=2), file="Dia10.jpeg",
       height = 6,width=9)
       
       