#------------------------------------------------------------------------
library(RPostgres)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(ggnewscale)
dvr         <- RPostgres::Postgres()
db          <- 'postgres'  ##Nombre de la BBDD
host_db     <- 'localhost'
db_port     <- '5432' 
db_user     <- 'postgres'  ##Tu usuario
db_password <- 'gflorezc' ##Tu contraseña 

# 3.0 Conexión
con <- dbConnect(dvr, dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
dbListTables(con)

Rios_Peru_Cuenca<- st_read(con, layer = "Rios_Peru_Cuenca")
Rios_Peru_Cuenca<- st_transform(Rios_Peru_Cuenca ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Rios_Peru_Cuenca<- st_read(con, layer = "Rios_Peru_Cuenca")
Rios_Peru_Cuenca<- st_transform(Rios_Peru_Cuenca ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Per          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()

Rios_Peru_Norte<- st_read(con, layer = "Rios_Peru_Norte")
Rios_Peru_Norte<- st_transform(Rios_Peru_Norte ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Rios_peru_Sur<- st_read(con, layer = "Rios_peru_Sur")
Rios_peru_Sur<- st_transform(Rios_peru_Sur ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

summary(Rios_Peru_Norte$MAIN_RIV)
colores<- c("red", "blue", "#598DB6","#598DB6", "#598DB6")
cortes <- c(60443230, 61264851)

Rio_navegables<- st_read(con, layer = "Rio_navegables")
Rio_navegables<- st_transform(Rio_navegables ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

ggplot()+
geom_sf(data = Rio_navegables, color= "#679436", linewidth = 0.1)

60443230
library(dplyr)



library(grid)
library(png)
library(ggimage)
Logo <- readPNG("Logo R.png", FALSE)
Logo_png <- rasterGrob(Logo, x = unit(0.9, "npc"),y = unit(0.9, "npc"), width = unit(0.2, "npc"))


Expo=ggplot()+
  geom_sf(data = Rios_Peru_Cuenca, aes(color= MAJ_NAME),linewidth =3)+
  scale_color_manual(values = c("red", "#7209b7", "#3a5a40", "#598DB6", "#598DB6"))+
  geom_sf(data = Per, fill=NA, color="black", size=1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.9,"cm"),
        legend.position = c(0.25,0.10),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=10, family="serif"),
        legend.title = element_text(size=10, family="mono", face='bold',hjust=0.5))+
  labs(color="")+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  ggplot2::annotate(geom = "text", x = -80, y =0, hjust = 0, vjust = 1, 
           label = "Ríos del Perú",size = 6, family="serif", color = 
             "black",  face = "bold")+
             ggplot2::annotate(geom = "text", x = -80, y =-0.5, hjust = 0, vjust = 1, 
                               label = "Data: Hydrosheds",size = 3, family="serif", color = 
                                 "black",  face = "bold")+
                                 ggplot2::annotate(geom = "text", x = -80, y =-0.8, hjust = 0, vjust = 1, 
                                                   label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                     "black",  face = "bold")+
 annotation_custom(Logo_png)
                   
library(ggpubr)
legend <- get_legend(Expo)                                                


Mapa = ggplot()+
  geom_sf(data = Rios_Peru_Norte, aes(color= MAIN_RIV),linewidth = 0.1)+
  geom_sf(data = Rios_peru_Sur, color= "#679436", linewidth = 0.1)+
  scale_color_gradientn(colours =  colores ,
                        breaks = cortes ,)+
  geom_sf(data = Per, fill=NA, color="black", size=1)+
  geom_sf(data = Rio_navegables, color= "red", linewidth = 0.4, alpha =0.3)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.9,"cm"),
        legend.position = "none",
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=10, family="serif"),
        legend.title = element_text(size=10, family="mono", face='bold',hjust=0.5))+
  labs(color="")+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  ggplot2::annotate(geom = "text", x = -80, y =0, hjust = 0, vjust = 1, 
                    label = "Ríos del Perú",size = 6, family="serif", color = 
                      "black",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -80, y =-0.5, hjust = 0, vjust = 1, 
                                        label = "Data: Hydrosheds",size = 3, family="serif", color = 
                                          "black",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -80, y =-0.8, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "black",  face = "bold")+
                                                              annotation_custom(Logo_png)+
  annotation_custom(legend , xmin = -80, xmax = -78, ymin =-19, ymax=-15)
Mapa
ggsave(plot=Mapa,"Mapa de Rios1.png",units = "cm",width = 21, #alto
       height = 29, #ancho
       dpi=1200)



















