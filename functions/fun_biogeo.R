#Petite fonction  pour remplir correctement la colonne BIOGEO de d
#toutes les informations se trouvant dans library/StationsRegions, créée avec la fonction RegionsBiogeo

#Ici, ce n'est pas tant la région d'appartenance que la région de référence. Il n'y a pas suffisament de données
#pour les stations aquatiques et alpines : on les compare aux résultats nationaux.




# Gives the altitude of stations, given their coordinates
Altitude <- function(d){
  library(rgbif)
  d <- d %>%
    mutate(LON = as.numeric(LON)) %>%
    mutate(LAT = as.numeric(LAT))
  d <- d[complete.cases(d$LON),]
  coord <- unique(d[,c("LON", "LAT", "NEW.ID_PROG")])
  colnames(coord) <- c("decimalLongitude","decimalLatitude","ID")
  coord <- elevation(coord, username = "zozio")
  colnames(coord) <- c("LON","LAT","NEW.ID_PROG","ALT")

  ##Add altitudes in d
  d <- merge(d,coord, all.x=TRUE)
  return(d)
}


ImportReg <- function(){
  library(sf)
  library(dplyr)

  ##Importation des shapefiles des 4 régions biogéographiques (4 fichiers : shp,shx,prj,dbr)+ conversion de Lambert à WGS84
  ATC <- st_read(dsn = 'library/Regions/ATC/ATC.shp',
                 layer = 'ATC')
  ATC <- st_transform(ATC, crs = 4326)
  Continental <- st_read(dsn = 'library/Regions/Continental/Continental.shp',
                         layer = 'Continental')
  Continental <- st_transform(Continental, crs = 4326)
  LUS <- st_read(dsn = 'library/Regions/LUS/LUS.shp',
                 layer = 'LUS')
  LUS <- st_transform(LUS, crs = 4326)
  Mediterraneen <- st_read(dsn = 'library/Regions/Mediterraneen/Mediterraneen.shp',
                           layer = 'Mediterraneen')
  Mediterraneen <- st_transform(Mediterraneen, crs = 4326)

  ##Affecte la région biogéographique au polygone associé
  Atl_c <- data.frame(REGBIOGEO = "Atlantique_central")
  ATC2 <- data.frame(ATC,Atl_c)
  ATC2 <- ATC2[,-2]
  Cont <- data.frame(REGBIOGEO = "Continental")
  Continental2 <- data.frame(Continental,Cont)
  Continental2 <- Continental2[,-2]
  Lusi <- data.frame(REGBIOGEO = "Lusitanien")
  LUS2 <- data.frame(LUS,Lusi)
  LUS2 <- LUS2[,-2]
  Med <- data.frame(REGBIOGEO = c("Mediterraneen","Mediterraneen"))
  Mediterraneen2 <- data.frame(Mediterraneen,Med)
  Mediterraneen2 <- Mediterraneen2[,-2]

  ##Regroupe toutes les régions en un seul tableau
  polygone <- rbind(LUS2,Continental2, Mediterraneen2, ATC2)
  polygone <- polygone[,-1] #remove ID column

  ##Affecte chaque station à la région biogéographique la plus proche
  polygone_sf <- st_as_sf(polygone,crs=4326)

  return(polygone_sf)
}


RegionsBiogeo <- function(d){
  ##Importation des packages nécessaires
  library(sf)
  library(ggmap)
  library(ggplot2)
  library(ggrepel)
  library(mapdata)
  library(maptools)
  #library(OpenStreetMap)
  library(maps)
  library(mapproj)
  library(dplyr)
  library(rgbif)
  library(geonames)

  # Import polygones regions
  polygone_sf <- ImportReg()

   ##Calcule altitude des stations
  d <- Altitude(d)

  ##Affecte chaque station à la région biogéographique la plus proche
  coord <- unique(d[,c("LON", "LAT", "ALT", "NEW.ID_PROG")])
  coord <- coord %>%
    filter(!is.na(LON)) %>%
    filter(LON !="") %>%
    distinct()
  d_sf <- st_as_sf(coord,coords=c("LON","LAT"),crs = 4326)
  d_sf <- d_sf[complete.cases(d_sf$ALT),]
  d_polygone <- st_join(d_sf, polygone_sf, join = st_nearest_feature)

  ##Affecte la région Altitude_sup_1200m aux stations de plus de 1200m
  d_polygone[d_polygone$ALT >= 1200, "REGBIOGEO"] <- "Altitude_sup_1200m"

  coord_biogeo <- left_join(d_polygone, coord)

  write.csv2(coord_biogeo[,1:5], file = "library/reg_biogeo.csv", row.names = FALSE)

  ##Add biogeographic region to d
  d <- left_join(d, d_polygone)


  return(d)
}



CarteBiogeoAll <- function(d, print.fig=TRUE, save.fig=TRUE){
  ##Carte des stations associées à leur région biogéographique

  polygone_sf <- ImportReg()

  coord <- read.csv2("library/reg_biogeo.csv")
  d_sf <- st_as_sf(coord,coords=c("LON","LAT"),crs = 4326)
  d_polygone <- st_join(d_sf, polygone_sf, join = st_nearest_feature)


  #plot map
  veccol <- c("#e41a1c", "#f698c8", "#85a6ec", "#5cdf8d", "#bb95db")
  vecfill <- c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4")
  gg <- ggplot() + geom_sf(data=polygone_sf,aes(fill=REGBIOGEO),colour="black",alpha=.25)
  gg <- gg + geom_sf(data=d_polygone,aes(colour=REGBIOGEO))
  gg <- gg + scale_colour_manual(values = veccol,name = "Régions", label = c("Altitude supérieure à 1200m","Atlantique central","Continental","Lusitanien","Méditerranéen"))
  gg <- gg + scale_fill_manual(values = vecfill)
  gg <- gg + guides(fill = FALSE) #enl?ve la l?gende pour les polygones

    if(print.fig) plot(gg)

  if(save.fig){
    ggfile <- paste("output/Regions_biogeo.png",sep="")
    ggsave(ggfile,gg,width=7, height=7)
  }

}



# We have created 5 biogeographic regions, but there are not enough stations to consider them separately
# So we group "Atlantique central" and "Lusitanien" to form a larger "Atlantique" region,
#and compare the rest to the national reference

assignRegion<-function(d){
  require(dplyr)
  d <- RegionsBiogeo(d)

  d <- d %>%
    mutate(BIOGEOREF = ifelse(HABITAT == "Terrestre" & (REGBIOGEO == "Atlantique_central" | REGBIOGEO == "Lusitanien"), "Atlantique", "National"))
  d$BIOGEO_HAB <- paste0(d$BIOGEOREF,"_",d$HABITAT)
  
  d_biogeo <- unique(d[,c("NEW.ID_PROG","BIOGEOREF","REGBIOGEO","HABITAT","BIOGEO_HAB","ALT","LON", "LAT")])
  write.csv2(d_biogeo, file = "library/reg_biogeo.csv", row.names = FALSE)
  
  print("REGIONS ASSIGNEES")
  return(d)
}



## Fonction qui compte le nombre de stations actives par annee, habitat et region biogeographique
ComptageAllReg <- function(d){

  ## Premiere partie : creation de dCompt = tableau sans doublon des stations, annees d activite, region biogeo et habitat
  dCompt <- select(d,c(NEW.ID_PROG,YEAR,REGBIOGEO,HABITAT))
  dCompt <- distinct(dCompt)

  ## Deuxi?me partie : synth?se de dCompt qui groupe le nombre de stations par crit?re
  dCompt <- group_by(dCompt, YEAR, REGBIOGEO, HABITAT)
  dCompt <- summarise(dCompt, YEAR, REGBIOGEO, HABITAT, n = n())
  dCompt <- distinct(dCompt)

  ## Sauvegarde du tableau de données en CSV
  #write.csv2(dCompt, "library/comptage_stations.csv")

  return (dCompt)
}

# Avec seulement Atlantique
## Fonction qui compte le nombre de stations actives par annee, habitat et region biogeographique
ComptageAtl <- function(d){
    dCompt <- d %>%
    select(NEW.ID_PROG,YEAR,BIOGEOREF,HABITAT) %>%
    distinct() %>%
    group_by(YEAR, BIOGEOREF, HABITAT) %>%
    count()
  return (dCompt)
}


test <- FALSE
if(test == TRUE) {

    d<-read.csv2("data_DB/data.csv",sep="\t")
    d_reg <- assignRegion(d)
    d_reg <- d_reg %>%
        select(-geometry)
    write.csv(d_reg, file = "data_DB/data_reg.csv",row.names = FALSE)
}
