library(dplyr)

rootdir="C:/Users/Silvio/Documents/GitHub/"
rootdir1="C:/Users/Silvio/Documents/"
setwd(paste0(rootdir,"U-of-Florida-Yucatan-Project/"))
mapdir="ArcGIS Explorer/My Basemaps/MEX_adm/"
mapdir1="ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/"
mapdir2="ArcGIS Explorer/My Basemaps/INEGI mapa/conjunto_de_datos/"

####
urbana2 <- st_read("Shapefiles/INEGI mapa/localidad250_a.shp",quiet=T,stringsAsFactors = F)
urbana2$nombre<- iconv(urbana2$nombre,from="UTF-8",to="ASCII//TRANSLIT")
urbana2 <- st_transform(urbana2,crs=4326)##Converting coordinates from NAD83 to WGS84
####cHANGING LOCALITIES, RENAMING SOME, REMOVING OTHERS THAT ARE DUPLICATES
urbana2$nombre[162] <- "CHOLUL MERIDA"
urbana2$nombre[1] <- "TEMOZON ABALA"
urbana2 <- urbana2[-c(43,61,102,153,242,295),]
rural<-st_read(paste0(rootdir1,mapdir1,"yuc_ageb_rural.shp"),quiet=T,stringsAsFactors = F)#Encuesta intercensal
rural$CVE_MUN <- as.numeric(rural$CVE_MUN)
rural$CVE_ENT <- as.numeric(rural$CVE_ENT)
municipios<- read.csv("catalogo de municipios_mod.csv",header=T,stringsAsFactors = F)#;View(municipios)
municipios$Nombre.del.Municipio <- toupper(municipios$Nombre.del.Municipio)
#####

municipios2 <- municipios
colnames(municipios2)[3] <- c("CVE_MUN")
rural<- left_join(rural,municipios2[3:4])
#View(rural)

addresses2_hits <- addresses2_hits[order(addresses2_hits[,5]),] ; View(addresses2_hits)
ageb <- c()
mun <- subset(addresses2_hits,(grepl("municipality matches!",addresses2_hits$LAT_HITS)))
mun<- mun$MUNICIPIO
for (i in mun) {#randomly creating various AGEB's to add to df
  tmp <- rural$CVE_AGEB[rural$Nombre.del.Municipio == i]#whichever ageb's equal that iteration name
  tmp1 <- sample(tmp, size = 1)#sample 1 from those ageb's
  ageb <- c(ageb, tmp1)#adding to the vector every time
}
ageb

#municipality placeholder to add coordinates here, then later add to addresses2_hits
placeholder <- data.frame(Nombre.del.Municipio=mun,CVE_AGEB=ageb)%>%
  left_join(rural)
View(placeholder)

################################################################################

####cODE For sampling random coordinates####
#For loop, nesting a while loop with target depending on the numb of schools needed
# install.packages("lwgeom")
library(lwgeom)

#First for municipalities
target=1
n <- 0
mun.coord.vector <- c()
#For every school that only identified by municipio, get random coords from that municipio
for (i in seq(placeholder$Nombre.del.Municipio)){
  # print("count")
  n<-0
  while(n < target){
    rand.coords<- st_sample(placeholder$geometry[i],target,"random")#+2*target
    n <- length(rand.coords)
    # print(n)
    if (n >= target) mun.coord.vector <- append(mun.coord.vector,rand.coords[1])#append each time
  }
}
mun.coords <- st_coordinates(mun.coord.vector[1:length(placeholder$Nombre.del.Municipio)]);View(mun.coords)
mun.coords <- as.data.frame(mun.coords)
placeholder$mun.coords <- mun.coords[1:length(placeholder$Nombre.del.Municipio),]
# placeholder$mun.coords <- NULL
# for (i in seq(mun.coords$X)){
#   placeholder$mun.coords[i] <- c(mun.coords$X[i],mun.coords$Y[i])
# }

# placeholder$mun.xcoordinates <- (st_coordinates(coord.vector[1:191,]))
# placeholder$mun.ycoordinates <- st_coordinates(coord.vector[])
# testing2 <- c()
# testing2 <- (st_coordinates(coord.vector[]))
# st_coordinates(coord.vector[1:3])

####Now, for localities####

loc.placeholder <- subset(addresses2_hits,(grepl("locality matches!",addresses2_hits$LAT_HITS)))
urbana2_subset <- data.frame(LOCALIDAD = urbana2$nombre,geometry = urbana2$geometry)
loc.placeholder <- left_join(loc.placeholder,urbana2_subset)#good, 1373 hits now

target=1
n <- 0
loc.coord.vector <- c()
#For every school that only identified by locality, get random coords from that locality
for (i in seq(loc.placeholder$LOCALIDAD)){#
  # print("count")
  n<-0
  while(n < target){
    rand.coords<- st_sample(loc.placeholder$geometry[i],target,"random")#+2*target
    n <- length(rand.coords)
    # print(n)
    if (n >= target) loc.coord.vector <- append(loc.coord.vector,rand.coords[1])#append each time
  }
}

loc.coords <- st_coordinates(loc.coord.vector[1:length(loc.placeholder$LOCALIDAD)]);View(loc.coords)
loc.coords <- as.data.frame(loc.coords)
loc.placeholder$loc.coords <- loc.coords[1:length(loc.placeholder$LOCALIDAD),]

##################################################################
#NOW, REPLACE ALL VALUES IN ADDRESSES2_HITS WITHOUT COORDS
#Municipios start at 2748 and go until 3290
#Localities start at 1375 and go until 2747
# addresses2_hits[3290,]

addresses2_hits$LAT_HITS[2748:3290] <- mun.coords$Y
addresses2_hits$LNG_HITS[2748:3290] <- mun.coords$X
addresses2_hits$LAT_HITS[1375:2747] <- loc.coords$Y
addresses2_hits$LNG_HITS[1375:2747] <- loc.coords$X
addresses2_hits$LAT_HITS[1] <- 20.9491951
addresses2_hits$LNG_HITS[1] <- -89.66059559999999

schools2_x <- as.numeric(addresses2_hits$LAT_HITS)
schools2_y <- as.numeric(addresses2_hits$LNG_HITS)
# write.table(addresses2_hits,"addresses.txt",sep="\t",row.names = F)
###NOTES

##Example 
# mun <- c(1, 1, 1, 6, 6, 6)
# ageb <- c()
# 
# for (i in mun) {#randomly creating various AGEB's to add to df
#   tmp <- rural$CVE_AGEB[rural$CVE_MUN == i]#whichever ageb's equal that iteration name
#   tmp1 <- sample(tmp, size = 1)#sample 1 from those ageb's
#   ageb <- c(ageb, tmp1)#adding to the vector every time
# }
# ageb
# df <- data.frame(CVE_MUN = mun, CVE_AGEB = ageb) %>%
#   left_join(rural)
# df
#so here add ageb to addresses2

# }
#     
#     if (n >= target){
#       portion <- sample(x = 1:n, target)
#       rand.coords <- rand.coords[portion]#sampling from random positions (13,10,12) in rand.coords
#       #from above to get exactly the target of 10
#     }
#   }
# 
# > nrow(unique(select(rural, CVE_MUN, CVE_AGEB)))
# [1] 346
# > length(unique(select(rural, CVE_MUN, CVE_AGEB)))
# [1] 3
# > unique(select(rural, CVE_MUN, CVE_AGEB)) %>% nrow()
# [1] 346
# rural %>% select(CVE_MUN,CVE_AGEB) %>% unique
