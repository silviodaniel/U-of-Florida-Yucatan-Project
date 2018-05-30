library(cartography)
library(sf)
library(sp)

rootdir="C:/Users/Silvio/Documents/GitHub/"
#rootdir1="C:/Users/Silvio/Documents/"
setwd(paste0(rootdir,"U-of-Florida-Yucatan-Project/"))
mapdir="ArcGIS Explorer/My Basemaps/MEX_adm/"
# mapdir1="ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/"
mapdir2="ArcGIS Explorer/My Basemaps/INEGI mapa/conjunto_de_datos/"
# mapdir4="ArcGIS Explorer/My Basemaps/Diego 2010 census/scince_2010/shps/yuc/"
# mapdir5="ArcGIS Explorer/My Basemaps/eleccion_2010/eleccion_2010/todo/yuc/cartografiadigital_ife/"

#Ben's code below 
# loc <- read.csv("catalogo de municipios.csv")
# colnames(loc) <- c("CVE_ENT", "NAME_ENT", "CVE_MUN", "NAME_MUN")
# urbana<- st_read(paste0(rootdir,mapdir1,"yuc_ageb_urbana.shp"),quiet=T)#encuesta intercensal
# rural<-st_read(paste0(rootdir,mapdir1,"yuc_ageb_rural.shp"),quiet=T,stringsAsFactors = F)#Encuesta intercensal
# encuesta<-read.csv(paste0(rootdir,mapdir1,"catalogos/localidades urbanas y rurales amanzanadas.csv"),
#                    header=T)
# urbana_mun <- read.csv("Linux Data/localidades urbanas y rurales amanzanadas_mod.csv",header=T)
# urbana_mun <- subset(urbana_mun,urbana_mun$ENTIDAD==31);length(unique(urbana_mun$NOMBRE.DE.LOCALIDAD))
#Encuesta 2015, but same issue that can't geolocate these
#611 unique localities
# rural$CVE_ENT <- as.numeric(rural$CVE_ENT)
# rural$CVE_MUN <- as.numeric(rural$CVE_MUN)
# 
# # head(rural)
# tmp <- left_join(rural, loc)
# head(tmp)
# ?left_join.sf
# 
# tmp1 <- rural$geometry[1]
# tmp1
# ?`cartography-package`
# getBorders(tmp1)

# runif
###########################################################################################
urbana2 <- st_read("Shapefiles/INEGI mapa/localidad250_a.shp",quiet=T,stringsAsFactors = F)
#INEGI mapa
mex2 <- st_read("Shapefiles/MEX_adm2.shp",quiet=T,stringsAsFactors = F)
mex2 <- subset(mex2, mex2$ID_1==31)
st_bbox(mex2[1,])#get bbox of first line
# length(unique(urbana2$nombre))#340 loclaities
#has good locality names
addresses2 <- read.csv("Linux Data/addresses2_mod2.csv",header=T,stringsAsFactors = F)

# 
# urbana3 <- st_read(paste0(rootdir,mapdir4,"yuc_loc_urb.shp"),quiet=T,stringsAsFactors = F);View(urbana3)
# urbana4 <- st_read(paste0(rootdir,mapdir5,"yuc_limite_localidad.shp"),quiet=T,stringsAsFactors = F);View(urbana4)
urbana4 <- st_read("C:/Users/Silvio/Documents/ArcGIS Explorer/My Basemaps/eleccion_2010/eleccion_2010/todo/yuc/cartografiadigital_ife/yuc_limite_localidad.shp",quiet=T,stringsAsFactors = F);View(urbana4)
length(unique(urbana4$NOMBRE))
#389 localities
# View(urbana_mun)

####################################################################
##Converting coordinates from NAD83 to WGS84
library(gdal)
st_bbox(urbana2[1,])

####Example
nad83_coords <- data.frame(x=c(577430), y=c(2323270)) # My coordinates in NAD83
nad83_coords <- nad83_coords *.3048 ## Feet to meters
coordinates(nad83_coords) <- c('x', 'y')
proj4string(nad83_coords)=CRS("+init=esri:102272") # West Illinois
## Erroneous code, NAD83 to NAD83
## coordinates_deg <- spTransform(nad83_coords,CRS("+init=epsg:3436"))
## Corrected with WGS84 to yield Lat/Long
coordinates_deg <- spTransform(nad83_coords,CRS("+init=epsg:4326"))
coordinates_deg
####

urbana2[1:10]
urbana2 <- st_transform(urbana2,crs=4326)
head(urbana2)

?st_transform()


#############################################
urbana2$nombre<- iconv(urbana2$nombre,from="UTF-8",to="ASCII//TRANSLIT")

#CHecking which localities from addresses2 are missing in urbana2
missing_urbana2=NULL
# missing_urbana4=NULL
for (i in seq(addresses2$LOCALIDAD)){
  if (addresses2$LOCALIDAD[i] %in% urbana2$nombre==F ){#if the address is not in urbana2 (==FALSE)
    missing_urbana2[i]=addresses2$LOCALIDAD[i]
  }
  else if (addresses2$LOCALIDAD[i] %in% urbana2$nombre){
    next
  }
}

#Creating vector of localities from addresses2 that are in urbana2
urbana2_hits=NULL
for (i in seq(addresses2$LOCALIDAD)){
  if (addresses2$LOCALIDAD[i] %in% urbana2$nombre==T ){#if the address is not in urbana2 (==FALSE)
    urbana2_hits[i]=addresses2$LOCALIDAD[i]
  }
  else if (addresses2$LOCALIDAD[i] %in% urbana2$nombre){
    next
  }
}
urbana2_hits <- (which(!is.na(urbana2_hits)))
# View(which(!is.na(urbana2_hits)))#2715 urban hits ie the file names match up, out of 3291!

#Copy
# urbana4_hits=NULL
# for (i in seq(addresses2$LOCALIDAD)){
#   if (addresses2$LOCALIDAD[i] %in% urbana4$NOMBRE==T ){#if the address is not in urbana2 (==FALSE)
#     urbana4_hits[i]=addresses2$LOCALIDAD[i]
#   }
#   else if (addresses2$LOCALIDAD[i] %in% urbana4$NOMBRE){
#     next
#   }
# }
# urbana4_hits <- (which(!is.na(urbana4_hits)))

##Creating vector of municipios from addresses2 that are in mex2
mex2_hits=NULL
for (i in seq(addresses2$LOCALIDAD)){
  if (addresses2$LOCALIDAD[i] %in% urbana2$nombre==T ){#if the address is not in urbana2 (==FALSE)
    mex2_hits[i]=addresses2$LOCALIDAD[i]
  }
  else if (addresses2$LOCALIDAD[i] %in% urbana2$nombre){
    next
  }
}

# head(which(!is.na(urbana2_hits)))


#######################################################################################################
#Fixing up the mex2 data
mex2$NAME_2 <- iconv(mex2$NAME_2,from="UTF-8",to="ASCII//TRANSLIT");View(mex2)
#now, capitalizing
mex2$NAME_2 <- toupper(mex2$NAME_2)

#CHecking which localities from addresses2 are missing in mex2 (municipalities)
missing_mex2=NULL
for (i in seq(addresses2$MUNICIPIO)){
  if (addresses2$MUNICIPIO[i] %in% mex2$NAME_2==F ){#if the address is not in urbana2 (==FALSE)
    missing_mex2[i]=addresses2$MUNICIPIO[i]
  }
  else if (addresses2$MUNICIPIO[i] %in% mex2$NAME_2){
    next
  }
}
#findings: there are 106 municipios, and they exactly match up with addresses2 number of municipios, along with
#number of polygons so each polygon corresponds exactly to 1 municipio

###############################################################################################
####################################################################################################
##TO COUNT HOW MANY additional SCHOOL HITS FROM urbana2 by LOCALITY NAME
#for each i, change the addresses2_hits for that value i position (urbana2_hits[i]) to "success"
#for i in addresses2_hits, if if that ith position is equal to one 
# of the position numbers in addresses2_hits, AND
#urbana2_hits$HITS contains ZERO_RESULTS, and , then replace that value with "locality matches!"

#new modified addresses with hits from Linux
addresses2_hits <-read.csv("Linux Data/addresses2_mod2_hits.csv",header=T,stringsAsFactors = F);View(addresses2_hits)

for (i in seq(length(addresses2_hits$HITS))){
  if ((i %in% urbana2_hits && grepl("ZERO_RESULTS",addresses2_hits$HITS[i]))==T){
    addresses2_hits$HITS[i] <- "locality matches!" #phrase: "locality matches!"
  }
}
View(addresses2_hits)

# for (i in seq(length(addresses2_hits$HITS))){
#   if ((i %in% urbana4_hits && grepl("ZERO_RESULTS",addresses2_hits$HITS[i]))==T){
#     addresses2_hits$HITS[i] <- "locality matches!" #phrase: "locality matches!"
#   }
# }
# View(addresses2_hits)
#HOW TO CREATE A FUNCTION BASED ON THIS??
# add_school_hits=function(hits_vec,phrase){
#   for (i in seq(length(addresses2_hits$HITS))){
#     if ((i %in% hits_vec && grepl("ZERO_RESULTS",addresses2_hits$HITS[i]))==T){#hits_vec is urbana2_hits
#       addresses2_hits$HITS[i] <- phrase #phrase: "locality matches!"
#     }
#   }
# }
# add_school_hits(urbana2_hits,"locality matches")

View(addresses2_hits)#works!
length(which(addresses2_hits$HITS=="locality matches!"))#998 additional hits! (urbana2)
length(which(addresses2_hits$HITS=="locality matches!"))#951 add hits! (urbana4)

which(!is.na(missing_urbana2))
unique(missing_urbana2)#count unique localities
length(unique(missing_urbana2))/length(unique(addresses2$LOCALIDAD))#43% missing localities! 254/587
#587 unique localities in total, so we have 254 or about 57% of them
length(unique(missing_urbana4))/length(unique(addresses2$LOCALIDAD))#46% missing localities! 268/587

missing_urbana2[2065:2095]

#######
###############################################################################################
length(which(grepl("ZERO_RESULTS",addresses2_hits$HITS)==T))
#There should be 193, but there are just 191 no results from addresses2 (won't add up to 3290 schools)

##TO COUNT HOW MANY additional SCHOOL HITS FROM mex2 by MUNICIPALITY NAME
for (i in seq(length(addresses2_hits$HITS))){
  if ((grepl("ZERO_RESULTS",addresses2_hits$HITS[i]))==T){
    addresses2_hits$HITS[i] <- "municipality matches!" #phrase: "municipality matches!"
  }
}

#Figuring out Checking for those 2 missing values
hits_strings=c("ZERO","status","locality")
length(which(grepl("status",addresses2_hits$HITS)==T))#2101
length(which(grepl("municipality",addresses2_hits$HITS)==T))#191
length(which(grepl("locality",addresses2_hits$HITS)==T))#998

checking_status <- subset(addresses2_hits,grepl("status",addresses2_hits$HITS));head(checking_status)
for (i in seq(checking_status$HITS)){
  if ((grepl("OK",checking_status$HITS[i])) ==F){
    print(checking_status[i,])
  }
    
}

# for (i in seq(addresses2_hits$HITS)){
#   if addresses2_hits$HITS[i] %in%
# }

#HOW TO CREATE A FUNCTION BASED ON THIS??
# add_school_hits=function(hits_vec,phrase){
#   for (i in seq(length(addresses2_hits$HITS))){
#     if ((i %in% hits_vec && grepl("ZERO_RESULTS",addresses2_hits$HITS[i]))==T){#hits_vec is urbana2_hits
#       addresses2_hits$HITS[i] <- phrase #phrase: "locality matches!"
#     }
#   }
# }
# add_school_hits(urbana2_hits,"locality matches")

View(addresses2_hits)#works!
length(which(addresses2_hits$HITS=="locality matches!"))#998 additional hits!

which(!is.na(missing_urbana2))
unique(missing_urbana2)#count unique localities
length(unique(missing_urbana2))/length(unique(addresses2$LOCALIDAD))#43% missing localities! 254/587
#587 unique localities in total, so we have 254 or about 57% of them
length(unique(missing_urbana4))/length(unique(addresses2$LOCALIDAD))#46% missing localities! 268/587

missing_urbana2[2065:2095]
###################################################################
####PLOTTING POLYGONS
#Plot one polygon
plot(urbana2$geometry[1])
plot(rural$geometry[1])
points(spsample(urbana2$geometry[1],n=10,"regular"),pch=3)

#sf add randomly sampled cooordinates
rand.coord=st_sample(urbana2$geometry[1],10,"random");rand.coord
# sample(r.coo,)
length(rand.coord)

plot(st_geometry(urbana2$geometry[1]))
plot(st_sample(urbana2$geometry[1],10,"random"),add=T, col='#88888888',pch=20)

##cODE For getting right amount of sampling
#For loop, nesting a while loop with target depending on the numb of schools needed
target=10
n <- 0
install.packages("lwgeom")
library(lwgeom)
while(n <= target){
  rand.coords<- st_sample(urbana2$geometry[1],target,"random")#+2*target
  n <- length(rand.coords)
  print(n)
  if (n >= target){
    portion <- sample(x = 1:n, target)
    rand.coords <- rand.coords[portion]#sampling from random positions (13,10,12) in rand.coords
    #from above to get exactly the target of 10
  }
}
rand.coords

#df with objectid, nombre, schools needed

#checking which multiple localities, repeats, 
nombre=urbana2$nombre
nombre.dummy=NULL
nombre.repeats=NULL
for (i in seq(urbana2$nombre)){
  if (nombre[i] %in% nombre.dummy){
    nombre.repeats<- append(nombre.repeats,(nombre[i]))
  }
  nombre.dummy[i]=nombre[i]
  next
}


plot(st_geometry(urbana2$geometry[153]),add=T, col = "red", lwd = 3)
plot(st_geometry(urbana2$geometry[317]))
##############################################
##importing MEX_adm2-mod.txt
attach(mex2_mun);names(mex2_mun)

#CHecking which names don't match up btw the 2 columns
count=NULL
for (i in seq(1:length(mex2_mun$NAME_2))){
  if(is.na(mex2_mun$VARNAME_2[i])){
    next
    # count[i]=OBJECTID[i]
  }
  else if(mex2_mun$NAME_2[i]!=mex2_mun$VARNAME_2[i]){
    #return(OBJECTID[i])
    count[i]=mex2_mun$NAME_2[i]
  }
  else{
    next
    # print("success")
  }
}

length(count)-length(which(is.na(count)))
#91 names don't match up in total

length(count[1692:1797])-length(which(is.na(count[1692:1797])))
#only 1 name doesn't match up, shown below
which(!is.na(count[1692:1797]))
count[1696]
#OK, so row 1696 is Bokoba in NAME_2 and Bokova in VARNAME_2, but the former is correct!
#So we can just use the NAME_2 column
mex_short=data.frame(CVE_ENT=mex2_mun$ID_1[1692:1797], NAME_ENT=mex2_mun$NAME_1[1692:1797],
                     CVE_MUN=seq(1692:1797),NAME_MUN=mex2_mun$NAME_2[1692:1797])

###
test=left_join(rural,mex_short);head(test)
##!!!! ISSUE: mex is not the same order as rural, and anyways have to change the 
#order to mex bc this is the shp file we will use, so take away rural and use mex2!

# gsub("[[:punct:]]","",NAME_2)
# gsub("[[:punct:]]","",VARNAME_2)
# 
# VARNAME_2
# install.packages("stringr")
# library(stringr)
# 
# str_replace_all(NAME_2, "[^[:alnum:]]", "")
# str_replace_all(VARNAME_2, "[^[:alnum:]]", "")
# 

