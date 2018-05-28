library(cartography)
library(sf)

rootdir="C:/Users/Silvio/Documents/"
mapdir="ArcGIS Explorer/My Basemaps/MEX_adm/"
mapdir2="ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/"
mapdir3="ArcGIS Explorer/My Basemaps/INEGI mapa/conjunto_de_datos/"
mapdir4="ArcGIS Explorer/My Basemaps/Diego 2010 census/scince_2010/shps/yuc/"
mapdir5="ArcGIS Explorer/My Basemaps/eleccion_2010/eleccion_2010/todo/yuc/cartografiadigital_ife/"


setwd(paste0(rootdir,"GitHub/U-of-Florida-Yucatan-Project"))

loc <- read.csv("catalogo de municipios.csv")
head(loc)

colnames(loc) <- c("CVE_ENT", "NAME_ENT", "CVE_MUN", "NAME_MUN")

mex2 <- st_read(paste0(rootdir,mapdir,"MEX_adm2.shp"),quiet=T,stringsAsFactors = F)
st_bbox(mex2[1,])#get bbox of first line

# urbana<- st_read(paste0(rootdir,mapdir2,"yuc_ageb_urbana.shp"),quiet=T)#encuesta intercensal
# rural<-st_read(paste0(rootdir,mapdir2,"yuc_ageb_rural.shp"),quiet=T,stringsAsFactors = F)#Encuesta intercensal
# encuesta<-read.csv(paste0(rootdir,mapdir2,"catalogos/localidades urbanas y rurales amanzanadas.csv"),
#                    header=T)
# # head(rural)
# 
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

urbana2<-st_read(paste0(rootdir,mapdir3,"localidad250_a.shp"),quiet=T,stringsAsFactors = F)#Encuesta intercensal
# length(unique(urbana2$nombre))#340 loclaities
#has good locality names
addresses2 <- read.csv("Linux Data/addresses2_mod2.csv",header=T,stringsAsFactors = F)
# urbana_mun <- read.csv("Linux Data/localidades urbanas y rurales amanzanadas_mod.csv",header=T)
# urbana_mun <- subset(urbana_mun,urbana_mun$ENTIDAD==31);length(unique(urbana_mun$NOMBRE.DE.LOCALIDAD))
#611 unique localities
# 
# urbana3 <- st_read(paste0(rootdir,mapdir4,"yuc_loc_urb.shp"),quiet=T,stringsAsFactors = F);View(urbana3)
# urbana4 <- st_read(paste0(rootdir,mapdir5,"yuc_limite_localidad.shp"),quiet=T,stringsAsFactors = F);View(urbana4)
# length(unique(urbana4$NOMBRE))
#389 localities
# View(urbana_mun)

#############################################
# Encoding(urbana2$nombre)
# tail(urbana2$nombre)
urbana2$nombre<- iconv(urbana2$nombre,from="UTF-8",to="ASCII//TRANSLIT")
# urbana4$nombre<- iconv(urbana4$nombre,from="UTF-8",to="ASCII//TRANSLIT")

# # tail(urbana2)
# urbana2$nombre[338]==addresses2$LOCALIDAD[2451]
# class(urbana2[,7])
# 'TEYA' %in% urbana2$nombre

#CHecking if all localities from addresses2 are in urbana2
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
# missing_urbana4=NULL
for (i in seq(addresses2$LOCALIDAD)){
  if (addresses2$LOCALIDAD[i] %in% urbana2$nombre==T ){#if the address is not in urbana2 (==FALSE)
    urbana2_hits[i]=addresses2$LOCALIDAD[i]
  }
  else if (addresses2$LOCALIDAD[i] %in% urbana2$nombre){
    next
  }
}

head(which(!is.na(urbana2_hits)))
View(which(!is.na(urbana2_hits)))#2715 urban hits ie the file names match up, out of 3291!
urbana2_hits <- (which(!is.na(urbana2_hits)))

#new modified addresses with hits from Linux
addresses2_hits <-read.csv("Linux Data/addresses2_mod2_hits.csv",header=T,stringsAsFactors = F);View(addresses2_hits)

##TO COUNT HOW MANY additional SCHOOL HITS FROM urbana2 by LOCALITY NAME
#for each i, change the addresses2_hits for that value i position (urbana2_hits[i]) to "success"
#for i in addresses2_hits, if if that ith position is equal to one 
# of the position numbers in addresses2_hits, AND
#urbana2_hits$HITS contains ZERO_RESULTS, and , then replace that value with "locality matches!"
for (i in seq(length(addresses2_hits$HITS))){
  if ((i %in% urbana2_hits && grepl("ZERO_RESULTS",addresses2_hits$HITS[i]))==T){
    addresses2_hits$HITS[i] <- "locality matches!"
  }
}
View(addresses2_hits)#works!
length(which(addresses2_hits$HITS=="locality matches!"))#998 additional hits!

which(!is.na(missing_urbana2))
unique(missing_urbana2)#count unique localities
length(unique(missing_urbana2))/length(unique(addresses2$LOCALIDAD))#43% missing localities! 254/587
#587 unique localities in total, so we have 254 or about 57% of them
length(unique(missing_urbana4))/length(unique(addresses2$LOCALIDAD))#46% missing localities! 268/587


missing_urbana2[2065:2095]
#######
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

