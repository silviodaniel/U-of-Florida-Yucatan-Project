rootdir="C:/Users/Silvio/Documents/"
mapdir="ArcGIS Explorer/My Basemaps/MEX_adm/"
mapdir2="ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/"
mapdir3="ArcGIS Explorer/My Basemaps/INEGI mapa/conjunto_de_datos/"

loc <- read.csv("catalogo de municipios.csv")
loc

colnames(loc) <- c("CVE_ENT", "NAME_ENT", "CVE_MUN", "NAME_MUN")

head(rural)

rural$CVE_ENT <- as.numeric(rural$CVE_ENT)
rural$CVE_MUN <- as.numeric(rural$CVE_MUN)

head(rural)
tmp <- left_join(rural, loc)
head(tmp)
?left_join.sf

tmp1 <- rural$geometry[1]
tmp1
?`cartography-package`
getBorders(tmp1)

runif

urbana2<-st_read(paste0(rootdir,mapdir3,"localidad250_a.shp"),quiet=T,stringsAsFactors = F)#Encuesta intercensal
#has good locality names

##############################################
mex2_mun=read.csv("MEX_adm2-mod.csv",header=T,stringsAsFactors = F);head(mex2_mun)
attach(mex2_mun);names(mex2_mun)

gsub("[[:punct:]]","",NAME_2)
gsub("[[:punct:]]","",VARNAME_2)

VARNAME_2
install.packages("stringr")
library(stringr)

str_replace_all(NAME_2, "[^[:alnum:]]", "")
str_replace_all(VARNAME_2, "[^[:alnum:]]", "")


count=NULL
for (i in seq(1:length(mex2_mun$OBJECTID))){
  if(is.na(VARNAME_2[i])){
    next
    # count[i]=OBJECTID[i]
  }
  else if(NAME_2[i]==VARNAME_2[i]){
    #return(OBJECTID[i])
    count[i]=OBJECTID[i]
  }
  else{
    next
    # print("success")
  }
}

plzwork=read.table("MEX_adm2-mod.txt",header=T,stringsAsFactors = F);head(plzwork)


