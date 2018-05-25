rootdir="C:/Users/Silvio/Documents/"
mapdir="ArcGIS Explorer/My Basemaps/MEX_adm/"
mapdir2="ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/"
mapdir3="ArcGIS Explorer/My Basemaps/INEGI mapa/conjunto_de_datos/"

loc <- read.csv("catalogo de municipios.csv")
head(loc)

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
attach(mex2_mun);names(mex2_mun)

#CHecking which names don't match up btw the 2 columns
count=NULL
for (i in seq(1:length(mex2_mun$OBJECTID))){
  if(is.na(mex2_mun$VARNAME_2[i])){
    next
    # count[i]=OBJECTID[i]
  }
  else if(mex2_mun$NAME_2[i]!=mex2_mun$VARNAME_2[i]){
    #return(OBJECTID[i])
    count[i]=mex2_mun$OBJECTID[i]
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

