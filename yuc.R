library(cartography)
##
urbana <- st_read("C:/Users/Silvio/Documents/ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/yuc_ageb_urbana.shp"
               , quiet = TRUE)#encuesta intercensal
rural<- st_read("C:/Users/Silvio/Documents/ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/yuc_ageb_rural.shp"
                , quiet = TRUE)#Encuesta intercensal
mex0=st_read("C:/Users/Silvio/Documents/ArcGIS Explorer/My Basemaps/MEX_adm/MEX_adm0.shp",
             quiet=T)#Diva-GIS
mex1=st_read("C:/Users/Silvio/Documents/ArcGIS Explorer/My Basemaps/MEX_adm/MEX_adm1.shp",
             quiet=T)#Diva-GIS

# class(urbana)
plot(st_geometry(urbana))

##

class(rural)

par(mar=c(2.1,2.1,2.1,2.1))


plot(st_geometry(mex0))#plots all of Mexico
#Map of Yucatan
plot(st_geometry(mex0),xlim=c(-91,-87),ylim=c(19.5,21.75),bg="lightblue",
     col="gray")#takes all Mexico plot, plot just Yucatan
# plot(st_geometry(mex1[31,]),add=T,col="#99FF99")#adds green color to Yucatan boundary
plot(st_geometry(rural),lwd=.5,add=T,col="#FFFFFF")
plot(st_geometry(urbana),lwd=.5,add=T,col="#000000")
# plot(st_geometry(urbana),lwd=.5,add=T,col=4)
# plot(st_geometry(urbana),lwd=.5,add=T,col="#FF8C00")

#Next steps: create heat map of distance children traveling to school by locality
#pair localiites with distance, color them darker gradient depending on value
#Research!: How to get data spdf??



