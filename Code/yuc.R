library(cartography)
rootdir="C:/Users/Silvio/Documents/"
mapdir="ArcGIS Explorer/My Basemaps/MEX_adm/"
setwd(paste0(rootdir,"GitHub/U-of-Florida-Yucatan-Project"))
# install.packages("cluster")
library(cluster)
# install.packages("dplyr")
library(dplyr)

#reading files necessary
py=read.table(paste0(rootdir,"R/Yucatan-Project/pop-yucatan/population-yucatan.txt"),header = T)
ly<-read.table(paste0(rootdir,"R/Yucatan-Project/pop-yucatan/locations-yucatan.txt"),header=T)
ly$hid=ly$id
ly$workid=ly$id
py=left_join(py,ly[,c("hid","x","y")],by="hid")#adding 2 columns in py (after workid) with house x y coordinates
py=left_join(py,ly[,c("workid","x","y")],by="workid")
colnames(py)<-c("pid","hid","age","sex","hh_serial","pernum","workid","x1","y1","x2","y2")
head(py)
##
# urbana <- st_read("C:/Users/Silvio/Documents/ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/yuc_ageb_urbana.shp"
#                , quiet = TRUE)#encuesta intercensal
# rural<- st_read("C:/Users/Silvio/Documents/ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/yuc_ageb_rural.shp"
#                 , quiet = TRUE)#Encuesta intercensal
mex0=st_read(paste0(rootdir,mapdir,"MEX_adm0.shp"),
             quiet=T)#Diva-GIS
mex1=st_read(paste0(rootdir,mapdir,"MEX_adm1.shp"),
             quiet=T)#Diva-GIS

# class(urbana)
# plot(st_geometry(urbana))#from encuesta file

##
#Map of Yucatan
par(mar=c(2.1,2.1,2.1,2.1))#margins
plot(st_geometry(mex0))#plots all of Mexico
plot(st_geometry(mex0),xlim=c(-90.75,-87.25),ylim=c(19.5,21.75),bg="lightblue",
     col="gray")#takes all Mexico plot, plot just Yucatan
plot(st_geometry(mex1[31,]),add=T,col="#99FF99")#adds green color to 
#Yucatan boundary

# plot(st_geometry(rural),lwd=.5,add=T,col="#FFFFFF")
# plot(st_geometry(urbana),lwd=.5,add=T,col="#000000")
# plot(st_geometry(urbana),lwd=.5,add=T,col=4)
# plot(st_geometry(urbana),lwd=.5,add=T,col="#FF8C00")

axis(1)#adds in long and lat axes
axis(2)
# grid()
# abline(h=seq(19,22,0.1))
# abline(v=seq(-91,-87,0.1))

#Next steps: create heat map of distance children traveling to school by locality
#pair localiites with distance, color them darker gradient depending on value

#Just add below code to above plot of Yuc (empty green area, mex0 and mex1)

students=py[py$age>4 & py$age<18,]
points(students$x1,students$y1,pch='.',col='red')
#
schools = ly[ly$type=='school',]
points(schools$x,schools$y,pch='.',col='blue')
#
head(students)
length(students$pid)

plotCircle <- function(x, y, r) {
  angles <- seq(0,2*pi,length.out=360)#between 0 and 2pi
  lines(r*cos(angles)+x,r*sin(angles)+y)#start at x and y and add
}#This output is Cartesian not lat long, so have to fix this
#must get lines to output the coordinates of x and y in lat/long degrees
plotCircle(-89,20,0.5)

pi*1:2
##
#Example
plot(1:100,type='n')
lines(c(0,0,20,0),c(0,20,20,0))#Plot triangle
##

 
# head(students)
# head(schools)




  