rm(list=ls())
library(cartography)

#rootdir = "C:/Users/Silvio/Documents/"
#mapdir  = paste0(rootdir, "ArcGIS Explorer/My Basemaps/MEX_adm/")
rootdir = "./"
mapdir  = paste0(rootdir, "../")
#setwd(paste(rootdir,"R/Yucatan-Project"))
# install.packages("cluster")
#library(cluster)
# install.packages("dplyr")
#library(dplyr)

#reading files necessary
py=read.table(file="../population-yucatan.txt",header = T)
ly<-read.table(file="../locations-yucatan.txt",header=TRUE)
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
mex0=st_read(paste0(rootdir, mapdir, "MEX_adm0.shp"), quiet=T)#Diva-GIS
mex1=st_read(paste0(rootdir, mapdir, "MEX_adm1.shp"), quiet=T)#Diva-GIS

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
grid()
abline(h=seq(19,22,0.1))
abline(v=seq(-91,-87,0.1))

#Next steps: create heat map of distance children traveling to school by locality
#pair localiites with distance, color them darker gradient depending on value

#Just add below code to above plot of Yuc (empty green area, mex0 and mex1)
schools = ly[ly$type=='school',]
points(schools$x,schools$y,pch='.',col='blue')
#
students=py[py$age>4 & py$age<18,]
points(students$x1,students$y1,pch='.',col='red')

head(students)
length(students$pid)

earth_r = 6371
rm(pi)
gcd=function(x1,y1,x2,y2){
  #convert degrees to radians by multiplying by pi, dividing by 180
  #y's should be latitudes and x's should be longitudes
  return(2 * earth_r *asin(sqrt((sin(pi*(y2-y1)/(2*180)))^2
                          +cos(pi*y1/180)*cos(pi*y2/180)*(sin(pi*(x2-x1)/(2*180)))^2)))
}

km_to_lat_rad = function(km) { return(km/earth_r) }
hav = function(theta) { return((1 - cos(theta))/2); }
deg2rad <- function(deg) return(deg*pi/180)
rad2deg <- function(rad) return(rad*180/pi)

plotCircle <- function(x_deg, y_deg, r) {
  x = deg2rad(x_deg)
  y = deg2rad(y_deg)
  halflats = y + km_to_lat_rad(r)*sin(pi*90:270/180)
  eastlons = x + 2*asin(sqrt((hav(r/earth_r) - hav(halflats - y))/(cos(y)*cos(halflats))))
  westlons = x - 2*asin(sqrt((hav(r/earth_r) - hav(halflats - y))/(cos(y)*cos(halflats))))
  
  lats = c(halflats, rev(halflats))
  lons = c(westlons, rev(eastlons))
  lines(rad2deg(lons), rad2deg(lats))
  #browser()

  #angles <- seq(0,2*pi,length.out=360)#between 0 and 2pi
  #lines(r*cos(angles)+x,r*sin(angles)+y)#start at x and y and add
}#This output is Cartesian not lat long, so have to fix this
#must get lines to output the coordinates of x and y in lat/long degrees

##
#Example
plot(1:100,type='n')
lines(c(0,0,20,0),c(0,20,20,0))#Plot triangle
##

plotCircle(-89,20,0.5)
 
# head(students)
# head(schools)




  
