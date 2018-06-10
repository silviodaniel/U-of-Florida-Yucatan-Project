rm(list=ls())
library(cartography)
rootdir="C:/Users/Silvio/Documents/GitHub/"
rootdir1="C:/Users/Silvio/Documents/"
rootdir2="C:/Users/Silvio/Documents/ArcGIS Explorer/My Basemaps/"
mapdir1="U-of-Florida-Yucatan-Project/Shapefiles/"
mapdir3="ArcGIS Explorer/My Basemaps/MEX_adm/"
mapdir2="encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/"
setwd(paste0(rootdir,"U-of-Florida-Yucatan-Project"))
# install.packages("cluster")
library(cluster)
# install.packages("dplyr")
library(dplyr)

#reading files necessary
py=read.table(paste0(rootdir1,"R/Yucatan-Project/pop-yucatan/population-yucatan.txt"),header = T)
ly<-read.table(paste0(rootdir1,"R/Yucatan-Project/pop-yucatan/locations-yucatan.txt"),header=T)
ly$hid=ly$id
ly$workid=ly$id
py=left_join(py,ly[,c("hid","x","y")],by="hid")#adding 2 columns in py (after workid) with house x y coordinates
py=left_join(py,ly[,c("workid","x","y")],by="workid")
colnames(py)<-c("pid","hid","age","sex","hh_serial","pernum","workid","x1","y1","x2","y2")
students=py[py$age>4 & py$age<18,]
schools = ly[ly$type=='school',]

head(py)
##
# urbana<- st_read(paste0(rootdir2,mapdir2,"yuc_ageb_urbana.shp"),quiet=T)#encuesta intercensal
# rural<-st_read(paste0(rootdir2,mapdir2,"yuc_ageb_rural.shp"),quiet=T,stringsAsFactors = F)#Encuesta intercensal
encuesta<-read.csv(paste0(rootdir,mapdir2,"catalogos/localidades urbanas y rurales amanzanadas.csv"),
                   header=T)

urbana2 <- st_read("Shapefiles/INEGI mapa/localidad250_a.shp",quiet=T,stringsAsFactors = F)
urbana2$nombre<- iconv(urbana2$nombre,from="UTF-8",to="ASCII//TRANSLIT")
urbana2 <- st_transform(urbana2,crs=4326)##Converting coordinates from NAD83 to WGS84
urbana2$nombre[162] <- "CHOLUL MERIDA"; urbana2$nombre[1] <- "TEMOZON ABALA"
urbana2 <- urbana2[-c(43,61,102,153,242,295),]
rural<-st_read(paste0(rootdir,mapdir1,"yuc_ageb_rural.shp"),quiet=T,stringsAsFactors = F)#Encuesta intercensal
rural$CVE_MUN <- as.numeric(rural$CVE_MUN)
rural$CVE_ENT <- as.numeric(rural$CVE_ENT)

# length(unique(urbana$CVE_LOC))#only 20 unique localities
# View(encuesta)
# View(urbana)
# head(urbana)
# tail(urbana)

mex0=st_read(paste0(rootdir,mapdir1,"MEX_adm0.shp"),
             quiet=T)#Diva-GIS
mex1=st_read(paste0(rootdir,mapdir1,"MEX_adm1.shp"),
             quiet=T)#Diva-GIS
# mex0 <- st_read(paste0(rootdir2,"gadm36_MEX_shp/gadm36_MEX_0.shp"),quiet=T,stringsAsFactors = F)
# mex1 <- st_read(paste0(rootdir2,"gadm36_MEX_shp/gadm36_MEX_1.shp"),quiet=T,stringsAsFactors = F)
# mex2.1 <- st_read(paste0(rootdir2,"gadm36_MEX_shp/gadm36_MEX_2.shp"),quiet=T,stringsAsFactors = F)
# mex2 <- subset(mex2.1, mex2.1$GID_1=="MEX.31_1")

# class(urbana)
# plot(st_geometry(urbana))#from encuesta file

rural$CVE_AGEB
##
#Map 1
png("Pictures/py_students_schools_enlarged_schools.png", width=2400, height=1600, res=240)
par(mar=c(2.1,2.1,2.1,2.1))#margins
plot(st_geometry(mex0))#plots all of Mexico
plot(st_geometry(mex0),xlim=c(-90.75,-87.25),ylim=c(19.5,21.75),bg="lightblue",
     col="gray")#takes all Mexico plot, plot just Yucatan
# plot(st_geometry(mex2),add=T,col="#99FF99")
# plot(st_geometry(mex2.1),add=T,col="#99FF99")
plot(st_geometry(rural),add=T,col="#99FF99")
plot(st_geometry(urbana2),add=T,col="white")

points(students$x1,students$y1,pch='.',col='red')
#
# points(schools$x,schools$y,pch='.',col='blue')
points(schools$x,schools$y,pch= 20, col='blue',lwd=0.5)#schools enlarged (pch=20)
# points(schools2_y,schools2_x,pch= 20, col='blue',lwd=0.5)#schools enlarged (pch=20)
dev.off()
#are rural and urbana municipalities of different codes??
# plot(mex2$geometry[88],add=T,col="yellow")#Teya mun
# plot(mex2$geometry[46],add=T,col="yellow")#Merida mun

# plot(rural$geometry[198],add=T,col="yellow")#Teya mun for rural
# plot(rural$geometry[235:239],add=T,col="yellow")#Merida mun for rural
# plot(urbana2$geometry[236],add=T,col="blue")#Teya 
# plot(urbana2$geometry[162],add=T,col="blue")#Cholul urban 
#only issue is municipalities with rural will not include the urban areas (red areas) enclosed 
#within them
#those are like holes, so when we random sample coordinates for those ~200 schools using 
#municipality shp files, it will not include the urban areas (red areas)

# plot(rural$geometry[198],add=T,col="yellow")#Teya mun 
# plot(urbana2$geometry[14],add=T,col="blue")#Cholul rural
# plot(st_as_sf(urbana)["CVE_LOC"])
#plot(st_as_sf(rural)[""])
#Map 2
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

#Just add below code to above plot of Yuc (empty green area, mex0 and mex1)
#####py data#####

#
head(students)
length(students$pid)
length(schools$x)#3402 schools!!
##################
points


########################################################################################
#Plot one polygon
plot(urbana$geometry[1])
plot(rural$geometry[1])
points(spsample(urbana$geometry[1],n=10,"regular"),pch=3)

#Haversine function
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
plotCircle <- function(x_deg, y_deg, r) {#Radius is in kilometers!
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
}
#This output is Cartesian not lat long, so have to fix this
#must get lines to output the coordinates of x and y in lat/long degrees

# plotCircle(-89.6,21,15)
# head(schools)

library(sp);library(maptools)
data(wrld_simpl)
s <- matrix(as.numeric(NA), nrow(wrld_simpl), 2)
for (i in seq_len(nrow(s))) s[i,] <- 
  as.vector(coordinates(spsample(wrld_simpl[i,], type = "random", n = 1, iter = 10)))
class(wrld_simpl)

res <- SpatialPointsDataFrame(s, as.data.frame(wrld_simpl),
                              proj4string = CRS(proj4string(wrld_simpl)))

##
#Example
library(sp)
# Definition of the CRS
poly.crs <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Definition of 3 Polygons
poly.a <- Polygon((matrix(c(-4.653724,1.2210259,-3.803160, 1.3799638, -3.245480, 0.1665987,-4.666098, 0.1097523, -4.653724, 1.2210259), nrow=5, ncol=2, byrow=T)))
poly.b <- Polygon((matrix(c(-5.820343, 2.675320,-5.427519, 3.062975,-4.701119, 2.819967,-4.555540, 1.855489, -5.050758, 1.388308, -5.783673, 1.572882,-5.820343, 2.675320), nrow=7, ncol=2, byrow=T)))
poly.c <- Polygon((matrix(c(-3.758639, 2.873654, -3.273958, 3.417311, -2.213099, 2.935320, -1.972031, 1.945992, -3.033510, 1.279312, -3.709359, 1.844633, -3.758639, 2.873654), nrow=7, ncol=2,     byrow=T))) 

# Making a SpatialPolygons
polys.a = Polygons(list(poly.a), "pa")
polys.b = Polygons(list(poly.b), "pb")
polys.c = Polygons(list(poly.c), "pc")
Spolys = SpatialPolygons(list(polys.a,polys.b,polys.c), 1:3,  proj4string=poly.crs)

# Making a SpatialPolygonsDataFrame
data.Spolys<- (data.frame(MatchID=c("abcd","efgh","ijkl"), row.names=row.names(Spolys)))
Poly <- SpatialPolygonsDataFrame(Spolys, data.Spolys, match.ID = TRUE)


## Data frame that should be converted in a SpatialPointsDataFrame thanks to the variable "MatchID"
df <- data.frame(
  MatchID=c("abcd","abcd","abcd","efgh","efgh","ijkl"),
  V1 = 1:30,
  V2 = "a"
)


### Preparing the SpatialPointsDataFrame
spdf <- matrix(as.numeric(NA), nlevels(Poly$MatchID), 1)
spdf <- as.list(spdf)

### Sample the coordinate, match it with data in spdf. It creates a list fore each factor of the MatchID
### sample(spsample()) fix the size of the sample

for (i in seq(Poly$MatchID))
  spdf= spsample(Poly[order(Poly$MatchID)==i,], n = 100, "stratified")
  sample(spdf,table(df$MatchID)[[i]])


  spdf[i] <- SpatialPointsDataFrame(
    sample(spsample(Poly[order(Poly$MatchID)==i,], n = 100, "stratified"),table(df$MatchID)[[i]]),  ### table(df$MatchID)[[i]] is the size of the sample and match the sum of factors in df 
    df[df$MatchID==dimnames(table(df$MatchID))[[1]][i],], ##  dimnames(table(df$MatchID))[[1]][i] ### match the value of the selected "factor" to select the rows of the data
    proj4string=poly.crs, 
    match.ID=TRUE)

## Merging together the list to make a SpatialDataFrame
spdf <- do.call("rbind", spdf)

## Plot 
plot(Poly[,])
plot(spdf, add=TRUE, col=spdf$MatchID)
######################################################################
#example
# plot(1:100,type='n')
# lines(c(0,0,20,0),c(0,20,20,0))#Plot triangle
# ##

 
# head(students)
# head(schools)




  
#=======
# rm(list=ls())
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
# py=read.table(file="../population-yucatan.txt",header = T)
# ly<-read.table(file="../locations-yucatan.txt",header=TRUE)
# ly$hid=ly$id
# ly$workid=ly$id
# py=left_join(py,ly[,c("hid","x","y")],by="hid")#adding 2 columns in py (after workid) with house x y coordinates
# py=left_join(py,ly[,c("workid","x","y")],by="workid")
# colnames(py)<-c("pid","hid","age","sex","hh_serial","pernum","workid","x1","y1","x2","y2")
# head(py)
# ##
# # urbana <- st_read("C:/Users/Silvio/Documents/ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/yuc_ageb_urbana.shp"
# #                , quiet = TRUE)#encuesta intercensal
# # rural<- st_read("C:/Users/Silvio/Documents/ArcGIS Explorer/My Basemaps/encuesta_intercensal_2015 Diego/encuesta_intercensal_2015/shps/yuc/yuc_ageb_rural.shp"
# #                 , quiet = TRUE)#Encuesta intercensal
# mex0=st_read(paste0(rootdir, mapdir, "MEX_adm0.shp"), quiet=T)#Diva-GIS
# mex1=st_read(paste0(rootdir, mapdir, "MEX_adm1.shp"), quiet=T)#Diva-GIS

# class(urbana)
# plot(st_geometry(urbana))#from encuesta file

####################################################################################
#Catchment Area Map of Yucatan
png("Pictures/newshps_newschools_white_large_schools.png", width=2400, height=1600, res=240)
#pdf("catchment_map.pdf", width=12, height=8)

par(mar=c(2.1,2.1,2.1,2.1))#margins
plot(st_geometry(mex0))#plots all of Mexico
plot(st_geometry(mex0),xlim=c(-90.75,-87.25),ylim=c(19.5,21.75),bg="lightblue",
     col="gray")#takes all Mexico plot, plot just Yucatan
plot(st_geometry(rural),add=T,col="white")
#plot(st_geometry(rural),add=T,col="#99FF99")
plot(st_geometry(urbana2),add=T,col="white")
# plot(st_geometry(mex1[31,]),add=T,col="#99FF99")#adds green color to 
#Yucatan boundary

# plot(st_geometry(rural),lwd=.5,add=T,col="#FFFFFF")
# plot(st_geometry(urbana),lwd=.5,add=T,col="#000000")
# plot(st_geometry(urbana),lwd=.5,add=T,col=4)
# plot(st_geometry(urbana),lwd=.5,add=T,col="#FF8C00")

##GRID##
# axis(1)#adds in long and lat axes
# axis(2)
# grid()
# abline(h=seq(19,22,0.1))
# abline(v=seq(-91,-87,0.1))

# students=py[py$age>4 & py$age<18,]
points(students$x1,students$y1,pch='.',col='red')
#########ly data#####################################
# schools = ly[ly$type=='school',]
# points(schools$x,schools$y,pch='.',col='blue')
#OR....
################
schools2_x <- as.numeric(addresses2_hits$LAT_HITS)
schools2_y <- as.numeric(addresses2_hits$LNG_HITS)
# points(schools2_y,schools2_x,pch= '.', col='blue')#Reversed by accident!
points(schools2_y,schools2_x,pch= 20, col='blue',lwd=0.5)#schools enlarged (pch=20)

#Plotting circles around all schools (new)
for (coordinate in 1:length(schools2_x)){
  plotCircle(schools2_y[coordinate],schools2_x[coordinate],10)#15 km radius of circle
}
######################

#Haversine function manipulation to plot circles correctly
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

#Function to plot circle using haversine, will look elliptical but that's bc of curvature
plotCircle <- function(x_deg, y_deg, r) {
  x = deg2rad(x_deg)
  y = deg2rad(y_deg)
  halflats = y + km_to_lat_rad(r)*sin(pi*90:270/180)
  eastlons = x + 2*asin(sqrt((hav(r/earth_r) - hav(halflats - y))/(cos(y)*cos(halflats))))
  westlons = x - 2*asin(sqrt((hav(r/earth_r) - hav(halflats - y))/(cos(y)*cos(halflats))))
  
  lats = c(halflats, rev(halflats))
  lons = c(westlons, rev(eastlons))
  lines(rad2deg(lons), rad2deg(lats))
  }#browser()

plotCircle_blue <- function(x_deg, y_deg, r) {
    x = deg2rad(x_deg)
    y = deg2rad(y_deg)
    halflats = y + km_to_lat_rad(r)*sin(pi*90:270/180)
    eastlons = x + 2*asin(sqrt((hav(r/earth_r) - hav(halflats - y))/(cos(y)*cos(halflats))))
    westlons = x - 2*asin(sqrt((hav(r/earth_r) - hav(halflats - y))/(cos(y)*cos(halflats))))
    lats = c(halflats, rev(halflats))
    lons = c(westlons, rev(eastlons))
    lines(rad2deg(lons), rad2deg(lats),col="blue")
  
  
  #angles <- seq(0,2*pi,length.out=360)#between 0 and 2pi
  #lines(r*cos(angles)+x,r*sin(angles)+y)#start at x and y and add
}#This output is Cartesian not lat long, so have to fix this
#must get lines to output the coordinates of x and y in lat/long degrees

##Plotting circles around all schools (old)!
# length(schools$x)
# for (coordinate in 1:length(schools$x)){
#   plotCircle(schools$x[coordinate],schools$y[coordinate],15)#15 km radius of circle
# }



# lines(c(-90,-89.8),c(19.5,19.7),col="blue")
###

##
#Example
# plot(1:100,type='n')
# lines(c(0,0,20,0),c(0,20,20,0))#Plot triangle
# ##

#These 20 coordinates place schools roughly (eyeballing) to cover those children not currently
#within 15km of a school
plotCircle_blue(-87.975,	20.875,	15)
plotCircle_blue(-87.7,      20.85,	15)
plotCircle_blue(-87.6,      21.05,	15)
plotCircle_blue(-88.04,     21.4,	15)
plotCircle_blue(-87.78,     21.33,	15)
plotCircle_blue(-87.87,     21.14,	15)
plotCircle_blue(-88.53,     21.32,	15)
plotCircle_blue(-90.225,	20.63,	15)
plotCircle_blue(-88.68,	    20.23,	15)
plotCircle_blue(-89.445,	20,	    15)
plotCircle_blue(-89.7,      20.22,	15)
plotCircle_blue(-88.7,      20.72,	15)
plotCircle_blue(-88.49,     21.21 ,	15)
plotCircle_blue(-89.345,	19.865,	15)
plotCircle_blue(-88.67,	    20.44,	15)
plotCircle_blue(-89.975,	21.17,	15)
plotCircle_blue(-88.285,	20.35,	15)
plotCircle_blue(-88.03,     20.465,	15)
plotCircle_blue(-87.66,     21.22,	15)
plotCircle_blue(-87.685,	21.515,	15)

dev.off()
#dev.copy2pdf()

# head(students)
# head(schools)

#pdf()
