setwd("C:/Users/Silvio/Documents/R/Yucatan-Project/Pictures/")

#Making maps based on households, then households, schools, and workplaces
d = read.table("../pop-yucatan/locations-yucatan.txt",header=T)
ws = read.table("../pop-yucatan/schools_and_workplaces.out", header=T)
png("yucatan_households.png", width=2000, height=1500,res=150)
plot(d$x, d$y, pch='.', xlim=c(-90.4,-87.53), ylim=c(19.65,21.65), xlab='longitude', ylab='latitude', main='Households', col='#00000033')
dev.off()
pdf("yucatan_all_locations.p", width=2000, height=1500)#res=150
plot(d$x, d$y, pch='.', xlim=c(-90.4,-87.53), ylim=c(19.65,21.65), xlab='longitude', ylab='latitude', main='All locations', col='#00000033')
points(ws$x[ws$type=='w'], ws$y[ws$type=='w'], col='#0000ff', pch=20)
points(ws$x[ws$type=='s'], ws$y[ws$type=='s'], col='#ff0000', pch=20)
legend('bottomright', inset=0.02,c('Households','Workplaces','Schools'),fill=c('black','blue','red'))
dev.off()
##
#Need df with: household locations, lat and long, distance traveled to school

#Run haversine code before running this!
head(movement_by_type)
###############################################################################
#HEATMAP CODE BELOW
library(ggplot2)

#Creating data for heatmap, only including those going to school, 
#with the long and lat coordinates for axes
new_loc_labels= ly[,c('id','type','x','y')]##**shortening the matrix of locations to these 2 columns
names(new_loc_labels)[1] = 'workid' #names the column workid
new_movement_by_type=merge(people_mat,new_loc_labels)
#head(new_movement_by_type)
heatmap_data=subset(new_movement_by_type,type=="school")#subsetting just the schools, instead
# of all places  to make the school heat map
length(heatmap_data$workid)#540818 going to school
# head(heatmap_data)

#GGPLOT2 HEATMAP of distance traveled by household
ggplot(heatmap_data,aes(x,y))+geom_point(aes(colour=distance))+
  scale_color_gradient(low="yellow",high="red")
#ALSO TRY MAKING THE POINTS SMALLER
ggplot(heatmap_data,aes(x,y))+geom_point(aes(colour=distance),size=2)+
  scale_color_gradientn(colours=rainbow(15))

#####################################################################
#Second heat map
install.packages("cartography")
library(cartography)
library(sf)
install.packages("sf")

plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)



#######################################################################################
#Findings

length((which(new_movement_by_type$distance==0)))#620859, distance of zero, zero distance 0 distance
length(which(new_movement_by_type$distance==0))/length(py$pid)#34.1% traveling zero distance total!

##
min(d$x)#CONSIDER CHANGING BOUNDARIES
max(d$x)
min(d$y)#19.72
max#21.62
min(d$x_ctr)#-90.40
max(d$x_ctr)#-87.53
min(d$y_ctr)#19.72
max(d$y_ctr)#21.62

#Testing
#Dark spots show up as small rectangles/sqares, pixels?
png("test.png",width=2000,height=1500,res=150)
plot(d$x_ctr,d$y_ctr,pch='.',xlim=c(-90.41,-87.50),ylim=c(19.65,21.65),col='#00000033')
dev.off()

head(d)

png("testing2.png", width=2000, height=1500,res=150)
plot(d$x, d$y, pch='.', xlim=c(-90.4,-87.53), ylim=c(19.65,21.65), xlab='longitude', ylab='latitude', main='All locations', col='#00000033')
#points(ws$x[ws$type=='w'], ws$y[ws$type=='w'], col='#0000ff', pch=20)
points(ws$x[ws$type=='s'], ws$y[ws$type=='s'], col='#ff0000', pch=20)
legend('bottomright', inset=0.02,c('Households','Schools'),fill=c('black','red'))
dev.off()

head(ws)

#Heatmap of distance traveled
#Aim: Get data frame with coordinates and distance traveled
#Then, figure out how to plot points with color gradient scale, with the condition 
#of higher distance indicating darker color




