setwd("C:/Users/Silvio/Documents/R/")
library(cluster)
library(dplyr)
py<-read.table(file="../../Downloads/pop-yucatan/population-yucatan.txt",header=TRUE)
ly<-read.table(file="../../Downloads/pop-yucatan/locations-yucatan.txt",header=TRUE)

head(py)
head(ly)
ly$hid=ly$id
ly$workid=ly$id
head(ly)

py=left_join(py,ly[,c("hid","x","y")],by="hid")#adding 2 columns in py (after workid) with house x y coordinates
py=left_join(py,ly[,c("workid","x","y")],by="workid")
colnames(py)<-c("pid","hid","age","sex","hh_serial","pernum","workid","x1","y1","x2","y2")
head(py)
hav=function(x1,y1,x2,y2){
  #fixed error of converting to radians wrong
  return(12742 *asin(sqrt((sin(pi/180*(y2-y1)/2))^2
                          +cos(pi/180*y1)*cos(pi/180*y2)*(sin(pi/180*(x2-x1)/2))^2)))
}

hav(-89.6848792716 ,20.6464119922,-89.0323486633, 20.8012998636)
hav(-89.12265, 21.23725, -89.55344, 20.92981)
hav(py[1:10,8:11],3,4,5)
py[1:10,8:11]

start = Sys.time()
hav_vec=hav(py[,8],py[,9],py[,10],py[,11])##ANOTHER FAST WAY TO CALCULATE
py$distance=hav_vec
print(Sys.time()-start)

head(ly)
head(py)

#HISTOGRAM
hist(py[1:1819497,12], xlab="Daily Distance Traveled (km)", main="Population Distribution of Daily Travel in Yucatan")
head(py)

py$distance=apply_dists# adds the haversine function to the distance column in population
people_mat = py[,c('pid','workid','distance')]##shortening the py matrix to 3 columns
head(people_mat)

tail(ly)
loc_labels = ly[,c('id','type')]##shortening the matrix of locations to these 2 columns
tail(loc_labels)
names(loc_labels)[1] = 'workid' #names the column workid
movement_by_type = merge(people_mat, loc_labels)#merging the shortened py and ly with "workid" , organized by number in dataset
head(movement_by_type)
tail(movement_by_type)
#??in merge, are we merging people_mt and loc_labels with "workid" and organizing the distances by their workid?
hist(movement_by_type$distance[movement_by_type$type=='house'],xlab="Distance Traveled (km)", main="Average Daily Work Transit in Yucatan")
table(movement_by_type$distance[movement_by_type$type=='house'])#gives number of people working from home (distance 0)
hist(movement_by_type$distance[movement_by_type$type=='work'],xlab="Distance Traveled (km)", main="Average Daily Work Transit in Yucatan")
#WORK HISTOGRAM (above)
hist(movement_by_type$distance[movement_by_type$type=='school'],xlab="Distance Traveled (km)", main="Average Daily School Transit in Yucatan")
#SCHOOL HISTOGRAM
mean(movement_by_type$distance[movement_by_type$type=='house'])
mean(movement_by_type$distance[movement_by_type$type=='school'])
mean(movement_by_type$distance[movement_by_type$type=='work'])
table(movement_by_type$distance[movement_by_type$type=='house'])
table(movement_by_type$distance[movement_by_type$type=='work'])
median(movement_by_type$distance[movement_by_type$type=='school'])
median(movement_by_type$distance[movement_by_type$type=='work'])
