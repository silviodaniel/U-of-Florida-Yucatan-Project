rootdir="C:/Users/Silvio/Documents/GitHub/U-of-Florida-Yucatan-Project/"
setwd(paste0(rootdir,"Linux Data/pop-yucatan/"))
# setwd("C:/Users/Silvio/Documents/R/Yucatan-Project/Pictures")
# install.packages("cluster")
library(cluster)
# install.packages("dplyr")
library(dplyr)
#py<-read.table(file="C:/Users/Silvio/Documents/R/Yucatan-Project/pop-yucatan/population-yucatan.txt",header=TRUE)
# py=read.table(file="pop-yucatan/population-yucatan_old.txt",header = T)
# py<- read.table(file="population-yucatan.txt",header = T)
# ly<-read.table(file="locations-yucatan.txt",header=TRUE)
py<- read.table("population-yucatan-silvio.txt",header = T)
ly<-read.table("locations-yucatan-silvio.txt",header=T)
ly$hid=ly$id
ly$workid=ly$id
py=left_join(py,ly[,c("hid","x","y")],by="hid")#adding 2 columns in py (after workid) with house x y coordinates
py=left_join(py,ly[,c("workid","x","y")],by="workid")
colnames(py)<-c("pid","hid","age","sex","hh_serial","pernum","workid","x1","y1","x2","y2")
#x1 and y1 refer to the home coordinates, and x2 y2 refer to the work or school
# py1<-py
head(py)

# ly$Urban=x#Create urban/rural column
# colnames(ly)<-c(" id",  "type"," x","y" ,"x_ctr", "y_ctr" ,"Urban/Rural")

#Haversine 
#pi=3.141593
rm(pi)
hav=function(x1,y1,x2,y2){
  #convert degrees to radians by multiplying by pi, dividing by 180
  #y's should be latitudes and x's should be longitudes
  return(12742 *asin(sqrt((sin(pi/180*(y2-y1)/2))^2
                          +cos(pi/180*y1)*cos(pi/180*y2)*(sin(pi/180*(x2-x1)/2))^2)))
}
12742/2

hav(-89.6848792716 ,20.6464119922,-89.0323486633, 20.8012998636)#=70.01462

####Creating distance column with haversine
start = Sys.time()
hav_vec=hav(py[,8],py[,9],py[,10],py[,11])##ANOTHER FAST WAY TO CALCULATE
py$distance=hav_vec
print(Sys.time()-start)
#head(py)
  
#Setting up histogram
hist(py[1:1819497,12], xlab="Daily Distance Traveled (km)", main="Population Distribution of Daily Travel in Yucatan")
# py$logdist=py$distance#adds new column called logdist set equal to distance
# py$logdist[which(py$logdist==0)]=5/1000##Changing the zeroes in the data to 5/1000 because cant take log(0)
# py$logdist=log10(py$logdist)#Taking log of the data for logdist
# ##py$distance=apply_dists# adds the haversine function to the distance column in population
people_mat = py[,c('pid','workid','distance')]##shortening the py matrix to 3 columns
# head(people_mat)
# people_log = py[,c('pid','workid','logdist')]##shortening the py matrix to 3 columns
# tail(people_log)
# head(py)
# head(ly)

# tail(ly)
loc_labels = ly[,c('id','type')]##**shortening the matrix of locations to these 2 columns
names(loc_labels)[1] = 'workid' #names the column workid
movement_by_type = merge(people_mat, loc_labels)#**merging the shortened py and ly with "workid" , organized by number in dataset
#movement by type has houses, schools, work, organized by work id and pid; histogram the distances separete as types (school or work)
# movement_by_log = merge(people_log, loc_labels)
# head(movement_by_type)
# head(movement_by_log)
# tail(movement_by_type)
#View(movement_by_type)

#??in merge, are we merging people_mt and loc_labels with "workid" and organizing the distances by their workid?
table(movement_by_type$distance[movement_by_type$type=='house'])#gives number of people working from home (distance 0)
#which is 620859

#WORK HISTOGRAM 
hist(movement_by_type$distance[movement_by_type$type=='work'],xlab="Distance Traveled (km)", main="Average Daily Work Transit in Yucatan")

######SCHOOL HISTOGRAM
students=py[py$age>4 & py$age<18,]
schools = ly[ly$type=='school',]

png("Pictures/new_schools_hist.png")
hist(students$distance,xlab="Distance Traveled (km)",
     ylab="Student count (thousands)",
     main="Average Daily School Transit in Yucatan")
dev.off()
# hist(movement_by_type$distance[movement_by_type$type=='school'],xlab="Distance Traveled (km)",
#      ylab="Student count (thousands)",
#      main="Average Daily School Transit in Yucatan")

#####Students mean, median, max
mean(students$distance)#mean distance traveled is 1.50
max(students$distance)
median(students$distance)
# mean(movement_by_type$distance[movement_by_type$type=='house'])
# mean(movement_by_type$distance[movement_by_type$type=='school'])#
# max(movement_by_type$distance[movement_by_type$type=='school'])#'
#reproduced old mean of 45.7km
#7.88 km old data, normal pi value
#1.13 km new data (mean)
#.705km old data (median)
#0.411 NEW DATA (median)

mean(movement_by_type$distance[movement_by_type$type=='work'])#
#reproduced old mean of 34.4 km
#13.9 km old data
#13.86 new data
# table(movement_by_type$distance[movement_by_type$type=='house'])
# table(movement_by_type$distance[movement_by_type$type=='work'])
median(movement_by_type$distance[movement_by_type$type=='school'])#
#reproduced old median of 46.0km
#.705km old data
#0.411 NEW DATA
median(movement_by_type$distance[movement_by_type$type=='work'])#
max(movement_by_type$distance[movement_by_type$type=='work'])#
#Reproduced old median of 11.06km
#2.78 km old data
#2.74 km new data

# outliers= students[c(407612,408036,408340,408518),]#students >250 km
# length(which(students$distance>50))
outliers2= subset(students,students$distance>20);View(outliers2)
length(outliers2$pid)/length(students$pid)#4733 outliers

# hist(movement_by_log$logdist[movement_by_log$type=='school'],
#      xlab="Distance Traveled (log(km))", 
#      main="Average Daily School Transit in Yucatan")

# set.seed(12345)
# x=rnorm(1000)
# hist.data=hist(x,plot=F)
# 
# hist.data$counts=log10(hist.data$counts)
# dev.new(width=4, height=4)
# hist(x)
# dev.new(width=4, height=4)
# plot(hist.data, ylab='log10(Frequency)')
# hist(x)
# 
# plot(density(movement_by_log$logdist[movement_by_log$type=='work'], log="y"), main="Histogram of Distance to Work")
# 
# #Testing Log transform histogram
# count=table(round(rnorm(10000)*2))#data
# head(count)
# #plot
# plot(log(count),type="h",yaxt="n", xlab="log(count)", ylab="position")
# # axis labels
# yAxis = c(0,1,10,100,1000)
# # draw axis labels
# axis(2, at=log(yAxis),labels=yAxis, las=2)
# 
# plot(movement_by_log$logdist[movement_by_log$type=='work']$count, log="y",type='h',lwd=10,lend=2)
# 
# ############
# #Number of schools, workplaces
# head(ly[,2])
# homes=which(ly[,2]=="house")#from ly, so unique
# length(homes)#376400
# school=which(ly[,2]=="school")
# work=which(ly[,2]=="work")
# length(school)#3402 schools
# length(work)#95560 workplaces
# length(school)+length(work)#98962
# #Number of children going to school supposedly, or # students enrolled in schools
# #use movement by type, which has repeated workplaces,and can show ALL people going to a work
# school_children=which(movement_by_type[,4]=="school")
# workers=which(movement_by_type[,4]=="work")
# homestayers=which(movement_by_type[,4]=="house")
# length(school_children)#43574 
# length(workers)#1155064
# length(homestayers)#620859
# 
# #Descriptive stuff
# max(movement_by_type$distance)#max distance travelled is 37.7 km
# 
# #NOtes#################################################################
# #where houses end in location : position 376400
# 
# length(students)
