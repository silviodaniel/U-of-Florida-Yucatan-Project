setwd("C:/Users/Silvio/Documents/R/Yucatan-Project")
library(cluster)
library(dplyr)
py<-read.table(file="C:/Users/Silvio/Downloads/pop-yucatan/population-yucatan.txt",header=TRUE)
ly<-read.table(file="C:/Users/Silvio/Downloads/pop-yucatan/locations-yucatan.txt",header=TRUE)
ly$hid=ly$id
ly$workid=ly$id
py=left_join(py,ly[,c("hid","x","y")],by="hid")#adding 2 columns in py (after workid) with house x y coordinates
py=left_join(py,ly[,c("workid","x","y")],by="workid")
colnames(py)<-c("pid","hid","age","sex","hh_serial","pernum","workid","x1","y1","x2","y2")
head(py)
#Haversine 
hav=function(x1,y1,x2,y2){
  return(12742 *asin(sqrt((sin(pi/180*(y2-y1)/2))^2
                          +cos(pi/180*y1)*cos(pi/180*y2)*(sin(pi/180*(x2-x1)/2))^2)))
}

hav(-89.6848792716 ,20.6464119922,-89.0323486633, 20.8012998636)#should return 70.01462
##FAST WAY TO CALCULATE ALL VALUES
start = Sys.time()
hav_vec=hav(py[,8],py[,9],py[,10],py[,11])
py$distance=hav_vec
print(Sys.time()-start)
head(py)

#Adding people_log data set with pid, workid, and logdist 
py$logdist=py$distance#adds new column called logdist set equal to distance 
py$logdist[which(py$logdist==0)]=5/1000##Changing the zeroes in the data to 5/1000 because cant take log(0)
log(0.005)
py$logdist=log10(py$logdist)#Taking log of the data for logdist
head(py)
#py$distance=apply_dists# adds the haversine function to the distance column in population
#people_mat = py[,c('pid','workid','distance')]##shortening the py matrix to 3 columns
#head(people_mat)
people_log = py[,c('pid','workid','logdist')]##shortening the py matrix to 3 columns
tail(people_log)

#Merging people_log and loc_labels to compare logdist and house type based on id
tail(ly)
loc_labels = ly[,c('id','type')]##**shortening the matrix of locations to these 2 columns
tail(loc_labels)
names(loc_labels)[1] = 'workid' #names the column workid
#movement_by_type = merge(people_mat, loc_labels)#**merging the shortened py and ly with "workid" , organized by number in dataset
movement_by_log = merge(people_log, loc_labels)

#ANALYSIS
##WORK HISTOGRAM
hist(movement_by_log$logdist[movement_by_log$type=='work'],xlab="Distance Traveled (km)", main="Average Daily Work Transit in Yucatan")
#SCHOOL HISTOGRAM
hist(movement_by_log$logdist[movement_by_log$type=='work'],xlab="Distance Traveled (km)", main="Average Daily School Transit in Yucatan")
#Means of distance travelled for house, school, then work (not log)
mean(movement_by_type$distance[movement_by_type$type=='house'])
mean(movement_by_type$distance[movement_by_type$type=='school'])
mean(movement_by_type$distance[movement_by_type$type=='work'])
#Medians
median(movement_by_type$distance[movement_by_type$type=='school'])#Mean surprisingly close to median
median(movement_by_type$distance[movement_by_type$type=='work'])#Mean far from median 
#Number of people working from home
table(movement_by_type$distance[movement_by_type$type=='house'])

