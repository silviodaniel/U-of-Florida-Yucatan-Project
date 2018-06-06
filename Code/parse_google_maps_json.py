#!/usr/bin/python
import json
from sys import exit

###Full file
json_data = open('json_sorted_full.mod').read()
data = json.loads(json_data)

##
#Add to code: if the line 'formatted_address' does not contain 'Yuc., Mexico', print hit or line number and go to next iteration
#also, want to check if every hit has formatted_address... they DO!
# if "blah" not in somestring: 
#       continue

phrase = ["a bunch of stuff Yuc., Mexico", "nope"]
testing = ['Yuc., Mexico','YUC, Mexico','yup']


for i in range(len(phrase)):
    if any(x in phrase[i] for x in testing):#checks if any of strings in testing is in phrase, and in this case, yes!
        print("YEAH BUDDY")
        print i
    
phrase1 = "a bunch of stuff DLZ, Mexico"

if any(x in phrase1 for x in testing)==False:#good, so will not print outside of Yucatan if at least 1 is a match
    print("outside of Yucatan")#
      


a = ['a', 'b', 'c']
str = "a123"
any(x in str for x in testing)

###
ints = [8, 23, 45, 12, 78]
for idx, val in enumerate(ints):
    print(idx)

########################################

##Addresses not in Yucatan
ctr = 0
miss_ctr=0
yuc = ['Yuc., Mexico','YUC, Mexico','Yucatan, Mexico','Merida, Mexico','Tizimin, Mexico']
for hit in data['all_results']:
    if hit['status']== 'OK' and \
       any(x in hit['results'][0]['formatted_address'] for x in yuc)==False:#for every string in yuc, check if contained in the phrase, or address
          # tmp = 
           print hit['results'][0]['formatted_address']
          # print hit
           miss_ctr+=1#THERE ARE 64 ADDRESSES WHERE IT IS NOT IN YUCATAN
    ctr+=1
    
    
#Now, excluding those lat/lng not in Yucatan
ctr = 0
hit_ctr_new=0
yuc = ['Yuc., Mexico','YUC, Mexico','Yucatan, Mexico','Merida, Mexico','Tizimin, Mexico']
API_coords=[]
for hit in data['all_results']:
    if hit['status']== 'OK' and \
       any(x in hit['results'][0]['formatted_address'] for x in yuc)==True:#for every string in yuc, check if contained in the phrase, or address
          # tmp = 
          # print hit['results'][0]['formatted_address']
           tmp = hit['results'][0]['geometry']['location']['lat'],',', hit['results'][0]['geometry']['location']['lng']
           API_coords.append(tmp)
          # print hit
           hit_ctr_new+=1#THERE ARE 64 ADDRESSES WHERE IT IS NOT IN YUCATAN
    else:
        tmp1 = hit['status']
        API_coords.append(tmp1)
    ctr+=1
    
import pandas as pd
from sklearn import datasets
df1 = pd.DataFrame(API_coords)
df1.head(10)

 #see if yoou can just modify the csv here in this step
 
#Just need to find a way to remove these 64 at first so that they output no results
    
    else if hit['status']== 'OK' and \
            ('Yuc., Mexico' not in hit['results'][0]['formatted_address'])==False:

ctr = 0
for hit in data['all_results']:
    if hit['status'] 
        
    else if hit['status'] == 'OK':
        print hit['results'][0]['geometry']['location']['lat'],',', hit['results'][0]['geometry']['location']['lng']
    else:
        print hit['status']

##ORIGINAL
ctr = 0
hit_ctr = 0
for hit in data['all_results']:
    if hit['status'] == 'OK':
        print hit['results'][0]['geometry']['location']['lat'],',', hit['results'][0]['geometry']['location']['lng']
        hit_ctr +=1
    else:
        print hit['status']
    ctr+=1
        

###############################################        
#### Second half file
json_data = open('json_sorted_second_half.mod').read()
data1 = json.loads(json_data)

ctr = 0
for hit in data1['all_results']:
    if hit['status'] == 'OK':
        print hit['results'][0]['geometry']['location']['lat'],',', hit['results'][0]['geometry']['location']['lng']
    else:
        print hit['status']
#    ctr += 1
#    if ctr > 10:
#        exit()
##########################################################################################################

######
#Example
phrase = "totally"
'total' in phrase
('total' not in phrase)==False
#Returns true! So this works
testing = ['tota','not','yup']

#Example
import collections
thing= [0,1,2,3,4]
thing1 = collections.defaultdict(thing)
thing[3].append(2.5)
thing

#Example
res=[1,2,3,4]
import os
import csv
cwd=os.getcwd()
csv.file="/home/silvio/work/Yucatan/Data/"#figure out how to get the path working
res=[1,2,3,4]

with open(csv.file,"w") as output:
    writer= csv.writer(output,lineterminator='\n')
    for val in res:
        writer.writerow([val])
