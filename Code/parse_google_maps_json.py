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

phrase = "totally"
'total' in phrase
('total' not in phrase)==False
#Returns true! So this works

ctr = 0
miss_ctr=0
for hit in data['all_results']:
    if hit['status']== 'OK' and \
       ('Yuc., Mexico' not in hit['results'][0]['formatted_address'])==True:
           print hit['results'][0]['formatted_address']
           print hit
           miss_ctr+=1#THERE ARE 89 ADDRESSES WHERE IT IS NOT IN YUCATAN
    ctr+=1
    
#Just need to find a way to remove these 89 at first so that they output no results
    
    else if hit['status']== 'OK' and \
            ('Yuc., Mexico' not in hit['results'][0]['formatted_address'])==False:

ctr = 0
for hit in data['all_results']:
    if hit['status'] 
        
    else if hit['status'] == 'OK':
        print hit['results'][0]['geometry']['location']['lat'],',', hit['results'][0]['geometry']['location']['lng']
    else:
        print hit['status']

##
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
