#!/usr/bin/python
import json
from sys import exit

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
###Full file
json_data = open('json_sorted_full.mod').read()
data = json.loads(json_data)

ctr = 0
for hit in data['all_results']:
    if hit['status'] == 'OK':
        print hit['results'][0]['geometry']['location']['lat'],',', hit['results'][0]['geometry']['location']['lng']
    else:
        print hit['status']
        
