# -*- coding: utf-8 -*-
"""
Created on Tue Dec  3 17:01:18 2019

@author: riselin
Translation of R xwing scripts
"""

#First filter by formats as that is ALWAYS necessary. Then continue with dates to limit by waves. 
#Later, in a separate file, by size, list completion, incomplete cut (allow to set true/false)
#use the reference number (e.g. 70 for Z95 Scum) to link pilots with their ships. list(ref_pilots.values())[XXX][4] is the ship number
# format of ref_pilots: name, xws-name, points, ini, ship ref, pilot ref, faction, image1, image2
# format of ref_upgrades: name, upgrade type, xws-name, points(most expensive if variable), ref number, image1, image2


import pandas as pd
import os
import json
import requests
import datetime
import shutil
import glob

#set working directory. Different for mac and pc?
os.chdir(r"C:\Users\iseli\OneDrive\Desktop\Xwing\analysis")

ttype = int(input("Select the type of tournament (2 for Nationals, 3 for Trials, 5 for SOS, 6 for Worlds): "))
ftype = int(input("Select the format (1 for Extended, 34 for Hyperspace): "))
ddate = datetime.datetime.strptime(input("Enter the cutoff date in YYYY-MM-DD: "), "%Y-%m-%d")

def get_all_events():
    api_url = 'https://listfortress.com/api/v1/tournaments/'
    response = requests.get(api_url)
    parsed = json.loads(response.content.decode('utf-8'))
    return parsed

tournaments_parsed = get_all_events()
tournament_ids = pd.DataFrame(tournaments_parsed)

def tournament_date(tournamentdata):
    #picks and returns the date of the id entry
    tourndate = pd.to_datetime(tournamentdata, format = "%Y-%m-%d")
    return tourndate

tournament_ids["date"] = tournament_date(tournament_ids["date"])

def filter_tournament_format(tournamentdata, formatid, tournamentid):
    #5 Format IDs: 1 is Extended, 2 is Second Edition, 3 is Custom, 4 is Other, 34 is Hyperspace. Those are all.
    #Tournament Type ID 1-8: 1 is Store Event, 2 is National Championship, 3 is Hyperspace Trial, 4 is Hyperspace Cup, 
    #5 is System Open, 6 is World Championship, 7 is Casual Event, 8 is Other #
    filtereddata = tournamentdata[(tournamentdata["format_id"]== formatid) & (tournamentdata["tournament_type_id"] == tournamentid)]
    return filtereddata

tournament_ids = filter_tournament_format(tournament_ids, formatid = ftype, tournamentid = ttype)
#tournament_ids = tournament_ids.drop(columns = ['created_at', 'updated_at', 'version_id']) #irrelevant unless I continue otherwise with this info
#down to 8 columns: date, format_id, id, location, name, state, tournament_type_id
# date, format_id and tournament_id are critical. The others are nice to have and thus carried on

#sort by certain date: wave 5 started at 20190913
tournament_ids = tournament_ids.loc[tournament_ids["date"] > pd.Timestamp(ddate)]

tournament_ids["id"]

#download Tournament data based on these IDs, and merge into a single json file
def get_tournament(tournament_id):
    api_url = 'https://listfortress.com/api/v1/tournaments/' + str(tournament_id)
    response = requests.get(api_url)
    parsed = json.loads(response.content.decode('utf-8'))
    print(tournament_id)
    return parsed

if os.path.isdir('TempData'):
    shutil.rmtree('TempData')
os.mkdir('TempData')
for ids in tournament_ids["id"]:
    parsedjson = get_tournament(ids)
    with open('TempData/' + str(ids) + '.json', 'w') as file:
        json.dump(parsedjson, file)
        
result = []
for f in glob.glob("TempData/*.json"):
    with open(f, "r") as infile:
        result.append(json.load(infile))

os.mkdir('pythonTournamentOutput')
with open("pythonTournamentOutput/merged_file.json", "w") as outfile:
    json.dump(result, outfile)
shutil.rmtree('TempData')


def switch_ftype(argument):
    switcher = {
        1: "Extended",
        2: "Second Edition",
        3: "Custom",
        4: "Other",
        34: "Hyperspace",
    }
    return switcher.get(argument, "Invalid format")

def switch_ttype(argument):
    switcher = {
        1: "Store Event",
        2: "National Championship",
        3: "Hyperspace Trial",
        4: "Hyperspace Cup",
        5: "System Open",
        6: "World Championship",
        7: "Casual Event",
        8: "Other",
    }
    return switcher.get(argument, "Invalid tournament type")


dateTimeObj = datetime.datetime.now()
ddatestr = str(ddate.year) + '/' + str(ddate.month) + '/' + str(ddate.day)
timestamp = str(dateTimeObj.year) + '/' + str(dateTimeObj.month) + '/' + str(dateTimeObj.day)
ReadMe = open("pythonTournamentOutput/README.txt","w")
ReadMe.write("Used setting in the current merged_file.json \n")
ReadMe.writelines("Tournament Type: " + str(ttype)  + " ("+ switch_ttype(ttype) + ")" + "\n")
ReadMe.writelines("Format: "+ str(ftype) + " ("+ switch_ftype(ftype) + ")" + "\n")
ReadMe.writelines("Cutoff Date: " + ddatestr + "\n")
ReadMe.write("Timestamp: " + timestamp)
ReadMe.close()

"""
def find_dictentry(lst, key, value):
    for i, dic in enumerate(lst):
        if dic[key] == value:
            return i
    return None

def identify_tournament_type(idnumber):
    # legacy; used to identify what the different tournament type id number mean
    listindex = find_dictentry(tournaments_parsed, "tournament_type_id", idnumber)
    return tournaments_parsed[listindex]
"""