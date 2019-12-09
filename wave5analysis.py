# -*- coding: utf-8 -*-
"""
Created on Tue Dec  3 17:01:18 2019

@author: riselin
Translation of R xwing scripts
"""
import pandas as pd
import os
import json
import requests
import datetime

#set working directory. Different for mac and pc?
os.chdir("//pasteur/SysBC-Home/riselin/Documents/polybox/privat/xwing/Turniere_2.0")
#Functions

#Constants
# factioncolors

#read database, requires pandas
database = pd.read_csv("./database5.csv", sep = ';') #works as intended
#alternatively, use xhud.sirjorj.com for all pilots and upgrades!
def get_ref_data():
    pilots_url = 'http://xhud.sirjorj.com/xwing.cgi/pilots2?format=json'
    pilots_response = requests.get(pilots_url)

    upgrades_url = 'http://xhud.sirjorj.com/xwing.cgi/upgrades2?format=json'
    upgrades_response = requests.get(upgrades_url)

    parsed_pilots = json.loads(pilots_response.content.decode('utf-8'))
    parsed_upgrades = json.loads(upgrades_response.content.decode('utf-8'))

    ships = {}
    pilots = {}
    upgrades = {}
    pilot_id = 0
    upgrade_id = 0

    for parsed_pilot in parsed_pilots:
        pilot_name = parsed_pilot['name']
        pilot_xws = parsed_pilot['xws']
        pilot_cost = parsed_pilot['cost']
        pilot_id = pilot_id + 1
        if pilot_cost == "???":
            continue
        pilot_initiative = parsed_pilot['initiative']
        ship = parsed_pilot['ship']

        pilot_faction = parsed_pilot['faction']

        pilot_art = parsed_pilot['cardart']
        pilot_card = parsed_pilot['cardimg']

        if ship not in ships:
            ships[ship] = (ship, len(ships) + 1)
        pilots[pilot_xws] = (pilot_name, pilot_xws, pilot_cost, pilot_initiative, 
              ships[ship][1], pilot_id, pilot_faction, pilot_art, pilot_card)

    ref_upgrade_types = []; #list to store upgrade types; will be filled dynamically with each new encountered upgrade

    for parsed_upgrade in parsed_upgrades:
        upgrade_type = parsed_upgrade['side'][0]['type']
        if upgrade_type not in ref_upgrade_types:
            ref_upgrade_types.append(upgrade_type)
        upgrade_type_id = ref_upgrade_types.index(upgrade_type) + 1
        upgrade_name = parsed_upgrade['name']
        upgrade_xws = parsed_upgrade['xws']
        upgrade_id = upgrade_id + 1
        upgrade_art = parsed_upgrade['side'][0]['cardart']
        upgrade_card = parsed_upgrade['side'][0]['cardimg']
        if 'cost' in parsed_upgrade: #always take the highest cost for each upgrade: highest ini, agility, or size
            if parsed_upgrade['cost']['variable'] == "None":
                upgrade_cost = parsed_upgrade['cost']['value']
            elif parsed_upgrade['cost']['variable'] == "initiative":
                upgrade_cost = parsed_upgrade['cost']['6']
            elif parsed_upgrade['cost']['variable'] == "Agility":
                upgrade_cost = parsed_upgrade['cost']['agi3']
            elif parsed_upgrade['cost']['variable'] == "BaseSize":
                upgrade_cost = parsed_upgrade['cost']['large']
            else:
                upgrade_cost = 200
        else:
            upgrade_cost = 0

        upgrades[upgrade_xws] = (upgrade_name, upgrade_type_id, upgrade_xws, upgrade_cost, upgrade_id, upgrade_art, upgrade_card)
    all_ref_pilots = []
    for pilot in pilots.items():
        pilot = pilot[1]
        all_ref_pilots.append(pilot)

    all_ref_ships = []
    for ship in ships.items():
        ship = ship[1]
        all_ref_ships.append(ship)

    upgrade_types_with_id = []
    for upgrade_type in ref_upgrade_types:
        upgrade_types_with_id.append([ref_upgrade_types.index(upgrade_type) + 1, upgrade_type])

    all_ref_upgrades = []
    for upgrade in upgrades.items():
        upgrade = upgrade[1]
        all_ref_upgrades.append(upgrade)

    return ships, pilots, upgrades

ref_ships, ref_pilots, ref_upgrades = get_ref_data()

#use the reference number (e.g. 70 for Z95 Scum) to link pilots with their ships. list(ref_pilots.values())[XXX][4] is the ship number
# format of ref_pilots: name, xws-name, points, ini, ship ref, pilot ref, faction, image1, image2
# format of ref_upgrades: name, upgrade type, xws-name, points(most expensive if variable), ref number, image1, image2

#get Tournament data
def get_tournament(tournament_id):
    api_url = 'https://listfortress.com/api/v1/tournaments/' + str(tournament_id)
    response = requests.get(api_url)
    parsed = json.loads(response.content.decode('utf-8'))
    print(tournament_id)
    return parsed

#use separate file with IDs -> collect manually? Or allow dynamic, e.g. by boxes/categories? First manually
#First filter by format as that is ALWAYS necessary. Then continue with dates to limit by waves. 
#Finally by size, list completion, incomplete cut (allow to set true/false)
#Format IDs: 1 is Extended, 2 is Second Edition, 3 is Custom, 4 is Other, 34 is Hyperspace. Those are all.
#Tournament Type ID: 1 is Store Event, 2 is National Championship, 3 is Hyperspace Trial, 4 is Hyperspace Cup, 
    # 5 is System Open, 6 is World Championship, 7 is Casual Event, 8 is Other

def get_all_events():
    api_url = 'https://listfortress.com/api/v1/tournaments/'

    response = requests.get(api_url)
    parsed = json.loads(response.content.decode('utf-8'))
    return parsed

tournaments_parsed = get_all_events()

print(datetime.datetime.strptime(tournaments_parsed[2]["date"],"%Y-%m-%d"))

def tournament_date(tournamentdata):
    #picks and returns the date of the id entry
    tourndate = datetime.datetime.strptime(tournamentdata["date"], "%Y-%m-%d") 
    return tourndate

def filter_tournament_format(tournamentdata, formatid):
    pddata = pd.DataFrame(tournamentdata)
    filtereddata = pddata[(pddata["format_id"]== formatid)]
    return filtereddata

tournaments_formatfiltered = filter_tournament_format(tournaments_parsed, 34)

#next, filter by date


def find_dictentry(lst, key, value):
    for i, dic in enumerate(lst):
        if dic[key] == value:
            return i
    return None

df = pd.DataFrame(tournaments_parsed)
set(df.iloc[:, 8]) #col 8 for tournament type id, 3 for format id, 5 for tournament id
def identify_tournament_type(idnumber):
    # legacy; used to identify what the different tournament type id number mean
    listindex = find_dictentry(tournaments_parsed, "tournament_type_id", idnumber)
    return tournaments_parsed[listindex]

