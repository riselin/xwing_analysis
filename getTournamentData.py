# -*- coding: utf-8 -*-
"""
Created on Thu Dec 12 12:54:24 2019

@author: riselin
"""

import os
import json
import pandas as pd
#import requests
#import datetime
#import shutil
#import glob


os.chdir("//pasteur/SysBC-Home/riselin/Documents/polybox/privat/xwing/Turniere_2.0/Wave5/")

class Tournamentdata(object):
    num_of_tournaments = 0
    
    def __init__(self):
        self
        
        Tournamentdata.num_of_tournaments += 1



def clean_pilot_xws(pilot_xws):
    if pilot_xws == "niennumb-t70xwing":
        pilot_xws = "niennunb"
    elif pilot_xws == "oddballarc170":
        pilot_xws = "oddball-arc170starfighter"
    elif pilot_xws == "ricolie-nabooroyaln1starfighter":
        pilot_xws = "ricolie"
    elif pilot_xws == "anakinskywalkerywing":
        pilot_xws = "anakinskywalker-btlbywing"
    return pilot_xws

def clean_upgrade_xws(upgrade_xws):
    if upgrade_xws == "hardpointcannon":
        upgrade_xws = "skip"
    elif upgrade_xws == "hardpointmissile":
        upgrade_xws = "skip"
    elif upgrade_xws == "hardpointtorpedo":
        upgrade_xws = "skip"
    elif upgrade_xws == "reysmilleniumfalcon":
        upgrade_xws = "reysmillenniumfalcon"
    elif upgrade_xws == "rey":
        upgrade_xws = "rey-gunner"
    elif upgrade_xws == "chewbaccaresistance":
        upgrade_xws = "chewbacca-crew-swz19"
    elif upgrade_xws == "leiaorganaresistance":
        upgrade_xws = "leiaorgana-resistance"
    return upgrade_xws



with open("merged_file.json", "r") as read_file:
    tourn_rawdata = json.load(read_file)
    manual_curation = [5,8,9,47]
    for entry in manual_curation:
        del tourn_rawdata[entry]

#eval turns the last bit into a dict, and that allows json.load to unpack it.
eval(tourn_rawdata[0]["participants"][0]["list_json"])

#tourn_rawdata[0]["participants"][0]["list_json"] contains the list.
parsed_playerdata = json.loads(tourn_rawdata[0]["participants"][0]["list_json"])
#loading that anew provides a dict with 5 entries, one of them the faction, another one the pilots, and one the points
#size of len(parsed_squad["pilots"]) gives the number of ships in a list

#tourn_rawdata[0]["round"] is often empty. If it is empty, take the swiss first ranked as maximum.
if not tourn_rawdata[0]["rounds"]:
    print("whoops, no rounds entered")

#assign points one level up
tourn_rawdata[0]["participants"][0]["list_points"] = json.loads(tourn_rawdata[0]["participants"][0]["list_json"])["points"]
d_raw = pd.DataFrame()
tourn_id = []
player = []
for tournament in range(0, len(tourn_rawdata)):
    tourn_id.append(tourn_rawdata[tournament]["id"])
    for players in range(0, len((tourn_rawdata[tournament]["participants"]))):
        player.append(tourn_rawdata[tournament]["participants"][players]["name"])





parsed_squad = json.loads(parsed_playerdata["pilots"][0])
datatest = Tournamentdata(tourn_rawdata[0])

def extract_values(obj, key):
    """Pull all values of specified key from nested JSON."""
    arr = []

    def extract(obj, arr, key):
        """Recursively search for values of key in JSON tree."""
        if isinstance(obj, dict):
            for k, v in obj.items():
                if isinstance(v, (dict, list)):
                    extract(v, arr, key)
                elif k == key:
                    arr.append(v)
        elif isinstance(obj, list):
            for item in obj:
                extract(item, arr, key)
        return arr

    results = extract(obj, arr, key)
    return results

len(extract_values(tourn_rawdata, "name")) #works for several keys on both dict levels, but not on the list as it's text


def flatten_json(nested_json):
    """
        Flatten json object with nested keys into a single level.
        Args:
            nested_json: A nested json object.
        Returns:
            The flattened json object if successful, None otherwise.
    """
    out = {}

    def flatten(x, name=''):
        if type(x) is dict:
            for a in x:
                flatten(x[a], name + a + '_')
        elif type(x) is list:
            i = 0
            for a in x:
                flatten(a, name + str(i) + '_')
                i += 1
        else:
            out[name[:-1]] = x

    flatten(nested_json)
    return out

flat1 = flatten_json(parsed_playerdata)

dat_participant = tourn_rawdata[0]["participants"][0]
flat2 = flatten_json(dat_participant)