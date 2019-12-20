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

class TournamentList:
    num_of_tournaments = 0
    tournament = None
    
    def __init__(self):
        self.tournament = self.Tournaments()
    

    class TournamentInstance:
        tournDate = 0
    
        for ids in TournamentList.tournament:
            tournid = TournamentList.tournament['id']
            tournstring = str(TournamentList.tournament['date']) + str(TournamentList.tournament['state']) + str(TournamentList.tournament['country'])
            
        def __init__(self):
            TournamentList.num_of_tournaments += 1
        
        @classmethod
        def report_date(cls, newDate):
            #will change a variable of TournamentInstance
            cls.tournDate = newDate
            return cls.tournDate
        
        @staticmethod
        def notaccessinginstancehere(variable):
            pass
        
        class ParticipantList:
            pass

class TournamentInstance:
        tournDate = datetime.date(2019,1,1)
    
#        for ids in TournamentList.tournament:
#            tournid = TournamentList.tournament['id']
#            tournstring = str(TournamentList.tournament['date']) + str(TournamentList.tournament['state']) + str(TournamentList.tournament['country'])
#            
        def __init__(self, data):
            self.tournament = data
            self.country = data['country']
            self.date = data['date']
            self.state = data['state']
            TournamentList.num_of_tournaments += 1
            
        def clean_data(self,remove_key):
            for key in remove_key:
                del self.data[key]
        
        @classmethod
        def set_date(cls, newDate):
            #will change a variable of TournamentInstance
            cls.tournDate = newDate
            return cls.tournDate
        
        @staticmethod
        def notaccessinginstancehere(variable):
            pass
        
        class ParticipantList:
            pass

#turn all lists into dicts, using code from https://thispointer.com/python-how-to-convert-a-list-to-dictionary/
#tourn_rawdata = { i : tourn_rawdata[i] for i in range(0, len(tourn_rawdata) ) }

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
#    manual_curation = [5,7,8,47]
#    for entry in manual_curation:
#        del tourn_rawdata[entry]

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

class Struct(object):
    """https://stackoverflow.com/questions/1305532/convert-nested-python-dict-to-object?page=1&tab=votes#tab-top"""
    def __init__(self, data):
        for name, value in data.iteritems():
            setattr(self, name, self._wrap(value))

    def _wrap(self, value):
        if isinstance(value, (tuple, list, set, frozenset)): 
            return type(value)([self._wrap(v) for v in value])
        else:
            return Struct(value) if isinstance(value, dict) else value
        
with open("merged_file.json", "r") as read_file:
    """https://stackoverflow.com/questions/6578986/how-to-convert-json-data-into-a-python-object/15882054#15882054"""
    x = json.loads(read_file.read(), object_hook=lambda d: namedtuple('X', d.keys())(*d.values()))

"""Alternative:
    https://stackoverflow.com/questions/1305532/convert-nested-python-dict-to-object?page=1&tab=votes#tab-top"""
class obj(object):
    def __init__(self, dict_):
        self.__dict__.update(dict_)

def dict2obj(d):
    return json.loads(json.dumps(d), object_hook=obj)