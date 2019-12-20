# -*- coding: utf-8 -*-
"""
Created on Thu Dec 12 12:54:24 2019

@author: riselin
"""
#from __future__ import print_function

import os
import json
#import pandas as pd
#import requests
import datetime
#import shutil
#import glob
from collections import namedtuple
from nested_lookup import nested_lookup


os.chdir("//pasteur/SysBC-Home/riselin/Documents/polybox/privat/xwing/Turniere_2.0/Wave5/")




class TournamentList:
    num_of_tournaments = 0
    tournament = None
    
    def __init__(self, data):
        self.tournament = data
    


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



with open("merged_file.json", "r") as read_file:
    tourn_rawdata = json.load(read_file)
#    manual_curation = [5,7,8,47]
#    for entry in manual_curation:
#        del tourn_rawdata[entry]

#or read out the tournament IDs and use them with zip to combine two lists into one dict!

def list2dict(data, identifier = 'id', adjust = False):
    used_ids = nested_lookup(identifier, data)
    if adjust:
        used_ids = [ids for ids in used_ids if ids < 1411] #This has to be adjusted manually
    used_ids = ["id" + str(s) for s in used_ids]
    data = dict(zip(used_ids, data))
    return data

def list2dictRounds(data):
    used_ids =  [str(nested_lookup('roundtype_id',data)[i]) + '_' + 
             str(nested_lookup('round_number', data)[i]) 
             for i in range(0, len(data))]
    data = dict(zip(used_ids, data))
    return data

tourn_rawdata = list2dict(tourn_rawdata, adjust = True)

for key in list(tourn_rawdata.keys()):
    tourn_rawdata[key]['participants'] = list2dict(tourn_rawdata[key]['participants'])
    tourn_rawdata[key]['rounds'] = list2dictRounds(tourn_rawdata[key]['rounds'])


removekey = ['participants', 'rounds'] #to simplify by removing levels
simpledata = tourn_rawdata[0].copy()
for key in removekey:
    del simpledata[key]

dictlist = []
for key, value in tourn_rawdata[0].items():
    temp = [key,value]
    dictlist.append(temp)


#maybe bad idea to go with an object...
"""
The idea was to get the data using attributes. For that I converted all lists to dictionaries.
But maybe I would rather go the other way, and flatten the list of lists?
"""
class Struct():
    """https://stackoverflow.com/a/6993694/9467950"""
    def __init__(self, data):
        for name, value in data.items():
            setattr(self, name, self._wrap(value))

    def _wrap(self, value):
        if isinstance(value, (tuple, list, set, frozenset)): 
            return type(value)([self._wrap(v) for v in value])
        else:
            return Struct(value) if isinstance(value, dict) else value

datacopy = Struct(tourn_rawdata.copy())
print(datacopy.id1064.date)
print(datacopy.id1064.country)
print(datacopy.id1064.state)
ids = list(datacopy.id1064.participants.__dict__)[0]
datacopy.id1064.participants.eval(ids)

TournamentInstance(simpledata)
simpledata["country"]

