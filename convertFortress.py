#!/usr/bin/python

import sys
import csv
import json
import os

def get_upgrade1(input):
  test = input.split(', ')
  return test

with open(sys.argv[1]) as csv_file:
  csv_reader = csv.reader(csv_file, delimiter=',')
  next(csv_reader)
  if not os.path.exists('parsed'):
    os.mkdir('parsed')
  with open('parsed/' + 'parsed-' + sys.argv[1], "w", newline='') as csv_out:
    
    headers = ['tournament', 'format', 'date', 'playerName', 'score', 'sos', 'swissRank', 'cutRank', 'points_squad', 'faction', 'listID', 'id', 'ship', 'points_ship', 'talent1', 
    'talent2', 'force1', 'sensor1', 'tech1', 'tech2', 'cannon1', 'turret1', 'torpedo1', 'torpedo2', 'missile1', 'missile2', 'crew1', 'crew2', 'crew3', 
    'gunner1', 'gunner2', 'astromech1', 'illicit1', 'illicit2', 'device1', 'device2', 'title1', 'configuration1', 'modification1', 'modification2', 'modification3']
    
    csvwriter = csv.DictWriter(csv_out, fieldnames=headers)
    csvwriter.writeheader()

    listID = 0
    
    for row in csv_reader:
      tournament = row[0]
      format = row[2]
      date = row[3]
      playerName = row[4]
      listID += 1
      xwsSquad = row[5]
      score = row[6]
      sos = row[8]
      swissRank = row[9]
      cutRank = row[10]
      faction = ""
      points_squad = ""
      if xwsSquad:
        parsedSquad = json.loads(xwsSquad)
        faction = parsedSquad['faction']
        for pilot in parsedSquad['pilots']:
          points_squad = parsedSquad.get('points')
          id = pilot['id']
          ship = pilot['ship']
          points_ship = pilot['points']
          upgrades = pilot.get('upgrades')

          #just give it junk so everything is empty
          if not upgrades:
            upgrades = pilot
          talent = upgrades.get('talent', '')
          talent1 = ''
          talent2 = ''
          if(talent):
            talent1 = talent[0]
            if(len(talent) > 1):
              talent2= talent[1]

          force1 = ''
          force = upgrades.get('force', '')
          if(force):
            force1 = force[0]

          sensor1 = ''
          sensor = upgrades.get('sensor', '')
          if(sensor):
            sensor1 = sensor[0]

          tech1 = ''
          tech2 = ''
          tech = upgrades.get('tech', '')
          if(tech):
            tech1 = tech[0]
            if(len(tech) > 1):
              tech2= tech[1]
          
          cannon1 = ''
          cannon = upgrades.get('cannon', '')
          if(cannon):
            cannon1 = cannon[0]

          turret1 = ''
          turret = upgrades.get('turret', '')
          if(turret):
            turret1 = turret[0]

          torpedo1 = ''
          torpedo2 = ''
          torpedo = upgrades.get('torpedo', '')
          if(torpedo):
            torpedo1 = torpedo[0]
            if(len(torpedo) > 1):
              torpedo2= torpedo[1]

          missile1 = ''
          missile2 = ''
          missile = upgrades.get('missile', '')
          if(missile):
            missile1 = missile[0]
            if(len(missile) > 1):
              missile2= missile[1]

          crew1 = ''
          crew2 = ''
          crew3 = ''
          crew = upgrades.get('crew', '')
          if(crew):
            crew1 = crew[0]
            if(len(crew) > 1):
              crew2= crew[1]
              if(len(crew) > 2):
                crew3= crew[2]

          gunner1 = ''
          gunner2 = ''
          gunner = upgrades.get('gunner', '')
          if(gunner):
            gunner1 = gunner[0]
            if(len(gunner) > 1):
              gunner2= gunner[1]
          
          astromech1 = ''
          astromech = upgrades.get('astromech', '')
          if(astromech):
            astromech1 = astromech[0]

          
          illicit1 = ''
          illicit2 = ''
          illicit = upgrades.get('illicit', '')
          if(illicit):
            illicit1 = illicit[0]
            if(len(illicit) > 1):
              illicit2= illicit[1]

          
          device1 = ''
          device2 = ''
          device = upgrades.get('device', '')
          if(device):
            device1 = device[0]
            if(len(device) > 1):
              device2= device[1]

          title1 = ''
          title = upgrades.get('title', '')
          if(title):
            title1 = title[0]

          configuration1 = ''
          configuration = upgrades.get('configuration', '')
          if(configuration):
            configuration1 = configuration[0]
          
          
          modification1 = ''
          modification2 = ''
          modification3 = ''
          modification = upgrades.get('mod', '')
          if(modification):
            modification1 = modification[0]
            if(len(modification) > 1):
              modification2= modification[1]
              if(len(modification) > 2):
                modification3= modification[2]
          
          csvwriter.writerow({'tournament' : tournament,
          'format' : format,
          'date' : date,
          'playerName' : playerName,
          'score' : score,
          'sos' : sos,
          'swissRank' : swissRank,
          'cutRank' : cutRank,
          'points_squad' : points_squad,
          'faction' : faction,
          'listID' : listID,
          'id' : id,
          'ship' : ship,
          'points_ship' : points_ship,
          'talent1' : talent1,
          'talent2' : talent2,
          'force1' : force1,
          'sensor1' : sensor1,
          'tech1' : tech1,
          'tech2' : tech2,
          'cannon1' : cannon1,
          'turret1' : turret1,
          'torpedo1' : torpedo1,
          'torpedo2' : torpedo2,
          'missile1' : missile1,
          'missile2' : missile2,
          'crew1' : crew1,
          'crew2' : crew2,
          'crew3' : crew3,
          'gunner1' : gunner1,
          'gunner2' : gunner2,
          'astromech1' : astromech1,
          'illicit1' : illicit1,
          'illicit2' : illicit2,
          'device1' : device1,
          'device2' : device2,
          'title1' : title1,
          'configuration1' : configuration1,
          'modification1' : modification1,
          'modification2' : modification2,
          'modification3' : modification3})