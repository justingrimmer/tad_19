# one way to create a dict

d1 = {'one':'uno', 'two':'dos', 'three':'tres'}

d1['one']

# another way to create a dict

example = ['eins','zwei','drei']

d2 = {}

for i in range(0,2):
    d2[i] = example[i]

d2[0]

# nested dicts

import os, time

os.chdir("C:\\Users\\mkrup\\Dropbox")

#look at all the folders in my dropbox, when were they created and how big are they?

folder_names = [f.path for f in os.scandir() if f.is_dir() ]

#create dict
folders = {}

for fold in folder_names:
    folders[fold] = {}
    folders[fold]['size'] = os.path.getsize(fold)
    folders[fold]['modified'] = time.ctime(os.path.getmtime(fold))

#methods

folders.get('.\\Teaching')
folders.get('xyz')

folders.keys()

folders.values()

#saving dictionaries

#as pickle
import pickle

with open('folders.pickle', 'wb') as handle:
    pickle.dump(folders, handle)

with open('folders.pickle', 'rb') as handle:
    tst1 = pickle.load(handle)

#as json
import json

with open('folders.json', 'w') as json_file:
  json.dump(folders, json_file)
  
with open('folders.json') as json_file:  
    tst2 = json.load(json_file)