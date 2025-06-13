#%% README -- Last Updated 5/30/2025 by Caleb Bray
# DATA PULL MASTER FILE



#%% LIBRARIES
import numpy as np
import pandas as pd
from steamcrawl import Request
import re
import os
from pull_mkt_data_func import pull_cs_price_history, pull_dota_price_history

#%% FILEPATHS
home_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),".."))
code_dir = os.getcwd()
data_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),"..")) + "/data"
cs_data_dir = data_dir + "/cs"
dota_data_dir = data_dir + "/dota2"
#%% SETUP
#need "URL Decoded" steamLoginSecure from steamcommunity.com (NOT store.steampowered.com !!)
with open(home_dir + "/steamLoginSecure.txt", 'r') as file:
    steamLoginSecure = file.read().strip()
request = Request(steamLoginSecure) #doesn't work on work pc endpoint

#read each line (each corresponding to one CS item) in the file into a list
with open(home_dir + "/cs_item_list.txt", 'r') as file:
    cs_item_list = [line.strip() for line in file]
    
#read each line (each corresponding to one dota2 item) in the file into a list
with open(home_dir + "/dota_item_list.txt", 'r') as file:
    dota_item_list = [line.strip() for line in file]

#possible skins, ak47 elite build, m4a1s basilisk, m4a4 evil daimyo, m4a1s cyrex, m4a1s hyperbeast, deagle conspiracy, p250 supernova, 
#glock water elemental, stat trak glock grinder, p90 elite build, mp9 ruby poison dart, mp9 deadly poison, usps stainless

#%% PULL DATA

for item in cs_item_list:
    df = pull_cs_price_history(item,request,cs_data_dir)
    
for item in dota_item_list:
    df = pull_dota_price_history(item,request,dota_data_dir)
