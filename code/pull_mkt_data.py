#%% README -- Last Updated 5/30/2025 by Caleb Bray
# DATA PULL MASTER FILE



#%% LIBRARIES
import os
import glob
import numpy as np
import pandas as pd
from steamcrawl import Request
import re
from pull_mkt_data_func import pull_cs_price_history, pull_dota_price_history

#%% FILEPATHS
home_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),".."))
code_dir = os.getcwd()
data_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),"..")) + r"\data"
cs_data_dir = data_dir + r"\cs"
dota_data_dir = data_dir + r"\dota2"
#%% SETUP
#need "URL Decoded" steamLoginSecure from steamcommunity.com (NOT store.steampowered.com !!)
with open(home_dir + r"\steamLoginSecure.txt", 'r') as file:
    steamLoginSecure = file.read().strip()
request = Request(steamLoginSecure) #doesn't work on work pc endpoint

#read each line (each corresponding to one CS item) in the file into a list
with open(home_dir + r"\cs_item_list.txt", 'r') as file:
    cs_item_list = [line.strip() for line in file]
    
#read each line (each corresponding to one dota2 item) in the file into a list
with open(home_dir + r"\dota_item_list.txt", 'r') as file:
    dota_item_list = [line.strip() for line in file]

#possible skins, ak47 elite build, m4a1s basilisk, m4a4 evil daimyo, m4a1s cyrex, m4a1s hyperbeast, deagle conspiracy, p250 supernova,
#glock water elemental, stat trak glock grinder, p90 elite build, mp9 ruby poison dart, mp9 deadly poison, usps stainless

#%% PULL DATA
for item in cs_item_list:
    df = pull_cs_price_history(item,request,cs_data_dir)
    
for item in dota_item_list:
    df = pull_dota_price_history(item,request,dota_data_dir)

#%% JOIN ADDITIONAL DATA TO DFs (origin date -> relative age, qualities -> relative rarity)


#%% JOIN TOGETHER CSVs BY GAME
#map pd.concat onto every csv in data/cs and data/dota2 respectively
cs_df = pd.concat(map(pd.read_csv, glob.glob(cs_data_dir + '\\*.csv')))
dota_df = pd.concat(map(pd.read_csv, glob.glob(dota_data_dir + '\\*.csv')))
combined_df = pd.concat([cs_df, dota_df], ignore_index=True)


#%% WRITE TO CSV
cs_df.to_csv(fr'{cs_data_dir}\cs_combined_items_price_history.csv',index=False)
dota_df.to_csv(fr'{dota_data_dir}\dota2_combined_items_price_history.csv',index=False)
combined_df.to_csv(fr'{data_dir}\combined_items_price_history.csv',index=False)

