#%% README -- Last Updated 5/30/2025 by Caleb Bray
# DATA PULL MASTER FILE



#%% LIBRARIES
import os

import re
import glob
from datetime import datetime
import numpy as np
import pandas as pd
from steamcrawl import Request
from pull_mkt_data_func import pull_cs_price_history, pull_dota_price_history

#%% FILEPATHS
home_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),".."))
code_dir = os.getcwd()
data_dir = os.path.abspath(os.path.join(os.path.dirname(__file__),"..")) + r"\data"
cs_data_dir = data_dir + r"\cs"
dota_data_dir = data_dir + r"\dota2"
attributes_dir = data_dir + r"\set_attributes"

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
    
#for item in dota_item_list:
#    df = pull_dota_price_history(item,request,dota_data_dir)

#%% JOIN TOGETHER CSVs BY GAME
#map pd.concat onto every csv in data/cs and data/dota2 respectively
cs_df = pd.concat(map(pd.read_csv, glob.glob(cs_data_dir + '\\*.csv')))
dota_df = pd.concat(map(pd.read_csv, glob.glob(dota_data_dir + '\\*.csv')))

#%% JOIN ADDITIONAL DATA TO DFs (origin date -> relative age, qualities -> relative rarity)

#merge item qualities into master file
cs_qualities_df = pd.read_csv(home_dir + r'\cs_item_list_qualities.csv')
dota_qualities_df = pd.read_csv(home_dir + r'\dota_item_list_qualities.csv')

#merge item qualities with attributes
cs_qualities_df = cs_qualities_df.merge(pd.read_csv(attributes_dir + r'\cs_grade_rarities.csv'), on='grade', how='left') #grade & drop

dota_qualities_df = dota_qualities_df.merge(pd.read_csv(attributes_dir + r'\dota_grade_rarities.csv'), on='grade', how='left') #grade
dota_qualities_df = dota_qualities_df.merge(pd.read_csv(attributes_dir + r'\dota_drop_rarities.csv'), on='drop', how='left') #drop

#output full qualities dataset
cs_qualities_df.to_csv(fr'{home_dir}\cs_full_item_qualities.csv',index=False)
dota_qualities_df.to_csv(fr'{home_dir}\dota_full_item_qualities.csv',index=False)

#merge price data with item qualities and attributes
cs_df = cs_df.merge(cs_qualities_df, on='item', how='left') #item qualities
cs_df = cs_df.merge(pd.read_csv(attributes_dir + r'\cs_wear_rarities.csv'), on='wear', how='left') #wear
cs_df = cs_df.merge(pd.read_csv(attributes_dir + r'\cs_stattrak_rarity.csv'), on='stattrak', how='left') #stattrak

dota_df = dota_df.merge(dota_qualities_df, on='item', how='left') #item qualities

#get relative age now that we have merged in everything
cs_df['rel_age_days'] = (pd.to_datetime(cs_df['py_date']) - pd.to_datetime(cs_df['origin_date'])).dt.days
dota_df['rel_age_days'] = (pd.to_datetime(dota_df['py_date']) - pd.to_datetime(dota_df['origin_date'])).dt.days
cs_df['rel_age_mos'] = cs_df['rel_age_days']/30.44
dota_df['rel_age_mos'] = dota_df['rel_age_days']/30.44

#days/months until treatment
cs_df['days_til_treat'] = (pd.to_datetime(cs_df['py_date']) -  datetime.strptime("2018-03-29", "%Y-%m-%d")).dt.days
dota_df['days_til_treat'] = (pd.to_datetime(dota_df['py_date']) -  datetime.strptime("2018-03-29", "%Y-%m-%d")).dt.days

cs_df['mos_til_treat'] = cs_df['days_til_treat']/30.44
dota_df['mos_til_treat'] = dota_df['days_til_treat']/30.44

#calculate relative rarity
cs_df['rel_rarity'] = cs_df['stattrak_rarity']*cs_df['grade_rarity']*cs_df['wear_rarity']
dota_df['rel_rarity'] = dota_df['grade_rarity']

#add cs 'drop' column for alignment reasons
cs_df['drop'] = cs_df['grade']

#%% CLEANUP AND WRITE TO CSV
#rearrange columns and write cs and dota df's
cs_df = cs_df[['item','date','py_date','days_til_treat','mos_til_treat','origin_date','rel_age_days', 'rel_age_mos','median_price','rolling_30day_sd','ewm_30day_sd','expanding_sd','total_sd','pre_sd','post_sd','volume_sold','item','equip','skin','treated_unit','post_treat','stattrak','stattrak_rarity','grade','grade_rarity','grade_count','wear','wear_rarity','wear_count','drop','drop_rarity','rel_rarity']]
dota_df = dota_df[['item','date','py_date','days_til_treat','mos_til_treat','origin_date','rel_age_days', 'rel_age_mos','median_price','rolling_30day_sd','ewm_30day_sd','expanding_sd','total_sd','pre_sd','post_sd','volume_sold','item','equip','treated_unit','post_treat','grade','grade_rarity','grade_count','drop','drop_rarity','rel_rarity']]

cs_df.to_csv(fr'{data_dir}\cs_combined_items_price_history.csv',index=False)
dota_df.to_csv(fr'{data_dir}\dota2_combined_items_price_history.csv',index=False)

#WIP NEED TO GET COLUMNS TO ALIGN BY CALCULATING RELATIVE RARITIES
#add and delete extra columns to get the frames to align
cs_df = cs_df.drop(['skin','stattrak','stattrak_rarity','wear','wear_rarity','wear_count'],axis=1)

combined_df = pd.concat([cs_df, dota_df], ignore_index=True)
combined_df.to_csv(fr'{data_dir}\combined_items_price_history.csv',index=False)

