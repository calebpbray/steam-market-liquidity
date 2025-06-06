#%% README -- Last Updated 5/30/2025 by Caleb Bray




#%% LIBRARIES
import numpy as np
import pandas as pd
from steamcrawl import Request
import re
import os

#%% FILEPATHS
home_filepath = os.path.abspath(os.path.join(os.path.dirname("__file__","..")))
code_filepath = os.getcwd()
raw_filepath = home_filepath + "/"
#%% SETUP
#need "URL Decoded" steamLoginSecure from steamcommunity.com (NOT store.steampowered.com !!)
steamLoginSecure = '76561198049245479||eyAidHlwIjogIkpXVCIsICJhbGciOiAiRWREU0EiIH0.eyAiaXNzIjogInI6MDAxNl8yNjVGRTE1Ql81QzA1NiIsICJzdWIiOiAiNzY1NjExOTgwNDkyNDU0NzkiLCAiYXVkIjogWyAid2ViOmNvbW11bml0eSIgXSwgImV4cCI6IDE3NDkxMzc2MTgsICJuYmYiOiAxNzQwNDEwMzIzLCAiaWF0IjogMTc0OTA1MDMyMywgImp0aSI6ICIwMDAyXzI2NjhGQjI0X0FFQjcxIiwgIm9hdCI6IDE3NDg2MzM3NzYsICJydF9leHAiOiAxNzY2NzgzMTA0LCAicGVyIjogMCwgImlwX3N1YmplY3QiOiAiMTM2LjQ4LjMwLjIxMyIsICJpcF9jb25maXJtZXIiOiAiMTM2LjQ4LjMwLjIxMyIgfQ.-iALBAS4T14j5gBwP0MsNYfSIWkgJUfZOAu3IcMAZStIWkuu9kyL2mtlVGWGVawgGXmaHt8A6kZ9qEh0xktIDQ'
request = Request(steamLoginSecure)

item_list = ["AK-47 | Redline (Battle-Scarred)", "AK-47 | Redline (Well-Worn)","AK-47 | Redline (Field-Tested)","AK-47 | Redline (Minimal Wear)"]
#possible skins, ak47 elite build, m4a1s basilisk, m4a4 evil daimyo, m4a1s cyrex, m4a1s hyperbeast, deagle conspiracy, p250 supernova, 
#glock water elemental, stat trak glock grinder, p90 elite build, mp9 ruby poison dart, mp9 deadly poison, usps stainless

#%% PULL DATA
#data_frame = request.get_market_history(count = 10)
#data_frame.to_csv('example.csv')

for item in item_list:
    #df = request.get_item_overview(item_name="AK-47 | Redline (Field-Tested)",appid="730")
    pull_df = request.get_price_history(item_name=item,appid="730")
    #use item name to get item qualities
    pull_df['item'] = item.replace(" (","").replace(re.search(r'\((.*?)\)',item).group(1),"").replace(")","") #item name excluding wear
    pull_df['weapon'] = item.split(" |")[0]                                                                   #weapon
    pull_df['skin'] = pull_df['item'][0].split(" | ")[1]                                                      #weapon skin, eg "Redline"
    pull_df['wear'] = re.search(r'\((.*?)\)',item).group(1)                                                   #item wear rating
    #clean a little
    pull_df['median_price'] = pull_df["median_price"].round(3)
    #export
    pull_df.to_csv(f'{item.replace("|","").replace(" ","")}_price_history.csv',write_index=False)
