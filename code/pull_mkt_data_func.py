#%% README -- Last Updated 5/30/2025 by Caleb Bray

#%% LIBRARIES
import numpy as np
import pandas as pd
from steamcrawl import Request
import re

#%% PULL COUNTER STRIKE PRICE DATA
def pull_cs_price_history(item,request,out_dir):
    """Wrapper for steamcrawl's get_price_history method with settings for Counter Strike pre-applied.

    Args:
        request (steamcrawl.Request):   steamcrawl.Request object containing a 'steamLoginSecure' key which must be updated daily
        item (str):                     Counter Strike Community Market full item name
        out_dir (str):                  directory to output .csv

    Returns:
        pull_df(DataFrame): Pandas dataframe with columns: 
                            date (str):            date of data point
                            median_price (float):  median price of data point, rounded to 3 decimals
                            volume_sold (int):     volume sold of data point at date and median_price
                            item (str):            item name without StatTrak and wear
                            weapon (str):          weapon of item
                            skin (str):            item skin, ie without StatTrak, weapon name, and wear
                            wear (str):            item wear rating
                            stattrak (int):        {0,1} dummy for StatTrak
        {item}_price_history.csv (csv): csv output of pull_df
    """
    
    pull_df = request.get_price_history(item_name=item,appid="730")
    #use item name to get item qualities
    pull_df['item'] = item.replace("StatTrak™ ","").replace(" (","").replace(re.search(r'\((.*?)\)',item).group(1),"").replace(")","") #item name excluding StatTrak and wear
    pull_df['weapon'] = item.split(" |")[0].replace("StatTrak™ ","") #weapon
    pull_df['skin'] = pull_df['item'][0].split(" | ")[1] #weapon skin, eg "Redline"
    pull_df['wear'] = re.search(r'\((.*?)\)',item).group(1) #item wear rating
    #assign StatTrak value
    if "StatTrak™" in item:
        pull_df['stattrak'] = 1
    else:
        pull_df['stattrak'] = 0
    #clean a little
    pull_df['median_price'] = pull_df["median_price"].round(3)
    #export
    pull_df.to_csv(f'{out_dir}/{item.replace("™","").replace("|","").replace(" ","")}_price_history.csv',index=False)
    return pull_df

#%% PULL DOTA 2 PRICE DATA
