#%% README -- Last Updated 5/30/2025 by Caleb Bray

#%% LIBRARIES
import numpy as np
import pandas as pd
from steamcrawl import Request
import re
import statistics as stats
from datetime import datetime
#from dateutil.parser import parse

#%% PULL COUNTER STRIKE PRICE DATA
def pull_cs_price_history(item,request,out_dir):
    """Wrapper for steamcrawl's get_price_history method with settings for Counter Strike pre-applied.

    Args:
        request (steamcrawl.Request):   steamcrawl.Request object containing a 'steamLoginSecure' key which must be updated daily
        item (str):                     Counter Strike Community Market full item name
        out_dir (str):                  directory to output .csv

    Returns:
        pull_df(DataFrame): Pandas dataframe with columns: 
                            date (str):                     date of data point
                            py_date (datetime.datetime):    datetime object of date
                            median_price (float):           median price of data point, rounded to 3 decimals
                            volume_sold (int):              volume sold of data point at date and median_price
                            item (str):                     item name without StatTrak and wear
                            equip (str):                    weapon of item, ie where item is equipped, equivalent to dota2 hero
                            skin (str):                     item skin, ie without StatTrak, weapon name, and wear
                            wear (str):                     item wear rating
                            stattrak (int):                 {0,1} dummy for StatTrak
                            treated_unit (int):             {0,1} dummy, dota2 = 0, cs2 = 1
                            post_treat (int):               {0,1} dummy, equals 1 if py_date >= March 29, 2018
        {item}_price_history.csv (csv): csv output of pull_df
    """
    
    pull_df = request.get_price_history(item_name=item,appid="730")
    #create python datetime format for date
    pull_df['py_date'] = pd.to_datetime(pull_df['date'], format='%b %d %Y')
    #use item name to get item qualities
    pull_df['item'] = item.replace("StatTrak™ ","").replace(re.search(r'\((?!.*Dragon King.*?\)).*?\)',item).group(0),"").strip() #item name excluding StatTrak and wear (wear being everything in parentheses except "(Dragon King)")
    pull_df['equip'] = item.split(" |")[0].replace("StatTrak™ ","").strip() #weapon name, ex: AK-47
    pull_df['skin'] = pull_df['item'][0].split(" | ")[1].strip() #weapon skin, eg "Redline"
    print(pull_df['skin'][0])
    pull_df['wear'] = re.search(r'\((?!Dragon King\)).*?\)',item).group(0).replace("(","").replace(")","").strip() #item wear rating
    pull_df['treated_unit'] = 1
    #assign StatTrak value
    if "StatTrak™" in item:
        pull_df['stattrak'] = 1
    else:
        pull_df['stattrak'] = 0
    #assign pre/post-intervention status (Mar 29 2018)
    pull_df['post_treat'] = 0
    pull_df.loc[pull_df['py_date'] >= datetime(2018,3,29), 'post_treat'] = 1
    #get stats
    pull_df['rolling_30day_sd'] = pull_df['median_price'].rolling(window=30, min_periods=1).std() #30-day rolling standard deviation
    pull_df['ewm_30day_sd'] = pull_df['median_price'].ewm(span=30, adjust=False).std() #30-day exponentially weighted moving standard deviation
    pull_df['expanding_sd'] = pull_df['median_price'].expanding(min_periods=1).std() #expanding standard deviation
    pull_df['total_sd'] = pull_df['median_price'].std() #total standard deviation
    pull_df['pre_sd'] = pull_df.loc[pull_df['py_date'] < datetime(2018,3,29), 'median_price'].std() #pre-intervention standard deviation
    pull_df['post_sd'] = pull_df.loc[pull_df['py_date'] >= datetime(2018,3,29), 'median_price'].std() #post-intervention standard deviation
    #clean a little
    pull_df['median_price'] = pull_df["median_price"].round(3)
    pull_df = pull_df[['date','py_date','median_price','rolling_30day_sd','ewm_30day_sd','expanding_sd','total_sd','pre_sd','post_sd','volume_sold','item','weapon','skin','wear','stattrak','treated_unit','post_treat']]
    #export
    pull_df.to_csv(f'{out_dir}/{item.replace("™","").replace("|","").replace(" ","")}_price_history.csv',index=False)
    return pull_df

#%% PULL DOTA 2 PRICE DATA
def pull_dota_price_history(item,request,out_dir):
    """Wrapper for steamcrawl's get_price_history method with settings for DOTA 2 pre-applied.

    Args:
        request (steamcrawl.Request):   steamcrawl.Request object containing a 'steamLoginSecure' key which must be updated daily
        item (str):                     Counter Strike Community Market full item name
        out_dir (str):                  directory to output .csv

    Returns:
        pull_df(DataFrame): Pandas dataframe with columns: 
                            date (str):                     date of data point
                            py_date (datetime.datetime):    datetime object of date
                            median_price (float):           median price of data point, rounded to 3 decimals
                            volume_sold (int):              volume sold of data point at date and median_price
                            item (str):                     item name
                            treated_unit (int):             {0,1} dummy, dota2 = 0, cs2 = 1
                            post_treat (int):               {0,1} dummy, equals 1 if py_date >= March 29, 2018
        {item}_price_history.csv (csv): csv output of pull_df
    """
    pull_df = request.get_price_history(item_name=item,appid="570")
    #create python datetime format for date
    pull_df['py_date'] = pd.to_datetime(pull_df['date'], format='%b %d %Y')
    #use item name to get item qualities
    pull_df['item'] = item #item name
    pull_df['treated_unit'] = 0
    #assign pre/post-intervention status (Mar 29 2018)
    pull_df['post_treat'] = 0
    pull_df.loc[pull_df['py_date'] >= datetime(2018,3,29), 'post_treat'] = 1
    #get stats
    pull_df['rolling_30day_sd'] = pull_df['median_price'].rolling(window=30, min_periods=1).std() #30-day rolling standard deviation
    pull_df['ewm_30day_sd'] = pull_df['median_price'].ewm(span=30, adjust=False).std() #30-day exponentially weighted moving standard deviation
    pull_df['expanding_sd'] = pull_df['median_price'].expanding(min_periods=1).std() #expanding standard deviation
    pull_df['total_sd'] = pull_df['median_price'].std() #total standard deviation
    pull_df['pre_sd'] = pull_df.loc[pull_df['py_date'] < datetime(2018,3,29), 'median_price'].std() #pre-intervention standard deviation
    pull_df['post_sd'] = pull_df.loc[pull_df['py_date'] >= datetime(2018,3,29), 'median_price'].std() #post-intervention standard deviation 
    #clean a little
    pull_df['median_price'] = pull_df["median_price"].round(3)
    pull_df = pull_df[['date','py_date','median_price','rolling_30day_sd','ewm_30day_sd','expanding_sd','total_sd','pre_sd','post_sd','volume_sold','item','treated_unit','post_treat']]
    #export
    pull_df.to_csv(f'{out_dir}/{item.replace("™","").replace("|","").replace(" ","")}_price_history.csv',index=False)
    return pull_df