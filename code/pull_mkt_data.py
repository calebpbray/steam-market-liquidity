#%% README -- Last Updated 5/30/2025 by Caleb Bray




#%% LIBRARIES
import numpy as np
import pandas as pd
from steamcrawl import Request


#%% SETUP
#need "URL Decoded" steamLoginSecure from steamcommunity.com (NOT store.steampowered.com !!)
steamLoginSecure = '76561198049245479||eyAidHlwIjogIkpXVCIsICJhbGciOiAiRWREU0EiIH0.eyAiaXNzIjogInI6MDAxNl8yNjVGRTE1Ql81QzA1NiIsICJzdWIiOiAiNzY1NjExOTgwNDkyNDU0NzkiLCAiYXVkIjogWyAid2ViOmNvbW11bml0eSIgXSwgImV4cCI6IDE3NDg5NTkwOTgsICJuYmYiOiAxNzQwMjMyMTY3LCAiaWF0IjogMTc0ODg3MjE2NywgImp0aSI6ICIwMDAyXzI2NUZFMUEyX0M1NjQ1IiwgIm9hdCI6IDE3NDg2MzM3NzYsICJydF9leHAiOiAxNzY2NzgzMTA0LCAicGVyIjogMCwgImlwX3N1YmplY3QiOiAiMTM2LjQ4LjMwLjIxMyIsICJpcF9jb25maXJtZXIiOiAiMTM2LjQ4LjMwLjIxMyIgfQ.S0XnKTPxk7SDja91ItKME7Z4NzMiyOTA8I2TdRQa0oYO8T9RTtgmjlWeRUmb8uxEd2zUiWEMam1liVOSDjpQAw'

request = Request(steamLoginSecure)

#data_frame = request.get_market_history(count = 10)
#data_frame.to_csv('example.csv')

df1 = pd.DataFrame()
df2 = pd.DataFrame()
str1 = ""

df1 = request.get_item_overview(item_name="USP-S | Printstream (Field-Tested)",appid="730")
df2 = request.get_price_history(item_name="USP-S | Printstream (Field-Tested)",appid="730")
str1 = request.get_itemname_id(item_name="USP-S | Printstream (Field-Tested)",appid="730")

df1.to_csv('item_overview.csv')
df2.to_csv('price_history.csv')
print(str1)
