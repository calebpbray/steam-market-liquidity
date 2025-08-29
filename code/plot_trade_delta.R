library(ggplot2) #plotting
library(fixest) #feols
library(dplyr) #mutate
library(lubridate) #floor_date
library(zoo) #moving averages, rollmean
library(scales)

df_trade_delta <- read.csv("../data/tradeTF_steam_trade_volume_2025_07_15.csv")
df_cs_players <- read.csv("../data/cs_player_count.csv")
df_dota_players <- read.csv("../data/dota2_player_count.csv")

#add date variable for plotting
df_trade_delta['date'] <- as.Date(df_trade_delta$excel_datetime,format="%m/%d/%Y")
df_dota_players['month'] <- as.Date(df_dota_players$Month,format="%m/%d/%Y")
df_cs_players['month'] <- as.Date(df_cs_players$Month,format="%m/%d/%Y")

#fix sorting
df_cs_players <- df_cs_players[order(df_cs_players$month), ]
df_dota_players <- df_dota_players[order(df_dota_players$month), ]

#aggregate to monthly
#create average price (across all items) for dota and cs df's for cool plot
df_trade_delta_mo <- df_trade_delta %>% 
  mutate(month = lubridate::floor_date(date, 'month')) %>%
  group_by(month) %>%
  summarize(trade_offer_delta = mean(trade_offer_delta))
df_trade_delta_mo['trade_delta_3mma'] <- rollmean(df_trade_delta_mo$trade_offer_delta, k = 3, fill = NA, align = "right")
df_trade_delta_mo['mos_til_treat'] <- interval(as.Date('2018-03-01'),df_trade_delta_mo$month) %/% months(1)
df_trade_delta_mo['post_treat'] <- ifelse(df_trade_delta_mo$mos_til_treat >= 0,1,0)


df_cs_players['avg_players_3mma'] <- rollmean(df_cs_players$Avg..Players, k = 3, fill = NA, align = "right")
df_dota_players['avg_players_3mma'] <- rollmean(df_dota_players$Avg..Players, k = 3, fill = NA, align = "right")

#create master df for plotting both in same chart
df_master <- merge(df_dota_players,df_cs_players,by='month',all=TRUE)
df_master <- merge(df_master,df_trade_delta_mo,by='month',all=TRUE)

#cleanup
df_master[,c("Month.x","Month.y")] <- list(NULL)
df_master['mos_til_treat'] <- interval(as.Date('2018-03-01'),df_master$month) %/% months(1)
df_master['post_treat'] <- ifelse(df_master$mos_til_treat >= 0,1,0)
names(df_master) <- c("month","dota_avg_players","dota_avg_gain","dota_pct_gain","dota_peak_players","dota_avg_players_3mma","cs_avg_players","cs_avg_gain","cs_pct_gain","cs_peak_players","cs_avg_players_3mma","trade_offer_delta","trade_delta_3mma","mos_til_treat","post_treat")

#create transformed data to plot

df_master['dota_avg_players_trans'] <- df_master['dota_avg_players'] * 1000
df_master['cs_avg_players_trans'] <- df_master['cs_avg_players'] * 1000
df_master['trade_delta_trans'] <- df_master['trade_offer_delta'] * 1000

#weekly
ggplot(data=df_trade_delta, aes(x=date, y=trade_offer_delta)) + 
  geom_line()+
  geom_vline(xintercept = as.Date("2018-03-29"))

#monthly -- FIX THE AXIS STUFF
ggplot(df_master, aes(x=month)) + 
  geom_line(aes(y=trade_delta_trans,color="Trade Offer Delta (Left)"))+
  geom_line(aes(y=dota_avg_players_trans,color="DOTA 2 (Right)"))+
  geom_line(aes(y=cs_avg_players_trans,color="Counter Strike (Right)"))+
  scale_y_continuous("Trade Offer Delta",
                     labels = scales::comma,
                     sec.axis = sec_axis(transform = ~./1000000,name = "Player Count")) +
  scale_color_manual(name = "Legend", values = c("Trade Offer Delta (Left)" = "black", "DOTA 2 (Right)" = "red", "Counter Strike (Right)" = "orange")) +
  geom_vline(xintercept = as.Date("2018-03-01"))

#3mma
ggplot(df_master, aes(x=month, y=trade_delta_3mma)) + 
  geom_line(aes(color="Trade Offer Delta (Left)"))+
  geom_line(aes(y=dota_avg_players_3mma,color="DOTA 2 (Right)"))+
  geom_line(aes(y=cs_avg_players_3mma,color="Counter Strike (Right)"))+
  scale_y_continuous("Trade Offer Delta",labels = scales::comma, sec.axis = sec_axis((~.),name = "Player Count")) +
  scale_color_manual(name = "Legend", values = c("Trade Offer Delta (Left)" = "black", "DOTA 2 (Right)" = "red", "Counter Strike (Right)" = "orange")) +
  geom_vline(xintercept = as.Date("2018-03-01"))