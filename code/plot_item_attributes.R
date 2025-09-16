library(ggplot2) #plotting
library(fixest) #fixed effects ols
#library(plm) #alt-fixed effects package
#library(did) #callaway & santa anna DiD
library(dplyr) #dataframe manipulation
library(lubridate) #date handling
library(scales) #editing y axes
library(patchwork) #arranging multiple plots side by side
library(latex2exp) #LaTeX expressions
library(ggthemes) #ggplot themes

df_cs_raw <- read.csv("../data/cs_combined_items_price_history.csv")
df_dota_raw <- read.csv("../data/dota2_combined_items_price_history.csv")
df_all_raw <- read.csv("../data/combined_items_price_history.csv")

df_cs_raw['date'] <- as.Date(df_cs_raw$date, format = "%b %d %Y")
df_dota_raw['date'] <- as.Date(df_dota_raw$date, format = "%b %d %Y")
df_all_raw['date'] <- as.Date(df_all_raw$date, format = "%b %d %Y")

df_cs_raw['mo_yr'] <- format(as.Date(df_cs_raw$date), "%Y-%m")
df_dota_raw['mo_yr'] <- format(as.Date(df_dota_raw$date), "%Y-%m")
df_all_raw['mo_yr'] <- format(as.Date(df_all_raw$date), "%Y-%m")

df_cs_raw['l_median_price'] <- log(df_cs_raw['median_price'])
df_dota_raw['l_median_price'] <- log(df_dota_raw['median_price'])
df_all_raw['l_median_price'] <- log(df_all_raw['median_price'])

#keep data within 2 years of treatment
df_cs <- df_cs_raw[df_cs_raw$date >= as.Date("2016-03-29") & df_cs_raw$date <= as.Date("2020-03-29"), ]
df_dota <- df_dota_raw[df_dota_raw$date >= as.Date("2016-03-29") & df_dota_raw$date <= as.Date("2020-03-29"), ]
df_all <- df_all_raw[df_all_raw$date >= as.Date("2016-03-29") & df_all_raw$date <= as.Date("2020-03-29"), ]

#remove dragonclaw hook
df_all <- df_all[!grepl("Dragonclaw Hook",df_all$item),]

#groupby month and take weighted averages (but sum vol_sold)
df_all_mos <- df_all %>% 
  mutate(month = lubridate::floor_date(date, 'month')) %>%
  group_by(item,month) %>%
  summarize(
    median_price = weighted.mean(median_price,volume_sold),
    l_median_price = weighted.mean(l_median_price,volume_sold),
    rel_rarity = weighted.mean(rel_rarity,volume_sold),
    grade_rarity = weighted.mean(grade_rarity,volume_sold),
    rel_age_mos = weighted.mean(rel_age_mos,volume_sold),
    volume_sold = sum(volume_sold)
  )
#  summarize(across(c('median_price','rel_rarity','grade_rarity','l_median_price','rel_age_mos'),weighted.mean())
df_all_mos['mos_til_treat'] <- interval(as.Date('2018-03-01'),df_all_mos$month) %/% months(1)
df_all_mos['post_treat'] <- ifelse(df_all_mos$mos_til_treat >= 0,1,0)
df_all_mos['treated_unit'] <- 0 #assign all values to 0
df_all_mos[grepl("\\|", df_all_mos$item),'treated_unit'] <- 1 #assign 1 to any rows where item has a "|" as that indicates a CSGO item

#create average price (across all items) for dota and cs df's for cool plot
df_cs_moavg <- df_cs %>% 
  mutate(month = lubridate::floor_date(date, 'month')) %>%
  group_by(month) %>%
  summarize(l_median_price = weighted.mean(l_median_price, volume_sold))
df_cs_moavg['mos_til_treat'] <- interval(as.Date('2018-03-01'),df_cs_moavg$month) %/% months(1)
df_cs_moavg['post_treat'] <- ifelse(df_cs_moavg$mos_til_treat >= 0,1,0)
df_cs_moavg['treated_unit'] <- 1 #assign all values to 1

df_dota_moavg <- df_dota %>% 
  mutate(month = lubridate::floor_date(date, 'month')) %>%
  group_by(month) %>%
  summarize(l_median_price = weighted.mean(l_median_price, volume_sold))
df_dota_moavg['mos_til_treat'] <- interval(as.Date('2018-03-01'),df_dota_moavg$month) %/% months(1)
df_dota_moavg['post_treat'] <- ifelse(df_dota_moavg$mos_til_treat >= 0,1,0)
df_dota_moavg['treated_unit'] <- 0 #assign all values to 0

#create one-year data sets for plotting
df_all_1yr <- df_all[df_all$date >= as.Date("2017-03-29") & df_all$date <= as.Date("2019-03-29"), ]
df_cs_1yr <- df_cs[df_cs$date >= as.Date("2017-03-29") & df_cs$date <= as.Date("2019-03-29"), ]
df_dota_1yr <- df_dota[df_dota$date >= as.Date("2017-03-29") & df_dota$date <= as.Date("2019-03-29"), ]

df_all_mos_1yr <- df_all_mos[df_all_mos$month >= as.Date("2017-03-29") & df_all_mos$month <= as.Date("2019-03-29"), ]
df_cs_moavg_1yr <- df_cs_moavg[df_cs_moavg$month >= as.Date("2017-03-29") & df_cs_moavg$month <= as.Date("2019-03-29"), ]
df_dota_moavg_1yr <- df_dota_moavg[df_dota_moavg$month >= as.Date("2017-03-29") & df_dota_moavg$month <= as.Date("2019-03-29"), ]

#join on player counts
df_cs_players <- read.csv("../data/cs_player_count.csv")
df_dota_players <- read.csv("../data/dota2_player_count.csv")

df_dota_players['month'] <- as.Date(df_dota_players$Month,format="%m/%d/%Y")
df_cs_players['month'] <- as.Date(df_cs_players$Month,format="%m/%d/%Y")

df_cs_players <- df_cs_players[order(df_cs_players$month), ]
df_dota_players <- df_dota_players[order(df_dota_players$month), ]

df_master <- merge(df_cs_players,df_dota_players,by='month',all=TRUE)
df_all_mos <- merge(df_all_mos,df_master,by='month',all.x=TRUE)
df_all_mos_1yr <- merge(df_all_mos_1yr,df_master,by='month',all.x=TRUE)

df_all_mos_1yr <- df_all_mos_1yr[ ,!names(df_all_mos_1yr) %in% c("Month.x","Month.y")]
df_all_mos_1yr <- df_all_mos_1yr %>%
  rename(cs_avg_plyrs = Avg..Players.x,
         cs_plyr_gain = Gain.x,
         cs_plyr_pct_gain = X..Gain.x,
         cs_peak_plyrs = Peak.Players.x,
         dota_avg_plyrs = Avg..Players.y,
         dota_plyr_gain = Gain.y,
         dota_plyr_pct_gain = X..Gain.y,
         dota_peak_plyrs = Peak.Players.y,)

df_all_mos <- df_all_mos[ ,!names(df_all_mos) %in% c("Month.x","Month.y")]
df_all_mos <- df_all_mos %>%
  rename(cs_avg_plyrs = Avg..Players.x,
         cs_plyr_gain = Gain.x,
         cs_plyr_pct_gain = X..Gain.x,
         cs_peak_plyrs = Peak.Players.x,
         dota_avg_plyrs = Avg..Players.y,
         dota_plyr_gain = Gain.y,
         dota_plyr_pct_gain = X..Gain.y,
         dota_peak_plyrs = Peak.Players.y,)

df_all_mos['avg_players'] <- ifelse(df_all_mos$treated_unit==1,df_all_mos$cs_avg_plyrs,df_all_mos$dota_avg_plyrs)
df_all_mos_1yr['avg_players'] <- ifelse(df_all_mos_1yr$treated_unit==1,df_all_mos_1yr$cs_avg_plyrs,df_all_mos_1yr$dota_avg_plyrs)

df_all_mos['l_avg_players'] <- log(df_all_mos$avg_players)
df_all_mos_1yr['l_avg_players'] <- log(df_all_mos_1yr$avg_players)

df_all_mos['player_gain'] <- as.numeric(ifelse(df_all_mos$treated_unit==1,df_all_mos$cs_plyr_gain,df_all_mos$dota_plyr_gain))
df_all_mos_1yr['player_gain'] <- as.numeric(ifelse(df_all_mos_1yr$treated_unit==1,df_all_mos_1yr$cs_plyr_gain,df_all_mos_1yr$dota_plyr_gain))

df_all_mos['player_pct_gain'] <- as.numeric(ifelse(df_all_mos$treated_unit==1,df_all_mos$cs_plyr_pct_gain,df_all_mos$dota_plyr_pct_gain))
df_all_mos_1yr['player_pct_gain'] <- as.numeric(ifelse(df_all_mos_1yr$treated_unit==1,df_all_mos_1yr$cs_plyr_pct_gain,df_all_mos_1yr$dota_plyr_pct_gain))

#group by age -- need to remove Dragonclaw Hook 
df_cs_mos_grp_age <- df_all_mos[df_all_mos$treated_unit == 1,] %>%
  mutate(age_grp = cut(rel_age_mos, breaks=c(0:93))) %>%
  group_by(age_grp) %>%
  summarize(median_price = weighted.mean(median_price, volume_sold),
            l_median_price = weighted.mean(l_median_price, volume_sold))

df_dota_mos_grp_age <- df_all_mos[df_all_mos$treated_unit == 0,] %>%
  mutate(age_grp = cut(rel_age_mos, breaks=c(0:93))) %>%
  group_by(age_grp) %>%
  summarize(median_price = weighted.mean(median_price, volume_sold),
            l_median_price = weighted.mean(l_median_price, volume_sold))

df_cs_mos_grp_age['age_grp'] <- as.numeric(rownames(df_cs_mos_grp_age))-1
df_dota_mos_grp_age['age_grp'] <- as.numeric(rownames(df_dota_mos_grp_age))-1



#age X price
plot1 <-  ggplot(NULL,aes(x=age_grp,y=l_median_price)) + 
  geom_point(data=df_cs_mos_grp_age,aes(color="Counter-Strike")) +
  geom_point(data=df_dota_mos_grp_age,aes(color="Dota 2")) + 
  geom_smooth(data=df_cs_mos_grp_age,method = "lm", formula = y ~ x + I(x^2), alpha = 0.2,aes(color="Counter-Strike")) +
  geom_smooth(data=df_dota_mos_grp_age,method = "lm", formula = y ~ x + I(x^2),alpha = 0.2, aes(color="Dota 2")) +
  #geom_point(data=df_dota_mos_grp_age, aes(y=median_price,color="Dota 2"),show.legend=TRUE) +
  #geom_point(data=df_dota_moavg, aes(y=l_median_price,color="Dota 2")) +
  scale_y_continuous("Log Median Price",labels = scales::comma) +
  scale_color_manual(NULL, name = "Legend", values = c("Counter-Strike" = "#EDA338", "Dota 2" = "#FF0000")) +
  xlab("Relative Age (Months)") +
  theme(axis.title.x = element_blank()) + theme_few()
plot1

#rarity X price
plot1 <-  ggplot(NULL,aes(x=age_grp,y=l_median_price)) + 
  geom_point(data=df_cs_mos_grp_age,aes(color="Counter-Strike")) +
  geom_point(data=df_dota_mos_grp_age,aes(color="Dota 2")) + 
  geom_smooth(data=df_cs_mos_grp_age,method = "lm", formula = y ~ x + I(x^2), alpha = 0.2,aes(color="Counter-Strike")) +
  geom_smooth(data=df_dota_mos_grp_age,method = "lm", formula = y ~ x + I(x^2),alpha = 0.2, aes(color="Dota 2")) +
  #geom_point(data=df_dota_mos_grp_age, aes(y=median_price,color="Dota 2"),show.legend=TRUE) +
  #geom_point(data=df_dota_moavg, aes(y=l_median_price,color="Dota 2")) +
  scale_y_continuous("Log Median Price",labels = scales::comma) +
  scale_color_manual(NULL, name = "Legend", values = c("Counter-Strike" = "#EDA338", "Dota 2" = "#FF0000")) +
  xlab("Relative Age (Months)") +
  theme(axis.title.x = element_blank()) + theme_few()
plot1