library(ggplot2) #plotting
library(fixest) #fixed effects ols
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

#remove Dragonclaw Hook
df_dota <- df_dota[!grepl("Dragonclaw Hook",df_dota$item),]
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

#price adjustment -- think about adding cube term for grade rarity?
p_reg_mos <- lm(log(median_price) ~ rel_age_mos + I(rel_age_mos^2) + grade_rarity + I(grade_rarity^2), weights=volume_sold, data=df_all_mos)
summary(p_reg_mos)

p_reg_mos_1yr <- lm(log(median_price) ~ rel_age_mos + I(rel_age_mos^2) + grade_rarity + I(grade_rarity^2), weights=volume_sold, data=df_all_mos_1yr)
summary(p_reg_mos_1yr)

df_all_mos['log_p_hat'] <- predict(p_reg_mos,df_all_mos,weights=df_all_mos$volume_sold,interval="prediction")[,1]
df_all_mos_1yr['log_p_hat'] <- predict(p_reg_mos_1yr,df_all_mos_1yr,weights=df_all_mos_1yr$volume_sold,interval="prediction")[,1]

#crude DiD
didreg_mos_hat1 <- feols(log_p_hat ~ l_avg_players + post_treat*treated_unit | item+month, weights= ~log(volume_sold), data=df_all_mos)
summary(didreg_mos_hat1)

didreg_mos_1yr_hat1 <- feols(log_p_hat ~ l_avg_players + post_treat*treated_unit | item+month, weights= ~volume_sold, data=df_all_mos_1yr)
summary(didreg_mos_1yr_hat1)

#looks promising, need to reduce post-treat SEs
didreg_mos_hat2 <- feols(log_p_hat ~ avg_players + i(mos_til_treat, treated_unit, ref=-1) | item+month, weights=~log(volume_sold),data=df_all_mos)
summary(didreg_mos_hat2)
pdf("../writing/manuscript/figures/did_mos_2yr_phat_iplot.pdf")
iplot(didreg_mos_hat2,xlab="Months Until Treatment",main="Effect on Log Quality Adjusted Price")
dev.off()

didreg_mos_1yr_hat2 <- feols(log_p_hat ~ log(avg_players) + i(mos_til_treat, treated_unit, ref = -1) | item+month, weights=~volume_sold,data=df_all_mos_1yr)
summary(didreg_mos_1yr_hat2)
iplot(didreg_mos_1yr_hat2)

# VOLUME SOLD REGS
didreg_mos_hat3 <- feols(log(volume_sold) ~ l_avg_players + post_treat*treated_unit | item+month, data=df_all_mos)
summary(didreg_mos_hat3)

didreg_mos_hat4 <- feols(volume_sold ~ avg_players + i(mos_til_treat,treated_unit,ref=-1) | item+month, data=df_all_mos)
summary(didreg_mos_hat4)
pdf("../writing/manuscript/figures/did_mos_2yr_vol_iplot.pdf")
iplot(didreg_mos_hat4,xlab="Months Until Treatment",main="Effect on Volume Sold")
dev.off()

#NO PRICE ADJUSTMENT
didreg_hat1 <- feols(l_median_price ~ avg_players + rel_age_mos + I(rel_age_mos^2) + grade_rarity + I(grade_rarity^2) + post_treat*treated_unit | item+month, weights= ~volume_sold, data=df_all_mos)
summary(didreg_hat1)

didreg_1yr_hat1 <- feols(l_median_price ~ avg_players + rel_age_mos + I(rel_age_mos^2) + grade_rarity + I(grade_rarity^2) + post_treat*treated_unit | item+month, weights= ~volume_sold, data=df_all_mos_1yr)
summary(didreg_1yr_hat1)

didreg_hat2 <- feols(l_median_price ~ rel_age_mos + I(rel_age_mos^2) + grade_rarity + I(grade_rarity^2) + avg_players + i(mos_til_treat, treated_unit, ref=-1) | item+month, weights=~volume_sold,data=df_all_mos)
summary(didreg_hat2)
iplot(didreg_hat2)

didreg_1yr_hat2 <- feols(l_median_price ~ rel_age_mos + I(rel_age_mos^2) + grade_rarity + I(grade_rarity^2) + avg_players + i(mos_til_treat, treated_unit, ref=-1) | item+month, weights=~volume_sold,data=df_all_mos_1yr)
summary(didreg_1yr_hat2)
iplot(didreg_1yr_hat2)
