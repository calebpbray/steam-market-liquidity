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

df_all <- df_all %>% 
  mutate(month = lubridate::floor_date(date, 'month'))

#create one-year data sets for plotting
df_all_1yr <- df_all[df_all$date >= as.Date("2017-03-29") & df_all$date <= as.Date("2019-03-29"), ]
df_cs_1yr <- df_cs[df_cs$date >= as.Date("2017-03-29") & df_cs$date <= as.Date("2019-03-29"), ]
df_dota_1yr <- df_dota[df_dota$date >= as.Date("2017-03-29") & df_dota$date <= as.Date("2019-03-29"), ]

#join on player counts
df_cs_players <- read.csv("../data/cs_player_count.csv")
df_dota_players <- read.csv("../data/dota2_player_count.csv")

df_dota_players['month'] <- as.Date(df_dota_players$Month,format="%m/%d/%Y")
df_cs_players['month'] <- as.Date(df_cs_players$Month,format="%m/%d/%Y")

df_cs_players <- df_cs_players[order(df_cs_players$month), ]
df_dota_players <- df_dota_players[order(df_dota_players$month), ]

df_master <- merge(df_cs_players,df_dota_players,by='month',all=TRUE)
df_all <- merge(df_all,df_master,by='month',all.x=TRUE)
df_all_1yr <- merge(df_all_1yr,df_master,by='month',all.x=TRUE)

df_all <- df_all[ ,!names(df_all) %in% c("Month.x","Month.y")]
df_all <- df_all %>%
                    rename(cs_avg_plyrs = Avg..Players.x,
                           cs_plyr_gain = Gain.x,
                           cs_plyr_pct_gain = X..Gain.x,
                           cs_peak_plyrs = Peak.Players.x,
                           dota_avg_plyrs = Avg..Players.y,
                           dota_plyr_gain = Gain.y,
                           dota_plyr_pct_gain = X..Gain.y,
                           dota_peak_plyrs = Peak.Players.y)

df_all_1yr <- df_all_1yr[ ,!names(df_all_1yr) %in% c("Month.x","Month.y")]
df_all_1yr <- df_all_1yr %>%
  rename(cs_avg_plyrs = Avg..Players.x,
         cs_plyr_gain = Gain.x,
         cs_plyr_pct_gain = X..Gain.x,
         cs_peak_plyrs = Peak.Players.x,
         dota_avg_plyrs = Avg..Players.y,
         dota_plyr_gain = Gain.y,
         dota_plyr_pct_gain = X..Gain.y,
         dota_peak_plyrs = Peak.Players.y)

df_all['avg_players'] <- ifelse(df_all$treated_unit==1,df_all$cs_avg_plyrs,df_all$dota_avg_plyrs)
df_all_1yr['avg_players'] <- ifelse(df_all_1yr$treated_unit==1,df_all_1yr$cs_avg_plyrs,df_all_1yr$dota_avg_plyrs)

df_all['l_avg_players'] <- log(df_all$avg_players)
df_all_1yr['l_avg_players'] <- log(df_all_1yr$avg_players)

df_all['player_gain'] <- as.numeric(ifelse(df_all$treated_unit==1,df_all$cs_plyr_gain,df_all$dota_plyr_gain))
df_all_1yr['player_gain'] <- as.numeric(ifelse(df_all_1yr$treated_unit==1,df_all_1yr$cs_plyr_gain,df_all_1yr$dota_plyr_gain))

df_all['player_pct_gain'] <- as.numeric(ifelse(df_all$treated_unit==1,df_all$cs_plyr_pct_gain,df_all$dota_plyr_pct_gain))
df_all_1yr['player_pct_gain'] <- as.numeric(ifelse(df_all_1yr$treated_unit==1,df_all_1yr$cs_plyr_pct_gain,df_all_1yr$dota_plyr_pct_gain))

#price adjustment
p_reg <- lm(log(median_price) ~ rel_age_days + I(rel_age_days^2) + grade_rarity + I(grade_rarity^2), weights=volume_sold, data=df_all)
summary(p_reg)

p_reg_1yr <- lm(log(median_price) ~ rel_age_days + I(rel_age_days^2) + grade_rarity + I(grade_rarity^2), weights=volume_sold, data=df_all_1yr)
summary(p_reg_1yr)

df_all['log_p_hat'] <- predict(p_reg,df_all,weights=df_all$volume_sold,interval="prediction")[,1]
df_all_1yr['log_p_hat'] <- predict(p_reg_1yr,df_all_1yr,weights=df_all_1yr$volume_sold,interval="prediction")[,1]

#crude DiD
didreg_hat1 <- feols(log_p_hat ~ avg_players + I(avg_players^2) + I(avg_players^3) + post_treat*treated_unit | item+date, weights= ~volume_sold, data=df_all)
summary(didreg_hat1)

didreg_1yr_hat1 <- feols(log_p_hat ~ avg_players + I(avg_players^2) + I(avg_players^3) + post_treat*treated_unit | item+date, weights= ~volume_sold, data=df_all_1yr)
summary(didreg_1yr_hat1)

didreg_hat2 <- feols(log_p_hat ~ l_avg_players + i(days_til_treat, treated_unit, ref=-1,keep=-160:160) | item+date, weights=~volume_sold,data=df_all)
summary(didreg_hat2)
iplot(didreg_hat2, xlim=c(-150,150))

didreg_1yr_hat2 <- feols(log_p_hat ~ l_avg_players + i(days_til_treat, treated_unit, ref = -1,keep=-150:150) | item+date,weights=~volume_sold,data=df_all_1yr)
summary(didreg_1yr_hat2)
pdf("../writing/manuscript/figures/did_1yr_daily_phat_iplot.pdf")
iplot(didreg_1yr_hat2,xlim=c(-50,50))
dev.off()

# VOLUME SOLD REGS
didreg_hat3 <- feols(volume_sold ~ avg_players + i(days_til_treat,treated_unit,ref=-1, keep=-110:110) | item+date, data=df_all)
summary(didreg_hat3)
#pdf("../writing/manuscript/figures/did_2yr_daily_vol_iplot.pdf")
iplot(didreg_hat3,xlab="Months Until Treatment",main="Effect on Volume Sold",xlim=c(-100,100))
#dev.off()

didreg_1yr_hat3 <- feols(volume_sold ~ avg_players + i(days_til_treat,treated_unit,ref=-1, keep=-110:110) | item+date, data=df_all)
summary(didreg_1yr_hat3)
pdf("../writing/manuscript/figures/did_1yr_daily_vol_iplot.pdf")
iplot(didreg_1yr_hat3,xlab="Months Until Treatment",main="Effect on Volume Sold",xlim=c(-100,100))
dev.off()

#NO PRICE ADJUSTMENT
didreg_hat1 <- feols(l_median_price ~ avg_players + rel_age_days + I(rel_age_days^2) + grade_rarity + I(grade_rarity^2) + post_treat*treated_unit | item+date, weights= ~volume_sold, data=df_all)
summary(didreg_hat1)

didreg_1yr_hat1 <- feols(l_median_price ~ rel_age_days + I(rel_age_days^2) + grade_rarity + I(grade_rarity^2) + avg_players + post_treat*treated_unit | item+date, weights= ~volume_sold, data=df_all_1yr)
summary(didreg_1yr_hat1)

didreg_hat2 <- feols(l_median_price ~ rel_age_days + I(rel_age_days^2) + grade_rarity + I(grade_rarity^2) + avg_players + i(days_til_treat, treated_unit, ref=-1,keep=-110:110) | item+date, weights=~volume_sold,data=df_all)
summary(didreg_hat2)
iplot(didreg_hat2, xlim=c(-100,100))

didreg_1yr_hat2 <- feols(l_median_price ~ avg_players + i(days_til_treat, treated_unit, ref = -1,keep=-110:110) | item+date, weights=~volume_sold,data=df_all_1yr)
summary(didreg_1yr_hat2)
iplot(didreg_1yr_hat2,xlim=c(-100,100))