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

#keep data within 1 years of treatment
#df_cs <- df_cs_raw[df_cs_raw$date >= as.Date("2017-03-29") & df_cs_raw$date <= as.Date("2019-03-29"), ]
#df_dota <- df_dota_raw[df_dota_raw$date >= as.Date("2017-03-29") & df_dota_raw$date <= as.Date("2019-03-29"), ]
#df_all <- df_all_raw[df_all_raw$date >= as.Date("2017-03-29") & df_all_raw$date <= as.Date("2019-03-29"), ]

#WIP -- TRY TIME SERIES EVENT STUDY
#maybe look into aggregating to monthly?
#https://libguides.princeton.edu/R-Timeseries
#https://cran.r-project.org/web/packages/ggfixest/vignettes/ggiplot.html
#https://asjadnaqvi.github.io/DiD/docs/code_r/07-twfe_r/#r-code

#df_cs['D'] <- df_cs['treated_unit']*df_cs['post_treat']
#df_dota['D'] <- df_dota['treated_unit']*df_dota['post_treat']
#df_all['D'] <- df_all['treated_unit']*df_all['post_treat']

#groupby month and average
df_all_mos <- df_all %>% 
                mutate(month = lubridate::floor_date(date, 'month')) %>%
                group_by(item,month) %>%
                summarize(across(c('median_price','rolling_30day_sd','ewm_30day_sd','expanding_sd','total_sd','pre_sd','post_sd','volume_sold','rel_rarity','grade_rarity','l_median_price','rel_age_mos'),mean))
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

#plot monthly averages for dota and cs for one year 
#ggplot(NULL, aes(x=month, y=l_median_price)) + 
#  geom_point(data=df_cs_moavg_1yr, color="#EDA338") + 
#  geom_point(data=df_dota_moavg_1yr, color= "#FF0000") + 
#  geom_smooth(data=subset(df_cs_moavg_1yr, df_cs_moavg_1yr$month < as.Date("2018-03-29")), method='lm', color="blue") +
#  geom_smooth(data=subset(df_dota_moavg_1yr, df_dota_moavg_1yr$month < as.Date("2018-03-29")), method='lm', color="navyblue") +
#  geom_smooth(data=subset(df_cs_moavg_1yr, df_cs_moavg_1yr$month >= as.Date("2018-03-29")), method='lm', color="green") +
#  geom_smooth(data=subset(df_dota_moavg_1yr, df_dota_moavg_1yr$month >= as.Date("2018-03-29")), method='lm', color="green4") +
#  geom_vline(xintercept = as.Date("2018-03-29"),color="gray40",linetype="dashed")


# dota 2 trade ban happened May 25, 2020
#daily data weighted by volume sold (monthly data is already weighted by vol_sold and used for points) (USE THIS ONE IN PAPER!!)
#did_trends.png
p1 <- ggplot(NULL, aes(x=date, y=log(median_price))) + 
        geom_point(data=df_cs_moavg_1yr,aes(x=month, y=l_median_price, color="Counter Strike")) + 
        geom_point(data=df_dota_moavg_1yr,aes(x=month, y=l_median_price, color="Dota 2")) + 
        geom_vline(xintercept = as.Date("2018-03-29"),color="gray50",linetype="dashed") + 
        geom_smooth(data=subset(df_cs_1yr, df_cs_1yr$date < as.Date("2018-03-29")), method='lm',linetype="longdash",color="#EDA338",aes(weight = volume_sold)) +
        geom_smooth(data=subset(df_dota_1yr, df_dota_1yr$date < as.Date("2018-03-29")), method='lm',linetype="longdash",color="#FF0000",aes(weight = volume_sold)) +
        geom_smooth(data=subset(df_cs_1yr, df_cs_1yr$date >= as.Date("2018-03-29")), method='lm', color="#EDA338",linetype="longdash",aes(weight = volume_sold)) +
        geom_smooth(data=subset(df_dota_1yr, df_dota_1yr$date >= as.Date("2018-03-29")), method='lm', color="#FF0000",linetype="longdash",aes(weight = volume_sold)) +
        geom_label(aes(as.Date("2018-06-12"), 1.5), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
        scale_x_date(breaks = scales::pretty_breaks(n=8)) +
        scale_color_manual(NULL,values = c("Dota 2" = "#FF0000", "Counter Strike" = "#EDA338")) +
        xlab("") +
        ylab("Log Median Price")+
        theme(axis.title.x = element_blank()) + theme_few()
p1
ggsave( '../writing/manuscript/figures/did_trends.pdf', plot = p1)

#unweighted
#ggplot(NULL, aes(x=date, y=log(median_price))) +
#  #geom_point(data=df_cs, color="#EDA338") + 
#  #geom_point(data=df_dota, color= "#FF0000") + 
#  geom_vline(xintercept = as.Date("2018-03-29")) + 
#  geom_smooth(data=subset(df_cs, df_cs$date < as.Date("2018-03-29")), method='lm', color="blue") +
#  geom_smooth(data=subset(df_dota, df_dota$date < as.Date("2018-03-29")), method='lm', color="navyblue") +
#  geom_smooth(data=subset(df_cs, df_cs$date >= as.Date("2018-03-29")), method='lm', color="green") +
#  geom_smooth(data=subset(df_dota, df_dota$date >= as.Date("2018-03-29")), method='lm', color="green4")

#ggplot(NULL, aes(x=date, y=log(volume_sold))) + 
#  #geom_point(data=df_cs, color="#EDA338") + 
#  #geom_point(data=df_dota, color= "#FF0000") + 
#  geom_vline(xintercept = as.Date("2018-03-29")) + 
#  geom_smooth(data=subset(df_cs, date >= as.Date("2016-03-29") & date <= as.Date("2018-03-29")), method='lm', color="blue") +
#  geom_smooth(data=subset(df_dota, date >= as.Date("2016-03-29") & date <= as.Date("2018-03-29")), method='lm', color="navyblue") +
#  geom_smooth(data=subset(df_cs, date >= as.Date("2018-03-29") & date <= as.Date("2020-03-29")), method='lm', color="green") +
#  geom_smooth(data=subset(df_dota, date >= as.Date("2018-03-29") & date <= as.Date("2020-03-29")), method='lm', color="green4")

#stage 1: quality adjusted price estimation
#we tend to think of rel_age as increasing to reach an apex once decently rare, and then falling off to more aesthetically pleasing skins
p_reg <- feols(log(median_price) ~ rel_age_days + I(rel_age_days^2) + grade_rarity + I(grade_rarity^2), weights= ~volume_sold, data=df_all)
summary(p_reg)

p_reg_1yr <- feols(log(median_price) ~ rel_age_days + I(rel_age_days^2) + grade_rarity + I(grade_rarity^2), weights= ~volume_sold, data=df_all_1yr)
summary(p_reg_1yr)

p_reg_mos <- feols(log(median_price) ~ rel_age_mos + I(rel_age_mos^2) + grade_rarity + I(grade_rarity^2), weights= ~volume_sold, data=df_all_mos)
summary(p_reg_mos)

p_reg_mos_1yr <- feols(log(median_price) ~ rel_age_mos + I(rel_age_mos^2) + grade_rarity + I(grade_rarity^2), weights= ~volume_sold, data=df_all_mos_1yr)
summary(p_reg_mos_1yr)

#p_boot <- boottest(feols_fit, B = 9999, param = "treated_unit",clustid = c("item","date"))

df_all['log_p_hat'] <- predict(p_reg,df_all,interval="prediction")$fit
df_all_1yr['log_p_hat'] <- predict(p_reg_1yr,df_all_1yr,interval="prediction")$fit
df_all_mos['log_p_hat'] <- predict(p_reg_mos,df_all_mos,interval="prediction")$fit
df_all_mos_1yr['log_p_hat'] <- predict(p_reg_mos_1yr,df_all_mos_1yr,interval="prediction")$fit

#stage 2: did reg on quality adjusted price
#crude DiD -- daily
didreg_hat1 <- feols(log_p_hat ~ post_treat*treated_unit | item+date, weights= ~volume_sold, data=df_all)
summary(didreg_hat1)

didreg_1yr_hat1 <- feols(log_p_hat ~ post_treat*treated_unit | item+date, weights= ~volume_sold, data=df_all_1yr)
summary(didreg_1yr_hat1)

#crude DiD -- monthly (second one looks most promising)
didreg_mos_hat1 <- feols(log_p_hat ~ post_treat*treated_unit | item+month, weights= ~volume_sold, data=df_all_mos)
summary(didreg_mos_hat1)

didreg_mos_1yr_hat1 <- feols(log_p_hat ~ post_treat*treated_unit | item+month, weights= ~volume_sold, data=df_all_mos_1yr)
summary(didreg_mos_1yr_hat1)

#daily data event study
didreg_hat2 <- feols(log_p_hat ~ i(days_til_treat,treated_unit,ref=-1,keep=-100:100) | item+date, weights= ~volume_sold, data=df_all)
summary(didreg_hat2)
iplot(didreg_hat2, xlim=c(-50,50),xlab="Days Until Treatment",main="Effect on Fitted Log Median Price")

didreg_1yr_hat2 <- feols(log_p_hat ~ i(days_til_treat,treated_unit,ref=-1,keep=-100:100) | item+date, weights= ~volume_sold, data=df_all_1yr)
summary(didreg_1yr_hat2)
iplot(didreg_1yr_hat2, xlim=c(-50,50),xlab="Days Until Treatment",main="Effect on Fitted Log Median Price")

#monthly data event study -- WHY ARE THESE TWO OPPOSITE??
didreg_mos_hat2 <- feols(log_p_hat ~ i(mos_til_treat,treated_unit,ref=-1) | item+month, weights= ~volume_sold, data=df_all_mos)
summary(didreg_mos_hat2)
iplot(didreg_mos_hat2,xlab="Months Until Treatment",main="Effect on Fitted Log Median Price")

didreg_mos_1yr_hat2 <- feols(log_p_hat ~ i(mos_til_treat,treated_unit,ref=-1) | item+month, weights= ~volume_sold, data=df_all_mos_1yr)
summary(didreg_mos_1yr_hat2)
iplot(didreg_mos_1yr_hat2,xlab="Months Until Treatment",main="Effect on Fitted Log Median Price")

#EXTRAS
#raw price did regs
didreg1 <- feols(log(median_price) ~ post_treat*treated_unit |item+date, weights= ~volume_sold, data=df_all)
summary(didreg1)

didreg2 <- feols(log(median_price) ~ i(days_til_treat,treated_unit,ref=-1,keep=-100:100) | item+date, weights= ~volume_sold, data=df_all)
summary(didreg2)
iplot(didreg2, xlim=c(-50,50),xlab="Days Until Treatment",main="Effect on Log Median Price")

didreg3 <- feols(volume_sold ~ i(days_til_treat,treated_unit,ref=-1,keep=-100:100) | item+date, data=df_all)
summary(didreg3)
iplot(didreg3, xlim=c(-50,50),xlab="Days Until Treatment",main="Effect on Volume Sold")

didreg_mos1 <- feols(log(median_price) ~ post_treat*treated_unit | item+month, weights= ~volume_sold, data=df_all_mos)
summary(didreg_mos1)

didreg_mos2 <- feols(log(median_price) ~ i(mos_til_treat,treated_unit,ref=-1) | item+month, weights= ~volume_sold, data=df_all_mos)
summary(didreg_mos2)
iplot(didreg_mos2, xlab="Months Until Treatment",main="Effect on Log Median Price")

didreg_mos3 <- feols(volume_sold ~ i(mos_til_treat,treated_unit,ref=-1) | item+month, data=df_all_mos)
summary(didreg_mos3)
iplot(didreg_mos3, xlab="Months Until Treatment",main="Effect on Volume Sold")
