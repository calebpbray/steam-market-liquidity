library(ggplot2)
library(fixest)
library(dplyr)
library(lubridate)

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

#WIP -- TRY TIME SERIES EVENT STUDY
#maybe look into aggregating to monthly?
#https://libguides.princeton.edu/R-Timeseries
#https://cran.r-project.org/web/packages/ggfixest/vignettes/ggiplot.html
#https://asjadnaqvi.github.io/DiD/docs/code_r/07-twfe_r/#r-code

df_cs['D'] <- df_cs['treated_unit']*df_cs['post_treat']
df_dota['D'] <- df_dota['treated_unit']*df_dota['post_treat']
df_all['D'] <- df_all['treated_unit']*df_all['post_treat']

#groupby month and average
df_all_mos <- df_all %>% 
                mutate(month = lubridate::floor_date(date, 'month')) %>%
                group_by(item,month) %>%
                summarize(across(c('median_price','rolling_30day_sd','ewm_30day_sd','expanding_sd','total_sd','pre_sd','post_sd','volume_sold','rel_rarity','l_median_price'),mean))
df_all_mos['mos_til_treat'] <- interval(as.Date('2018-03-01'),df_all_mos$month) %/% months(1)
df_all_mos['post_treat'] <- ifelse(df_all_mos$mos_til_treat >= 0,1,0)
df_all_mos['treated_unit'] <- 0 #assign all values to 0
df_all_mos[grepl("\\|", df_all_mos$item),'treated_unit'] <- 1 #assign 1 to any rows where item has a "|" as that indicates a CSGO item


# dota 2 trade ban happened May 25, 2020
#weighted by volume sold
ggplot(NULL, aes(x=date, y=log(median_price))) + 
  #geom_point(data=df_cs, color="#EDA338") + 
  #geom_point(data=df_dota, color= "#FF0000") + 
  geom_vline(xintercept = as.Date("2018-03-29")) + 
  geom_smooth(data=subset(df_cs, df_cs$date < as.Date("2018-03-29")), method='lm', color="blue",aes(weight = volume_sold)) +
  geom_smooth(data=subset(df_dota, df_dota$date < as.Date("2018-03-29")), method='lm', color="navyblue",aes(weight = volume_sold)) +
  geom_smooth(data=subset(df_cs, df_cs$date >= as.Date("2018-03-29")), method='lm', color="green",aes(weight = volume_sold)) +
  geom_smooth(data=subset(df_dota, df_dota$date >= as.Date("2018-03-29")), method='lm', color="green4",aes(weight = volume_sold))

#unweighted
ggplot(NULL, aes(x=date, y=log(median_price))) + 
  #geom_point(data=df_cs, color="#EDA338") + 
  #geom_point(data=df_dota, color= "#FF0000") + 
  geom_vline(xintercept = as.Date("2018-03-29")) + 
  geom_smooth(data=subset(df_cs, df_cs$date < as.Date("2018-03-29")), method='lm', color="blue") +
  geom_smooth(data=subset(df_dota, df_dota$date < as.Date("2018-03-29")), method='lm', color="navyblue") +
  geom_smooth(data=subset(df_cs, df_cs$date >= as.Date("2018-03-29")), method='lm', color="green") +
  geom_smooth(data=subset(df_dota, df_dota$date >= as.Date("2018-03-29")), method='lm', color="green4")

ggplot(NULL, aes(x=date, y=log(volume_sold))) + 
  #geom_point(data=df_cs, color="#EDA338") + 
  #geom_point(data=df_dota, color= "#FF0000") + 
  geom_vline(xintercept = as.Date("2018-03-29")) + 
  geom_smooth(data=subset(df_cs, date >= as.Date("2016-03-29") & date <= as.Date("2018-03-29")), method='lm', color="blue") +
  geom_smooth(data=subset(df_dota, date >= as.Date("2016-03-29") & date <= as.Date("2018-03-29")), method='lm', color="navyblue") +
  geom_smooth(data=subset(df_cs, date >= as.Date("2018-03-29") & date <= as.Date("2020-03-29")), method='lm', color="green") +
  geom_smooth(data=subset(df_dota, date >= as.Date("2018-03-29") & date <= as.Date("2020-03-29")), method='lm', color="green4")

didreg1 <- feols(log(median_price) ~ post_treat*treated_unit |item+date, weights= ~volume_sold, data=df_all)
summary(didreg1)

didreg2 <- feols(log(median_price) ~ i(days_til_treat,treated_unit,ref=-1,keep=-100:100) | item+date, weights= ~volume_sold, data=df_all)
summary(didreg2)
iplot(didreg2, xlim=c(-50,50),xlab="Days Until Treatment",main="Effect on Log Median Price")

didreg3 <- feols(volume_sold ~ i(days_til_treat,treated_unit,ref=-1,keep=-100:100) | item+date, data=df_all)
summary(didreg3)
iplot(didreg3, xlim=c(-50,50),xlab="Days Until Treatment",main="Effect on Volume Sold")

didreg_mos <- feols(log(median_price) ~ i(mos_til_treat,treated_unit,ref=-1) | item+month, weights= ~volume_sold, data=df_all_mos)
summary(didreg_mos)
iplot(didreg_mos, xlab="Months Until Treatment",main="Effect on Log Median Price")

didreg_mos2 <- feols(volume_sold ~ i(mos_til_treat,treated_unit,ref=-1) | item+month, data=df_all_mos)
summary(didreg_mos2)
iplot(didreg_mos2, xlab="Months Until Treatment",main="Effect on Volume Sold")

didreg3 <- feols(log(median_price) ~ post_treat*treated_unit*(grade_rarity+rel_age_mos+pre_sd) | item+mo_yr, weights= ~volume_sold, data=df_all)
summary(didreg3)

didreg4 <- feols(log(volume_sold)~ post_treat + treated_unit + post_treat*treated_unit*(rel_rarity+rel_age_mos+pre_sd) | item, data=df_all)
summary(didreg4)