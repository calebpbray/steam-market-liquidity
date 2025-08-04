library(ggplot2)
library(fixest)

df_cs_raw <- read.csv("../data/cs_combined_items_price_history.csv")
df_dota_raw <- read.csv("../data/dota2_combined_items_price_history.csv")
df_all_raw <- read.csv("../data/combined_items_price_history.csv")

df_cs_raw['r_date'] <- as.Date(df_cs_raw$date, format = "%b %d %Y")
df_dota_raw['r_date'] <- as.Date(df_dota_raw$date, format = "%b %d %Y")
df_all_raw['r_date'] <- as.Date(df_all_raw$date, format = "%b %d %Y")

#keep data within 2 years of treatment
df_cs <- df_cs_raw[df_cs_raw$r_date >= as.Date("2016-03-29") & df_cs_raw$r_date <= as.Date("2020-03-29"), ]
df_dota <- df_dota_raw[df_dota_raw$r_date >= as.Date("2016-03-29") & df_dota_raw$r_date <= as.Date("2020-03-29"), ]
df_all <- df_all_raw[df_all_raw$r_date >= as.Date("2016-03-29") & df_all_raw$r_date <= as.Date("2020-03-29"), ]


# dota 2 trade ban happened May 25, 2020
ggplot(NULL, aes(x=r_date, y=log(median_price))) + 
  geom_point(data=df_cs, color="#EDA338") + 
  geom_point(data=df_dota, color= "#FF0000") + 
  geom_vline(xintercept = as.Date("2018-03-29")) + 
  geom_smooth(data=subset(df_cs, df_cs$r_date < as.Date("2018-03-29")), method='lm', color="blue") +
  geom_smooth(data=subset(df_dota, df_dota$r_date < as.Date("2018-03-29")), method='lm', color="navyblue") +
  geom_smooth(data=subset(df_cs, df_cs$r_date >= as.Date("2018-03-29")), method='lm', color="green") +
  geom_smooth(data=subset(df_dota, df_dota$r_date >= as.Date("2018-03-29")), method='lm', color="green4")

#ggplot(NULL, aes(x=r_date, y=log(volume_sold))) + 
  #geom_point(data=df_cs, color="#EDA338") + 
  #geom_point(data=df_dota, color= "#FF0000") + 
#  geom_vline(xintercept = as.Date("2018-03-29")) + 
#  geom_smooth(data=subset(df_cs, r_date >= as.Date("2016-03-29") & r_date <= as.Date("2018-03-29")), method='lm', color="blue") +
#  geom_smooth(data=subset(df_dota, r_date >= as.Date("2016-03-29") & r_date <= as.Date("2018-03-29")), method='lm', color="navyblue") +
#  geom_smooth(data=subset(df_cs, r_date >= as.Date("2018-03-29") & r_date <= as.Date("2020-03-29")), method='lm', color="green") +
#  geom_smooth(data=subset(df_dota, r_date >= as.Date("2018-03-29") & r_date <= as.Date("2020-03-29")), method='lm', color="green4")

didreg1 <- feols(log(median_price) ~ post_treat + treated_unit + post_treat*treated_unit, data=df_all)
summary(didreg1)

didreg2 <- feols(log(median_price) ~ post_treat + treated_unit + post_treat*treated_unit*(rolling_30day_sd+volume_sold)| item, data=df_all)
summary(didreg2)