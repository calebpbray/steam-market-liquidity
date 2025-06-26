library(ggplot2)

df_cs <- read.csv("../data/cs/cs_combined_items_price_history.csv")
df_dota <- read.csv("../data/dota2/dota2_combined_items_price_history.csv")

df_cs['r_date'] <- as.Date(df_cs$date, format = "%b %d %Y")
df_dota['r_date'] <- as.Date(df_dota$date, format = "%b %d %Y")

# dota 2 trade ban happened May 25, 2020
ggplot(NULL, aes(x=r_date, y=log(median_price))) + 
  geom_point(data=df_cs, color="#EDA338") + 
  geom_point(data=df_dota, color= "#FF0000") + 
  geom_vline(xintercept = as.Date("2018-03-29")) + 
  geom_smooth(data=subset(df_cs, r_date >= as.Date("2016-03-29") & r_date <= as.Date("2018-03-29")), method='lm', color="blue") +
  geom_smooth(data=subset(df_dota, r_date >= as.Date("2016-03-29") & r_date <= as.Date("2018-03-29")), method='lm', color="navyblue") +
  geom_smooth(data=subset(df_cs, r_date >= as.Date("2018-03-29") & r_date <= as.Date("2020-03-29")), method='lm', color="green") +
  geom_smooth(data=subset(df_dota, r_date >= as.Date("2018-03-29") & r_date <= as.Date("2020-03-29")), method='lm', color="green4")

ggplot(NULL, aes(x=r_date, y=log(volume_sold))) + 
  geom_point(data=df_cs, color="#EDA338") + 
  geom_point(data=df_dota, color= "#FF0000") + 
  geom_vline(xintercept = as.Date("2018-03-29")) + 
  geom_smooth(data=subset(df_cs, r_date >= as.Date("2016-03-29") & r_date <= as.Date("2018-03-29")), method='lm', color="blue") +
  geom_smooth(data=subset(df_dota, r_date >= as.Date("2016-03-29") & r_date <= as.Date("2018-03-29")), method='lm', color="navyblue") +
  geom_smooth(data=subset(df_cs, r_date >= as.Date("2018-03-29") & r_date <= as.Date("2020-03-29")), method='lm', color="green") +
  geom_smooth(data=subset(df_dota, r_date >= as.Date("2018-03-29") & r_date <= as.Date("2020-03-29")), method='lm', color="green4")

