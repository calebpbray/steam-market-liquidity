library(ggplot2) #plotting
library(fixest) #feols
library(dplyr) #mutate
library(lubridate) #floor_date
library(zoo) #moving averages, rollmean
library(scales) #editing y axes
library(patchwork) #arranging multiple plots side by side
library(latex2exp) #LaTeX expressions
library(ggthemes) #ggplot themes

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

#weekly
ggplot(data=df_trade_delta, aes(x=date, y=trade_offer_delta)) + 
  geom_line()+
  geom_vline(xintercept = as.Date("2018-03-29"))

#monthly -- FIX THE AXIS STUFF
plot1 <-  ggplot(df_master, aes(x=month)) + 
  geom_line(linewidth = 1.25,aes(y=trade_offer_delta,color="Trade Offer Delta"))+
  scale_y_continuous("Avg. Weekly Trade Offers",labels = scales::comma) +
  scale_x_date(breaks = scales::pretty_breaks(n=10)) +
  scale_color_manual(name = "Legend", values = c("Trade Offer Delta" = "black")) +
  geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed",color="gray50") + 
  geom_label(aes(as.Date("2019-04-01"), 4100000), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
  guides(color = FALSE, size = FALSE) +
  xlab("") +
  theme(axis.title.x = element_blank()) + theme_few()

plot2 <-  ggplot(df_master, aes(x=month)) + 
  geom_line(linewidth = 1.25, aes(y=dota_avg_players,color="Dota 2"))+
  geom_line(linewidth = 1.25, aes(y=cs_avg_players,color="Counter Strike"))+
  scale_y_continuous("Avg. Concurrent Player Count",labels = scales::comma) +
  scale_x_date(breaks = scales::pretty_breaks(n=10)) +
  scale_color_manual(NULL,values = c("Dota 2" = "#FF0000", "Counter Strike" = "#EDA338")) +
  geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed",color="gray50") + 
  geom_label(aes(as.Date("2019-04-01"), 1200000), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
  xlab("") +
  theme(axis.title.x = element_blank()) + theme_few()

combined <- plot1 / plot2 + plot_layout(axis_titles = "collect")
combined

ggsave( '../writing/manuscript/figures/trade_delta_and_plyr_ct.pdf', plot = combined)

#3mma
# plot_3mma1 <-  ggplot(df_master, aes(x=month)) + 
#   geom_line(aes(y=trade_delta_3mma,color="Trade Offer Delta"))+
#   scale_y_continuous(TeX("Trade Offer $\\Delta$, 3mma"),labels = scales::comma) +
#   scale_color_manual(name = "Legend", values = c("Trade Offer Delta" = "black")) +
#   geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed") + 
#   geom_label(aes(as.Date("2019-04-01"), 3200000), label = "March 29th, 2018", show.legend = FALSE,label.size = NA)+
#   guides(color = FALSE, size = FALSE)+
#   xlab("")+
#   theme(axis.title.x = element_blank()) + theme_few()
# 
# plot_3mma2 <-  ggplot(df_master, aes(x=month)) + 
#   geom_line(aes(y=dota_avg_players_3mma,color="DOTA 2"))+
#   geom_line(aes(y=cs_avg_players_3mma,color="Counter Strike"))+
#   scale_y_continuous("Avg. Player Count, 3mma",labels = scales::comma) +
#   scale_color_manual(NULL,values = c("DOTA 2" = "#FF0000", "Counter Strike" = "#EDA338")) +
#   geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed") + 
#   geom_label(aes(as.Date("2019-04-01"), 1200000), label = "March 29th, 2018", show.legend = FALSE,label.size = NA)+
#   xlab("")+
#   theme(axis.title.x = element_blank()) + theme_few()
# 
# combined_3mma <- plot_3mma1 / plot_3mma2 + plot_layout(axis_titles = "collect")
# combined_3mma

