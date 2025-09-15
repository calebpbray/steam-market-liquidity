library(ggplot2) #plotting
library(fixest) #feols
library(dplyr) #mutate
library(lubridate) #floor_date
library(zoo) #moving averages, rollmean
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

#keep data within 1 years of treatment
df_cs <- df_cs_raw[df_cs_raw$date >= as.Date("2017-03-29") & df_cs_raw$date <= as.Date("2019-03-29"), ]
df_dota <- df_dota_raw[df_dota_raw$date >= as.Date("2017-03-29") & df_dota_raw$date <= as.Date("2019-03-29"), ]
df_all <- df_all_raw[df_all_raw$date >= as.Date("2017-03-29") & df_all_raw$date <= as.Date("2019-03-29"), ]

df_dc <- df_dota[grepl("Dragonclaw Hook",df_dota$item),]

#get monthly avgs
df_dota_moavg <- df_dota %>% 
  mutate(month = lubridate::floor_date(date, 'month')) %>%
  group_by(month) %>%
  summarize(l_median_price = weighted.mean(l_median_price, volume_sold),
            volume_sold = mean(volume_sold)
            )

df_dc_moavg <- df_dc %>% 
  mutate(month = lubridate::floor_date(date, 'month')) %>%
  group_by(month) %>%
  summarize(l_median_price = weighted.mean(l_median_price, volume_sold),
            volume_sold = mean(volume_sold)
            )

#daily avg
df_dota_dlyavg <- df_dota %>% 
  group_by(date) %>%
  summarize(l_median_price = weighted.mean(l_median_price, volume_sold),
            volume_sold = mean(volume_sold)
  )

## MONTHLY
#PRICE
plot1 <-  ggplot(NULL, aes(x=month)) + 
  geom_point(data=df_dc_moavg, aes(y=l_median_price,color="Dragonclaw Hook"),show.legend=TRUE) +
  geom_line(data=df_dc_moavg, aes(y=l_median_price,color="Dragonclaw Hook")) +
  #geom_point(data=df_dota_moavg, aes(y=l_median_price,color="Dota 2")) +
  scale_y_continuous("Log Median Price",labels = scales::comma) +
  scale_x_date(breaks = scales::pretty_breaks(n=10)) +
  scale_color_manual(NULL, name = "Legend", values = c("Dragonclaw Hook" = "black")) +
  geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed",color="gray50") + 
  geom_label(aes(as.Date("2018-05-15"), 5.5), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
  annotate(geom="text", label="Dragonclaw Hook", x=as.Date("2017-01-15"), y=6.7, vjust="top",hjust="left", color="black",fontface =2) +
  guides(color = FALSE, size = FALSE) +
  xlab("") +
  theme(axis.title.x = element_blank()) + theme_few()
plot1

plot2 <-  ggplot(NULL, aes(x=month)) + 
  #geom_point(data=df_dc_moavg, aes(y=l_median_price,color="DC")) +
  geom_point(data=df_dota_moavg, aes(y=l_median_price,color="All Dota 2 Items"),show.legend=TRUE) +
  geom_line(data=df_dota_moavg, aes(y=l_median_price,color="All Dota 2 Items")) +
  scale_y_continuous("Log Median Price",labels = scales::comma) +
  scale_x_date(breaks = scales::pretty_breaks(n=10)) +
  scale_color_manual(NULL,values = c("All Dota 2 Items" = "#FF0000")) +
  geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed",color="gray50") + 
  geom_label(aes(as.Date("2018-05-15"), -0.2), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
  annotate(geom="text", label="All Dota 2 Items", x=as.Date("2017-01-15"), y=0.3, vjust="top",hjust="left", color="#FF0000",fontface =2) +
  guides(color = FALSE, size = FALSE) +
  xlab("") +
  theme(axis.title.x = element_blank()) + theme_few()
plot2

combined <- plot1 / plot2 + plot_layout(guides = "collect")
combined
ggsave("../writing/manuscript/figures/dragonclaw_hook.pdf",plot=combined)


#VOL SOLD
plot3 <-  ggplot(NULL, aes(x=month)) + 
  geom_point(data=df_dc_moavg, aes(y=volume_sold,color="DC")) +
  geom_line(data=df_dc_moavg, aes(y=volume_sold,color="DC")) +
  #geom_point(data=df_dota_moavg, aes(y=l_median_price,color="Dota 2")) +
  scale_y_continuous("Volume Sold",labels = scales::comma) +
  scale_x_date(breaks = scales::pretty_breaks(n=10)) +
  scale_color_manual(name = "Legend", values = c("DC" = "black", "Dota 2" = "#FF0000")) +
  geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed",color="gray50") + 
  #geom_label(aes(as.Date("2019-04-01"), 4100000), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
  guides(color = FALSE, size = FALSE) +
  xlab("") +
  theme(axis.title.x = element_blank()) + theme_few()
plot3

plot4 <-  ggplot(NULL, aes(x=month)) + 
  #geom_point(data=df_dc_moavg, aes(y=l_median_price,color="DC")) +
  geom_point(data=df_dota_moavg, aes(y=volume_sold,color="Dota 2")) +
  geom_line(data=df_dota_moavg, aes(y=volume_sold,color="Dota 2")) +
  scale_y_continuous("Volume Sold",labels = scales::comma) +
  scale_x_date(breaks = scales::pretty_breaks(n=10)) +
  scale_color_manual(name = "Legend", values = c("DC" = "black", "Dota 2" = "#FF0000")) +
  geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed",color="gray50") + 
  #geom_label(aes(as.Date("2019-04-01"), 4100000), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
  guides(color = FALSE, size = FALSE) +
  xlab("") +
  theme(axis.title.x = element_blank()) + theme_few()
plot4

combined2 <- plot3 / plot4 + plot_layout(axis_titles = "collect")
combined2

## DAILY --  MEH CHARTS
#PRICE
plot1 <-  ggplot(NULL, aes(x=date)) + 
  geom_point(data=df_dc, aes(y=l_median_price,color="DC")) +
  #geom_point(data=df_dota_moavg, aes(y=l_median_price,color="Dota 2")) +
  scale_y_continuous("Log Median Price",labels = scales::comma) +
  scale_x_date() +
  scale_color_manual(name = "Legend", values = c("DC" = "black", "Dota 2" = "#FF0000")) +
  geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed",color="gray50") + 
  geom_label(aes(as.Date("2018-05-15"), 5.5), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
  guides(color = FALSE, size = FALSE) +
  xlab("") +
  theme(axis.title.x = element_blank()) + theme_few()
plot1

plot2 <-  ggplot(NULL, aes(x=date)) + 
  #geom_point(data=df_dc, aes(y=l_median_price,color="DC")) +
  geom_point(data=df_dota_dlyavg, aes(y=l_median_price,color="Dota 2")) +
  scale_y_continuous("Log Median Price",labels = scales::comma) +
  scale_x_date(breaks = scales::pretty_breaks(n=10)) +
  scale_color_manual(name = "Legend", values = c("DC" = "black", "Dota 2" = "#FF0000")) +
  geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed",color="gray50") + 
  geom_label(aes(as.Date("2018-05-15"), -0.2), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
  guides(color = FALSE, size = FALSE) +
  xlab("") +
  theme(axis.title.x = element_blank()) + theme_few()
plot2

combined <- plot1 / plot2 + plot_layout(axis_titles = "collect")
combined


#VOL SOLD
plot3 <-  ggplot(NULL, aes(x=month)) + 
  geom_point(data=df_dc_moavg, aes(y=volume_sold,color="DC")) +
  #geom_point(data=df_dota_moavg, aes(y=l_median_price,color="Dota 2")) +
  scale_y_continuous("Volume Sold",labels = scales::comma) +
  scale_x_date(breaks = scales::pretty_breaks(n=10)) +
  scale_color_manual(name = "Legend", values = c("DC" = "black", "Dota 2" = "#FF0000")) +
  geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed",color="gray50") + 
  #geom_label(aes(as.Date("2019-04-01"), 4100000), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
  guides(color = FALSE, size = FALSE) +
  xlab("") +
  theme(axis.title.x = element_blank()) + theme_few()
plot3

plot4 <-  ggplot(NULL, aes(x=month)) + 
  #geom_point(data=df_dc_moavg, aes(y=l_median_price,color="DC")) +
  geom_point(data=df_dota_moavg, aes(y=volume_sold,color="Dota 2")) +
  scale_y_continuous("Volume Sold",labels = scales::comma) +
  scale_x_date(breaks = scales::pretty_breaks(n=10)) +
  scale_color_manual(name = "Legend", values = c("DC" = "black", "Dota 2" = "#FF0000")) +
  geom_vline(xintercept = as.Date("2018-03-29"), linetype = "dashed",color="gray50") + 
  #geom_label(aes(as.Date("2019-04-01"), 4100000), label = "March 29th, 2018", color="gray50",show.legend = FALSE,label.size = NA) +
  guides(color = FALSE, size = FALSE) +
  xlab("") +
  theme(axis.title.x = element_blank()) + theme_few()
plot4

combined2 <- plot3 / plot4 + plot_layout(axis_titles = "collect")
combined2