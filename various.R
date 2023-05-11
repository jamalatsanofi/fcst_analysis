# in percentage
sku_mape_volume <- df_extended %>% 
  filter(GBU == "GEM") %>% 
  filter(Date >= "2023-01-01") %>% 
  mutate(abs_err = abs(Final_fcst - Vol)) %>% 
  group_by(Country, GMID, P_Fam, Franchise) %>% 
  summarise(Vol = sum(Vol), abs_er = sum(abs_err)) %>% 
  arrange(-Vol) %>% 
  ungroup() %>% 
  mutate(Vol_perc = label_percent(accuracy=0.1, trim = FALSE)(Vol/sum(Vol) ), 
         Vol_cumul = label_percent(accuracy=0.1, trim = FALSE)(cumsum(Vol/sum(Vol))), 
         wMAPE = label_percent(accuracy=0.1, trim = FALSE)(abs_er/Vol))

# in number
sku_mape_volume <- df_extended %>% 
  filter(GBU == "GEM") %>% 
  filter(Date >= "2023-01-01") %>% 
  mutate(abs_err = abs(Final_fcst - Vol)) %>% 
  group_by(Country, GMID, P_Fam, Franchise) %>% 
  summarise(Vol = sum(Vol), abs_er = sum(abs_err)) %>% 
  arrange(-Vol) %>% 
  ungroup() %>% 
  mutate(Vol_perc = Vol/sum(Vol), 
         Vol_cumul = cumsum(Vol/sum(Vol)), 
         wMAPE = abs_er/Vol)

#define n percent
npercent <- .2
simulated_mape_improvement <- .2

top_n_percent_vol <- sku_mape_volume[sku_mape_volume$Vol_cumul < npercent, ]
bottom_n_percent_vol <- sku_mape_volume[sku_mape_volume$Vol_cumul >= npercent, ]

top_n_percent_vol <- top_n_percent_vol %>% 
  select("Country", "GMID", "P_Fam", "Franchise", "Vol", "abs_er")



bottom_n_percent_mape <- bottom_n_percent_vol %>% 
  select("Country", "GMID", "P_Fam", "Franchise", "Vol", "abs_er") %>% 
  filter(Vol > 0) %>% 
  summarise(Vol = sum(Vol), abs_er = sum(abs_er)) %>% 
  mutate(Country = "Various", GMID = "Various", P_Fam = "Various", Franchise = "Various", 
         .before = Vol)%>% 
  mutate(wMAPE = sum(abs_er)/sum(Vol)) %>% pull(wMAPE)

simulated_mape <- bottom_n_percent_mape * (1-simulated_mape_improvement)   #20% improvement

bottom_n_percent_vol <- bottom_n_percent_vol %>% 
  select("Country", "GMID", "P_Fam", "Franchise", "Vol", "abs_er") %>% 
  filter(Vol > 0) %>% 
  # mutate(abs_er = ifelse(abs_er < Vol*simulated_mape, abs_er, Vol*simulated_mape)) %>% 
  mutate(abs_er = Vol*simulated_mape) %>% 
  summarise(Vol = sum(Vol), abs_er = sum(abs_er)) %>% 
  mutate(Country = "Various", GMID = "Various", P_Fam = "Various", Franchise = "Various", 
         .before = Vol)


top_n_percent_mape <- top_n_percent_vol %>% 
  summarise(Vol = sum(Vol), abs_er = sum(abs_er)) %>% 
  mutate(wMAPE = sum(abs_er)/sum(Vol)) %>% pull(wMAPE)

bottom_n_percent_vol %>% 
  summarise(Vol = sum(Vol), abs_er = sum(abs_er)) %>% 
  mutate(wMAPE = sum(abs_er)/sum(Vol))


# bind top and bottom
rbind(top_n_percent_vol, bottom_n_percent_vol) %>% mutate(wMAPE = abs_er/Vol*100)

simulated_final_mape <- rbind(top_n_percent_vol, bottom_n_percent_vol) %>% 
  summarise(Vol = sum(Vol), abs_er = sum(abs_er)) %>% 
  mutate(wMAPE = sum(abs_er)/sum(Vol)) %>% 
  pull(wMAPE)

glue("Top {(npercent)*100}% Volume wMAPE: {round(top_n_percent_mape,3)*100}% | Bottom {(1-npercent)*100}% Volume wMAPE: {round(bottom_n_percent_mape,3)*100}%
     Should we improve {simulated_mape_improvement*100}% the Bottom volume (~{round(nrow(sku_mape_volume)/1000)}k SKUs) at {round(simulated_mape,3)*100}%, we would have a total wMAPE of {round(simulated_final_mape,3)*100}%")


top_40_sku <- head(sku_mape_volume, 2000) 

source("san_style.R")

vol_plot <- ggplot(top_40_sku, aes(x = as.numeric(row.names(top_40_sku)), y = Vol_cumul )) +
  geom_line(colour = "#333333", size = 2) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  #top 20%
  annotate("rect", xmin = 0, xmax = 8, ymin = 0, ymax = 0.193, fill = "#23004C", alpha = .8) +
  geom_label(aes(x = 1000, y = .1, label = "8 SKUs"), 
             hjust = 0, vjust = 0.5, colour = "#555555", fill = "white", label.size = NA, family="Sanofi Sans", size = 4) +
  geom_segment(aes(x = 8, y = .1, xend = 1000, yend = .1), colour = "#23004C", size=0.1) +
  #top 50%
  annotate("rect", xmin = 0, xmax = 112, ymin = 0, ymax = 0.499, fill = "#23004C", alpha = .4) +
  geom_label(aes(x = 1000, y = .25, label = "112 SKUs"), 
             hjust = 0, vjust = 0.5, colour = "#555555", fill = "white", label.size = NA, family="Sanofi Sans", size = 4) +
  geom_segment(aes(x = 112, y = .25, xend = 1000, yend = .25), colour = "#23004C", size=0.1) +
  #top 80%
  annotate("rect", xmin = 0, xmax = 672, ymin = 0, ymax = 0.8, fill = "#23004C", alpha = .2) + 
  geom_label(aes(x = 1000, y = .45, label = "672 SKUs"), 
             hjust = 0, vjust = 0.5, colour = "#555555", fill = "white", label.size = NA, family="Sanofi Sans", size = 4) +
  geom_segment(aes(x = 672, y = .4, xend = 1000, yend = .4), colour = "#23004C", size=0.1) +
  #remaining 20%
  annotate("rect", xmin = 672, xmax = Inf, ymin = .8, ymax = Inf, fill = "#CCCCCC", alpha = .4) + 
  geom_label(aes(x = 1500, y = .85, label = "13.500 SKUs"), 
             hjust = 0, vjust = 0.5, colour = "#555555", fill = "white", label.size = NA, family="Sanofi Sans", size = 4) +
  labs(title="GenMed Volume distribution by SKU",
       subtitle = "8 SKUs represent one fifth of total volume",
       caption = "YTD04 2023") +
  scale_y_continuous(breaks=c(.2,.5,.8),
                     labels = scales::label_percent()) +
  sanofi_style() 

  
# bbplot::finalise_plot(
#   plot_name = vol_plot,
#   source = "Source: SCeye",
#   save_filepath = "../vol_plot.png",
#   width_pixels = 640,
#   height_pixels = 450,
#   logo_image_path = "placeholder.png")



sku_mape_volume %>% head(40) %>% 
  ggplot() +
  geom_line(aes(x = as.numeric(row.names(sku_mape_volume)), y = Vol_cumul), stat = "identity") + 
  bbplot::bbc_style()

df_extended %>% 
  filter(GMID == "354895", Date >= "2023-01-01") %>% 
  select(Date, GMID, Vol, Final_fcst) %>% 
  mutate(abs_er = abs(Final_fcst - Vol), 
         wMAPE = abs_er/Vol) %>% 
  group_by(GMID) %>% 
  summarise(Vol = sum(Vol), Final_fcst = sum(Final_fcst), abs_er = sum(abs_er), wMAPE = abs_er/Vol)
