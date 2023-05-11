# keep the scope defined in variables
market_view <- df_extended %>% 
  filter(
    GBU %in% scope_gbu &
      Old_z_touch_segm %in% scope_categ) %>% 
  filter(!Country %in% nbm_markets) %>% 
  filter(Date >= scope_start_date)

# Before filtering out where no stat is available, some figures
# SKU
sku_in_scope <- market_view %>%
  distinct(Country, GMID) %>%
  nrow()
sku_with_stat <- market_view %>%
  filter(Vol_w_stat > 0 & Vol > 0) %>%
  distinct(Country, GMID) %>%
  nrow()
sku_stat_coverage <- round(sku_with_stat / sku_in_scope, 4)
# Volume
vol_with_stat <- market_view %>%
  select(Vol_w_stat) %>%
  sum()
vol_without_stat <- market_view %>%
  filter(Vol_w_stat == 0) %>%
  select(Vol) %>%
  sum()
vol_in_scope <- vol_with_stat + vol_without_stat
vol_stat_coverage <- round(vol_with_stat / vol_in_scope, 4)

# market_view %>% 
#   group_by(REGION, Old_z_touch_segm) %>% 
#   summarise("Total Volume" = sum(Vol), 
#             "Volume with baseline" = sum(Vol_w_stat)) %>% 
#   autoplot()

# Filter out where no stat forecast is available
market_view <- market_view %>% filter(Vol_w_stat > 0)

# remove direct Rupture and Recovery impact
market_view <- market_view %>% filter(MAPE_ExcludeRuptures == "No Impact")

# Add enrichment volume
market_view <- market_view %>% 
  mutate(Enrichment_vol = Final_fcst - Stat_fcst)

# Calculate adherence vol and val
adherence_sku <- round(nrow(market_view[market_view$Enrichment_vol == 0,]) / nrow(market_view),3)
# adherence_vol 


# add error data to calculate MAPE & SPA later
market_view <- market_view %>% mutate(er_stat  = Stat_fcst - Vol_w_stat, 
                                      er_final = Final_fcst - Vol_w_stat, 
                                      abs_er_stat = abs(er_stat), 
                                      abs_er_final = abs(er_final)
) 

# Add FVA in volume and performance
market_view <- market_view %>% mutate(FVA_Vol = abs_er_stat - abs_er_final,
                                      FVA_Perf = case_when(
                                        FVA_Vol > 0 ~ "Improving",
                                        FVA_Vol < 0 ~ "Worsening",
                                        T ~ "Neutral"
                                      ))

### Filter top n% of Vol ----
# Define n%
market_top_n_vol <- .8
# Countries representing top n% of volume
top_nperc_vol_market_list <- market_view %>%
  group_by(Country) %>%
  summarise(Vol_w_stat = sum(Vol_w_stat)) %>%
  arrange(-Vol_w_stat) %>%
  mutate(Vol_w_stat_cum = cumsum(Vol_w_stat / sum(Vol_w_stat))) %>%
  filter(lag(Vol_w_stat_cum, default = 0) < market_top_n_vol) %>%
  pull(Country)


# filter on topn% volume countries
market_view_top_nperc <- market_view %>% 
  filter(Country %in% top_nperc_vol_market_list) 


market_view_top_nperc_group <- market_view_top_nperc %>% 
  group_by(GBU, REGION, Country, Old_z_touch_segm)


# calculate the enrichment in volume (/!\, Vol_w_stat != Vol. to take into consideration if delta is high)
# check delta
market_view_top_nperc %>%
  mutate(Delta = Vol_w_stat - Vol) %>%
  arrange(Delta) %>%
  filter(Delta != 0)

### Prepare Plots ----
### Market view
market_nperc_fva_mape_base_plot <- market_view_top_nperc_group %>% 
  summarise(FVA_mape_pp = round(sum(abs_er_stat-abs_er_final)/sum(Vol_w_stat)*100,2),
            Vol_w_stat = sum(Vol_w_stat)) %>% 
  arrange(-FVA_mape_pp) %>% 
  mutate(Performance = case_when(
    FVA_mape_pp > 0.5 ~ "Improving",
    FVA_mape_pp < -0.5 ~ "Worsening",
    T ~ "Neutral"
  )) 

### Volume top_down view

net_fva <- market_view_top_nperc_group %>%
  mutate(FVA = "Net") %>%
  group_by(GBU, REGION, Country, Old_z_touch_segm, FVA) %>% 
  summarise(FVA_Vol = sum(FVA_Vol))

abs_fva <- market_view_top_nperc_group %>%
  mutate(FVA = "Abs") %>%
  group_by(GBU, REGION, Country, Old_z_touch_segm, FVA) %>% 
  summarise(FVA_Vol = sum(abs(FVA_Vol)))

imp_fva <- market_view_top_nperc_group %>%
  filter(FVA_Perf == "Improving") %>% 
  mutate(FVA = "Imp") %>%
  group_by(GBU, REGION, Country, Old_z_touch_segm, FVA) %>% 
  summarise(FVA_Vol = sum(FVA_Vol))

wor_fva <- market_view_top_nperc_group %>%
  filter(FVA_Perf == "Worsening") %>% 
  mutate(FVA = "Wor") %>%
  group_by(GBU, REGION, Country, Old_z_touch_segm, FVA) %>% 
  summarise(FVA_Vol = sum(FVA_Vol))

market_fva <- bind_rows(net_fva, imp_fva, wor_fva)

#### prepare Legend
# Am I changing years?
dates_in_same_year <- year(max(market_view_top_nperc_group$Date)) - year(min(market_view_top_nperc_group$Date)) == 0
ifelse(dates_in_same_year, 
       start_date <- format(min(market_view_top_nperc_group$Date), "%B"),
       start_date <- format(min(market_view_top_nperc_group$Date), "%B %Y")
)
end_date <- format(max(market_view_top_nperc_group$Date), "%B %Y")
# legend
legend_date_scope <- paste(start_date, "to", end_date) 

# filter out some countries 
market_nperc_fva_mape_base_plot_filtered <- market_nperc_fva_mape_base_plot %>%  
  filter(Country != special_country_filter)
