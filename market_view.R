# grouping by market ---------------------------------------------------------------

# Copy ray data from df_sku

market_view <- df_sku


# add enrichment volume




# Filter out where no stat forecast is available 
df <- df %>% filter(Vol_w_stat > 0) %>% 
  mutate(Performance = case_when(FVA_wMAPE_pp < -1 ~ "Deteriorating", FVA_wMAPE_pp > 1 ~ "Improving", .default = "Neutral"))

# On the full scope what is the improvement


# Filter markets representing top 90% of volume
df_top90 <- df %>% 
  arrange(-Vol_w_stat) %>% 
  mutate(Vol_w_stat_cum = cumsum(Vol_w_stat / sum(Vol_w_stat))) %>% 
  filter(lag(Vol_w_stat_cum, default = 0) < .9) %>% 
  select(!Vol_w_stat_cum) 



base_plot_market <- ggplot(df_top90, aes(x=reorder(Market, FVA_wMAPE_pp), y=FVA_wMAPE_pp, label=round(FVA_wMAPE_pp, digits = 1))) + 
  geom_segment(aes(y = 0,
                   x = reorder(Market, FVA_wMAPE_pp),
                   yend = FVA_wMAPE_pp,
                   xend = reorder(Market, FVA_wMAPE_pp)),
               color = "grey60",
               linetype="dashed") +
  geom_point(stat='identity', aes(col=Performance), size=8)  +
  scale_color_manual(name="FVA in pp",
                     values = c("Improving"="#7A00E6", "Deteriorating"="#ED6C4E")) +
  geom_text(color="white", size=3) +
  labs(title="FVA in wMAPE percentage point", 
       subtitle="Countries cumulating 90% of 0-touch Volume - Q1 2023") + 
  # ylim(-2.5, 2.5) +
  coord_flip()

base_plot_market

base_plot_market + 
  labs(
    x = NULL, 
    y = "Added Value of the Enrichment versus the Baseline in wMAPE pp", 
  ) 


