# Dataset extension ----
# after loading df_sku extracted from SCEye, adds additional info


# Over/Under forecasting. On final forecast vs sales in volume
df_extended <- df_sku %>%
  mutate(Bias = case_when(
    Vol / Final_fcst < (1 - bias_threshold) ~ "Overforecast",
    Vol / Final_fcst > (1 + bias_threshold) ~ "Underforecast",
    TRUE ~ "Unbiased"
  )) %>% 
  # add some info on enrichment Direction and Size
  mutate(Enrichment_Direction = case_when(
    Stat_fcst == 0           ~ "No Baseline",
    Final_fcst > Stat_fcst   ~ "Up",
    Final_fcst < Final_fcst  ~ "Down",
    T ~ "None")) %>% 
  mutate(Enrichment_Size = case_when(
    abs((Final_fcst - Stat_fcst)/Stat_fcst) > 0.15 ~ "High",
    abs((Final_fcst - Stat_fcst)/Stat_fcst) > 0.07 ~ "Med",
    abs((Final_fcst - Stat_fcst)/Stat_fcst) > 0.02 ~ "Low",
    abs((Final_fcst - Stat_fcst)/Stat_fcst) > 0.00 ~ "Insignificant",
    T ~ "Error"
  )) 
