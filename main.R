# Description -------------------------------------------------------------
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# 
# 
# 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# Libraries and source files ----------------------------------------------

library(ggplot2)
library(patchwork)
library(tidyverse)
library(readxl)
library(scales)
library(glue)
library(extrafont)
library(ggh4x)
library(ggallin)   # to plot negative values on log scale
loadfonts(device = "win", quiet = TRUE)

source("functions/plot_functions.R")
source("functions/wrangling_functions.R")


# Variables ---------------------------------------------------------------

Sys.setlocale("LC_TIME", "English")
scope_categ <- c("Zero Touch") # c("Enrichment", "Zero Touch")
scope_gbu <- c("GEM") # c("GEM", "SPC", "CHC")
bias_threshold <- .1
scope_start_date <- as.Date("2022-07-01")
special_country_filter <- c("Saudi Arabia")

# Distribution models
## Note: might need to quantify the volume in distribution model
nbm_markets <- c("Afghanistan", "Angola", "Armenia", "Azerbaijan", "Bangladesh",
  "Belarus", "Bosnia", "Cambodia", "Central America",
  "Chile", "CIS Countries CHC", "Ecuador", "Estonia",
  "Georgia", "Kazakhstan", "Kyrgyzstan", "Latvia",
  "Lithuania", "Macedonia", "Malta", "Moldova", "NBM Eurasia",
  "NBM Europe", "Nigeria", "Pakistan", "Paraguay",
  "Sub Sahara Africa", "Tajikistan", "Turkmenistan",
  "Uruguay", "Uzbekistan")

top10_markets <- c("United States", "Germany", "France", "United Kingdom", "Italy", 
            "Spain", "China", "Japan", "Russia", "Brazil")

# top20_products

# GEM KEY MARKETS
gem_key_markets <- c(
    "France", "Italy", "Spain", "Portugal",
    "Germany", "Austria", "Switzerland",
    "United Kingdom", "Ireland",
    "Brazil", "Russia", "Algeria",
    "Turkey",
    "United Arab Emirates", "Yemen", "Oman", "Saudi Arabia", "Qatar", "Kuwait", "Bahrain"
  )


# Data loading and cleansing ----------------------------------------------
fun_open_sceye_extract()
fun_reload_sceye_extract()


# Dataset extension ----

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

# Forecast hits -----------------------------------------------------------

## Data preparation (spa_hits) ----
spa_hits <- df_extended %>%
  group_by(Country, GMID, Date, Asset, REGION, GBU, Franchise) %>%
  filter(
    GBU == "GEM",
    REGION == "EUROPE",
    Country %in% c("France", "Germany", "Italy", "Spain", "United Kingdom"),
    MAPE_ExcludeRuptures == "No Impact"
  ) %>%
  summarise(SPA = sum(Vol) / sum(Final_fcst)) %>%
  filter(!is.na(SPA) & !is.infinite(SPA))

## Forecast hits Plot ----
spa_hits %>% 
  ggplot(aes(x = SPA, fill = case_when(GBU == "SPC" ~ Franchise, GBU == "GEM" ~ Asset))) +
  geom_vline(aes(xintercept = 1),
             color = "grey80"
  ) +
  geom_histogram(
    position = "identity",
    alpha = 0.2, binwidth = 0.05
  ) +
  coord_cartesian(xlim = c(-.1, 2.1)) +
  theme_light() +
  stat_theodensity(aes(
    y = stat(count) * 0.1,
    color = case_when(GBU == "SPC" ~ Franchise, GBU == "GEM" ~ Asset)
  )) +
  facet_wrap(. ~ Country, scales = "free") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey90"),
    strip.background = element_rect(fill = NA, color = "white"),
    strip.text = element_text(face = "bold", colour = "#23004C"),
    legend.box = "horizontal",
    legend.position = "bottom"
  ) +
  ggtitle("Distribution of Forecast hits SPA", "GenMed Oct22 to Mar23") +
  ylab("Frequency of hit") +
  xlab("SPA hit") +
  labs(fill = NULL, color = NULL)
  # guides(fill=guide_legend(title="Segment"))
  # bbc_style()



# Market view - FVA in Vol and wMAPE --------------------------------------

## Data Preparation (market_view) ----
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

market_view %>% 
  group_by(REGION, Old_z_touch_segm) %>% 
  summarise("Total Volume" = sum(Vol), 
            "Volume with baseline" = sum(Vol_w_stat)) %>% 
  autoplot()

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

market_fva <- bind_rows(abs_fva, imp_fva, wor_fva)


market_fva <- market_fva %>% 
  left_join(net_fva,
            by = c("GBU", "REGION", "Country", "Old_z_touch_segm"),
            suffix = c("", ".y")) %>% 
  select(!FVA.y) %>% 
  rename(FVA_NetVol = FVA_Vol.y) 


## Market View Plot ----
### FVA wMAPE lollipop plot ----

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

###
# Remove KSA (need to check tender topic)
###
market_nperc_fva_mape_base_plot_filtered <- market_nperc_fva_mape_base_plot %>%  
  filter(Country != special_country_filter)

(p1 <- market_nperc_fva_mape_base_plot_filtered %>% 
    ggplot(aes(x=reorder(Country, -FVA_mape_pp), 
               y=FVA_mape_pp, 
               label=round(FVA_mape_pp, digits = 1))) + 
  geom_segment(aes(y = 0,
                   x = reorder(Country, FVA_mape_pp),
                   yend = FVA_mape_pp,
                   xend = reorder(Country, FVA_mape_pp)),
               color = "grey60",
               linewidth=.5) +
  geom_hline(yintercept = 0, color = "gray90") +
  geom_point(stat='identity', aes(col=Performance), size=10) +
  scale_color_manual(name="FVA Performance",
                     values = c("Improving"="#7A00E6", 
                                "Worsening"="#ED6C4E", 
                                "Neutral"  ="grey60")) +
  geom_text(color="white", size=4) +
  coord_flip() + 
  labs(
  title="FVA in wMAPE percentage point",
  #      subtitle="Countries cumulating 90% of 0-touch Volume",
  #      caption = legend_date_scope) + 
    x = NULL, 
    y = NULL, 
  ) +
  theme_minimal()+
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray60", 
                                      linewidth = .2, 
                                      linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face="bold", color="#555555", size=14)
  ) 
)

(p2 <- market_nperc_fva_mape_base_plot_filtered %>% 
  ggplot(aes(x=reorder(Country, FVA_mape_pp), y=Vol_w_stat)) + 
    geom_col(fill = "gray70") +
    geom_label(aes(label = format(round(Vol_w_stat/10^6,1))), 
               hjust = "inward", 
               size=4) +
    geom_hline(yintercept = 0, color = "gray70") +
    coord_flip() + 
    labs(
      # title="FVA in wMAPE percentage point",
      # subtitle="Countries cumulating 90% of 0-touch Volume",
      # caption = legend_date_scope,
      x = NULL,
      y = "Volume (M Units)"
      ) +
    theme_minimal()+
    scale_y_reverse() + 
    theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) 
)
(patchwork <- p2 + p1 +
  plot_layout(widths = c(1,2)) +
  plot_annotation(
    title = glue("{scope_gbu} | {scope_categ} | 
                 Countries weighting 
                 {scales::percent(market_top_n_vol)} of volume"),
    subtitle = 'Which Market enrichment is improving or worsening the baseline',
    caption = glue("Data extract on 2023-09-05 | From: {legend_date_scope}")
  )
)

# bbplot::finalise_plot(
#   plot_name = patchwork,
#   source = "Source: SCeye",
#   save_filepath = "../plot.png",
#   width_pixels = 640,
#   height_pixels = 450,
#   logo_image_path = "placeholder.png") 

### FVA Volume bar plot ---- 
# base_plot <- 
temp_impwor <- market_fva %>% 
  filter(FVA %in% c("Imp", "Wor")) 
temp_net    <- market_fva %>% 
  filter(FVA == "Net")

ggplot(temp_impwor, aes(Country, FVA_Vol))+
  geom_col(
  aes(
    x = reorder(Old_z_touch_segm, FVA_NetVol),
    y = FVA_Vol,
    fill = case_when(
      FVA_NetVol >= 0 ~ "Improving",
      FVA_NetVol < 0 ~ "Worsening",
      T ~ ""
    ),
    alpha = Old_z_touch_segm
  ), 
  position = "dodge", width = .95) +
  scale_fill_manual(values = c("#ED6C4E", "#7A00E6")) +
  scale_alpha_manual(values = c(1, 0.4)) +
  coord_flip() +
  facet_wrap(~ reorder(Country, -FVA_NetVol),
             ncol = 1,
             strip.position = "left"
  ) +
  geom_point(temp_net,
             aes(
               x = Old_z_touch_segm,
               y = FVA_NetVol,
               size = 5,
             ),
             shape = 21, # or size for additional information, might add clutter
             colour = "white",
             # fill = ifelse(market_fva$FVA_NetVol > 0, "#00B050", "#6C1C0B"),
             size = 5,
             show.legend = FALSE
  ) +
  theme(
    # Title
    plot.title = element_text(face = "bold", size = 14),
    # remove categories from axis (already covered by alpha)
    axis.text.y = element_blank(),
    # Format legend
    legend.background = element_rect(
      fill = "white",
      # linewidth = 4,
      colour = "grey90",
    ),
    legend.box = "horizontal",
    legend.position = "bottom",
    strip.placement = "outside",
    strip.background = element_rect(fill = NA, color = "white"),
    strip.text = element_text(face = "bold"),
    strip.text.y.left = element_text(angle = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey90"),
  ) 


temp_impwor %>% 
  ggplot() +
  geom_col(
    aes(
      x = reorder(Old_z_touch_segm, FVA_NetVol),
      y = FVA_Vol,
      fill = filter(FVA != "Net"),
      # fill = case_when(
      #   FVA_NetVol >= 0 ~ "Improving",
      #   FVA_NetVol < 0 ~ "Worsening",
      #   T ~ ""
      # ),
      alpha = Old_z_touch_segm
    ),
    position = "dodge", width = .95
  ) +
  scale_fill_manual(values = c("#ED6C4E", "#7A00E6")) +
  scale_alpha_manual(values = c(1, 0.4)) +
  coord_flip() +
  facet_wrap(~ reorder(Country, -FVA_NetVol),
             ncol = 1,
             strip.position = "left"
  ) +
  geom_point(temp_net,
    aes(
      x = Old_z_touch_segm,
      y = FVA_NetVol,
      size = 5,
    ),
    shape = 21, # or size for additional information, might add clutter
    colour = "white",
    # fill = ifelse(market_fva$FVA_NetVol > 0, "#00B050", "#6C1C0B"),
    size = 5,
    show.legend = FALSE
  ) +
  theme(
    # Title
    plot.title = element_text(face = "bold", size = 14),
    # remove categories from axis (already covered by alpha)
    axis.text.y = element_blank(),
    # Format legend
    legend.background = element_rect(
      fill = "white",
      # linewidth = 4,
      colour = "grey90",
    ),
    legend.box = "horizontal",
    legend.position = "bottom",
    strip.placement = "outside",
    strip.background = element_rect(fill = NA, color = "white"),
    strip.text = element_text(face = "bold"),
    strip.text.y.left = element_text(angle = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey90"),
  ) 






# 


top_90_abs_enrich <- market_fva %>%
  filter(
    Market %in% c(market_fva %>%
                    arrange(FVA, -Enrichment_vol) %>%
                    head(15) %>%
                    pull(Market)),
    FVA != "Abs"
  ) %>%
  select(!Abs_Enrichment)
market_fva <- top_15_abs_enrich


# Plot --------------------------------------------------------------------


# Flipped

base_plot <- ggplot(posneg_fva) +
  geom_col(
    aes(
      x = reorder(Old_z_touch_segm, abs(Enrichment_vol)),
      y = Enrichment_vol,
      fill = FVA,
      alpha = Old_z_touch_segm
    ),
    position = "stack", width = .95
  ) +
  scale_fill_manual(values = c("#ED6C4E", "#7A00E6")) +
  scale_alpha_manual(values = c(1, 0.4)) +
  coord_flip() +
  facet_wrap(~ reorder(str_to_title(Market), -Net_Enrichment),
             ncol = 1,
             strip.position = "left"
  ) +
  # Add the dot plot for the net_FVA in volume
  geom_point(
    data = net_fva,
    aes(
      x = Old_z_touch_segm,
      y = Enrichment_vol,
      size = abs(Enrichment_vol),
    ),
    shape = 21, # or size for additional information, might add clutter
    colour = "white",
    fill = ifelse(net_fva$Enrichment_vol > 0, "#00B050", "#6C1C0B"),
    size = 5,
    show.legend = FALSE
  )



base_plot

labelled <- base_plot +
  labs(
    x = NULL,
    y = "Added Value of the Enrichment versus the Baseline in box volume",
  ) +
  # Change y axis from scientific to number notation and center the scale to
  # show better the differences between pos and neg enrichments
  scale_y_continuous(
    labels = label_comma(),
    limits = c(
      -ceiling(max(abs(posneg_fva$Enrichment_vol))),
      ceiling(max(abs(posneg_fva$Enrichment_vol)))
    )
  ) +
  ggtitle("Forecast value added - volume view", 
          "Deteriorating and Improving enrichments in box volume")
labelled

styled <- labelled +
  theme_minimal() +
  theme(
    # Title
    plot.title = element_text(face = "bold", size = 14),
    # remove categories from axis (already covered by alpha)
    axis.text.y = element_blank(),
    # Format legend
    legend.background = element_rect(
      fill = "white",
      # linewidth = 4,
      colour = "grey90",
    ),
    legend.box = "horizontal",
    legend.position = "bottom",
    strip.placement = "outside",
    strip.background = element_rect(fill = NA, color = "white"),
    strip.text = element_text(face = "bold"),
    strip.text.y.left = element_text(angle = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey90"),
  ) +
  guides(label = FALSE) +
  labs(fill = "FVA in volume", alpha = "Old segment")

styled

