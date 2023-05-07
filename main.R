
# Libraries and source files ----------------------------------------------

library(ggplot2)
library(tidyverse)
library(readxl)
library(scales)
library(glue)
library(extrafont)
library(ggh4x)
library(ggallin)   # to plot negative values on log scale
loadfonts(device = "win", quiet = TRUE)

source("functions/plot_functions.R")


# Variables ---------------------------------------------------------------

scope_categ <- c("Zero Touch") # c("Enrichment", "Zero Touch")
scope_gbu <- c("GEM") # c("GEM", "SPC", "CHC")
bias_threshold <- .1

<<<<<<< HEAD
sceye_extract <- "22-10_23-03_3GBU.xlsx"
# sceye_extract <- file.choose()
=======
sceye_extract <- file.choose()
>>>>>>> 183f9e625997e6818d513c4598d4d0f93b28d282
options(digits = 2)

# Data loading and cleansing ----------------------------------------------

# File read into df_sku

df_sku <- read_excel(sceye_extract,
                     col_types = c(
                       "text", # GBU
                       "text", # Country
                       "date", # DATE
                       "text", # GMID
                       "text", # P_Fam
                       "text", # * Sales_with_Stat_Fcst *
                       "text", # * Sales *
                       "text", # * Statistical Forecast *
                       "text", # * Final Forecast *
                       "text", # GBU_Segmentation
                       "text", # FRANCHISE
                       "text", # asset_segmentation
                       "text", # MAPE_ExcludeRuptures
                       "text"  # REGION
                     ) 
)
# Rename columns
colnames(df_sku) <- c(
  "GBU", "Country", "Date", "GMID", "P_Fam", "Vol_w_stat", "Vol", "Stat_fcst", "Final_fcst",
  "Old_z_touch_segm", "Franchise", "Asset", "MAPE_ExcludeRuptures", "REGION"
)

# coerce integers (not done correctly w/ read_excel), datetime to date, Cleanse P_Fam
df_sku <- df_sku %>% mutate_at(c("Vol_w_stat", "Vol", "Stat_fcst", "Final_fcst"), as.integer) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(P_Fam = str_replace(P_Fam, "_.*", ""))


#
#
# df_sku: raw extract from SCEYE


# add info ----------------------------------------------------------------

<<<<<<< HEAD
# Over/Under forecasting. On final forecast vs sales in volume
=======
# Over/Underforecasting. On final forecast vs sales in volume
>>>>>>> 183f9e625997e6818d513c4598d4d0f93b28d282

df_extended <- df_sku %>%
  mutate(Bias = case_when(
    Vol / Final_fcst < (1 - bias_threshold) ~ "Overforecast",
    Vol / Final_fcst > (1 + bias_threshold) ~ "Underforecast",
    TRUE ~ "Unbiased"
  ))

# group by maret/gmid and calculate spa

spa_hits <- df_extended %>%
  group_by(Country, GMID, Date, Asset, REGION, GBU, Franchise) %>%
  filter(
    GBU == "GEM",
    REGION == "EUROPE",
    MAPE_ExcludeRuptures == "No Impact"
  ) %>%
  summarise(SPA = sum(Vol) / sum(Final_fcst)) %>%
  filter(!is.na(SPA) & !is.infinite(SPA)) %>%
  ggplot(aes(x = SPA, fill = case_when(GBU == "SPC" ~ Franchise, GBU == "GEM" ~ Asset))) +
  geom_vline(aes(xintercept = 1),
             color = "grey80"
  ) +
  geom_histogram(
    position = "identity",
    # colour="#7A00E6",
    # fill = "#7A00E6",
    alpha = 0.2, binwidth = 0.1
  ) +
  scale_x_continuous(limits = c(-.1, 2.1)) +
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
  xlab("SPA hit")
# guides(fill=guide_legend(title="Segment"))


spa_hits + labs(fill = NULL, color = NULL)


# Market view - 0touch analysis -------------------------------------------

market_view <- df_sku

# keep the scope defined in variables
market_view <- market_view %>% filter(GBU %in% scope_gbu &
                                        Old_z_touch_segm %in% scope_categ)

## Note: might need to quantify the volume in distribution model
nbm_markets <- c(
  "Afghanistan", "Armenia", "Azerbaijan", "Bangladesh",
  "Belarus", "Bosnia", "Cambodia", "Central", "America",
  "Chile", "CIS Countries CHC", "Ecuador", "Estonia",
  "Georgia", "Kazakhstan", "Kyrgyzstan", "Latvia",
  "Lithuania", "Macedonia", "Malta", "Moldova", "NBM Eurasia",
  "NBM Europe", "Nigeria", "Pakistan", "Paraguay",
  "Sub Sahara Africa", "Tajikistan", "Turkmenistan",
  "Uruguay", "Uzbekistan"
)
market_view <- market_view %>% filter(!Country %in% nbm_markets)

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


# Filter out where no stat forecast is available
market_view <- market_view %>% filter(Vol_w_stat > 0)


# remove direct Rupture and Recovery impact
market_view <- market_view %>% filter(MAPE_ExcludeRuptures == "No Impact")

# calculate the enrichment in volume (/!\, Vol_w_stat != Vol. to take into consideration if delta is high)
# check delta
market_view_top90 %>%
  mutate(Delta = Vol_w_stat - Vol) %>%
  arrange(Delta) %>%
  filter(Delta != 0)
# Add enrichment volume
market_view <- market_view %>% 
  mutate(Enrichment_vol = Final_fcst - Stat_fcst)

# Calculate adherence vol and val
adherence_sku <- round(nrow(market_view[market_view$Enrichment_vol == 0,]) / nrow(market_view),2)
# adherence_vol 

# add some info on enrichment Direction and Size
market_view <- market_view %>% mutate(Enrichment_Direction = case_when(
  Enrichment_vol > 0 ~ "Up",
  Enrichment_vol < 0 ~ "Down",
  T ~ ""
)) %>% mutate(Enrichment_Size = case_when(
  abs(Enrichment_vol/Stat_fcst) > .15 ~ "High",
  abs(Enrichment_vol/Stat_fcst) > .07 ~ "Med",
  abs(Enrichment_vol/Stat_fcst) > .02 ~ "Low",
  abs(Enrichment_vol/Stat_fcst) > 0 ~ "Insignificant",
  T ~ ""
)) 

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
                                        FVA_Vol < 0 ~ "worsening",
                                        T ~ "Neutral"
                                      ))

# market_view %>% 
#   filter(Enrichment_vol != 0) %>%
#   ggplot(aes(x = Enrichment_Size, y=FVA_Vol, color=Enrichment_Direction)) +
#   geom_jitter() +
#   scale_y_continuous(trans = pseudolog10_trans) +
#   coord_flip()




# Countries representing top 90% of volume
top90_vol_market_list <- market_view %>%
  group_by(Country) %>%
  summarise(Vol_w_stat = sum(Vol_w_stat)) %>%
  arrange(-Vol_w_stat) %>%
  mutate(Vol_w_stat_cum = cumsum(Vol_w_stat / sum(Vol_w_stat))) %>%
  filter(lag(Vol_w_stat_cum, default = 0) < .9) %>%
  pull(Country)

# filter on top90% volume countries
market_view_top90 <- market_view %>% filter(Country %in% top90_vol_market_list)


market_view_top90_group <- market_view_top90 %>% group_by(GBU, REGION, Country, Old_z_touch_segm, Enrichment_vol)

# market_view %>% mutate(touched = case_when(
#   Enrichment_vol == 0 ~ F,
#   T ~ T
# )) %>% 
#   group_by(touched) %>% summarise(sum(Vol_w_stat))

# add  enrichment in volume info


# Prepare Plots -----------------------------------------------------------
### Market view
market_90_fva_mape_base_plot <- market_view_top90 %>% 
  group_by(GBU, REGION, Country, Old_z_touch_segm) %>% 
  summarise(FVA_mape_pp = round(sum(abs_er_stat-abs_er_final)/sum(Vol_w_stat)*100,2)) %>% 
  arrange(-FVA_mape_pp) %>% 
  mutate(Performance = case_when(
    FVA_mape_pp > 0.5 ~ "Improving",
    FVA_mape_pp < -0.5 ~ "Worsening",
    T ~ "Neutral"
  )) 

### Volume top_down view

net_fva <- market_view_top90_group %>%
  mutate(FVA = "Net") %>%
  group_by(GBU, REGION, Country, Old_z_touch_segm, FVA) %>% 
  summarise(FVA_Vol = sum(FVA_Vol))

abs_fva <- market_view_top90_group %>%
  mutate(FVA = "Abs") %>%
  group_by(GBU, REGION, Country, Old_z_touch_segm, FVA) %>% 
  summarise(FVA_Vol = sum(abs(FVA_Vol)))

imp_fva <- market_view_top90_group %>%
  mutate(FVA = "Imp") %>%
  group_by(GBU, REGION, Country, Old_z_touch_segm, FVA) %>% 
  filter(FVA_Perf == "Improving") %>% 
  summarise(FVA_Vol = sum(Enrichment_vol))

wor_fva <- market_view_top90_group %>%
  mutate(FVA = "Wor") %>%
  group_by(GBU, REGION, Country, Old_z_touch_segm, FVA) %>% 
  filter(FVA_Perf == "worsening") %>% 
  summarise(FVA_Vol = sum(Enrichment_vol))

market_fva <- bind_rows(net_fva, abs_fva, imp_fva, wor_fva)


market_fva <- market_fva %>% 
  left_join(net_fva,
            by = c("GBU", "REGION", "Country", "Old_z_touch_segm"),
            suffix = c("", ".y")) %>% 
  select(!FVA.y) %>% 
  rename(FVA_NetVol = FVA_Vol.y) 


# Plots -------------------------------------------------------------------
## Market view

#### prepare Legend
# Am I changing years?
dates_in_same_year <- year(max(market_view_top90$Date)) - year(min(market_view_top90$Date)) == 0
ifelse(dates_in_same_year, 
       start_date <- format(min(market_view_top90$Date), "%B"),
       start_date <- format(min(market_view_top90$Date), "%B %Y")
       )
end_date <- format(max(market_view_top90$Date), "%B %Y")
# legend
legend_date_scope <- paste(start_date, "to", end_date) 


market_90_fva_mape_base_plot %>% ggplot(aes(x=reorder(Country, -FVA_mape_pp), y=FVA_mape_pp, label=round(FVA_mape_pp, digits = 1))) + 
  geom_segment(aes(y = 0,
                   x = reorder(Country, FVA_mape_pp),
                   yend = FVA_mape_pp,
                   xend = reorder(Country, FVA_mape_pp)),
               color = "grey60",
               linetype="dashed") +
  geom_point(stat='identity', aes(col=Performance), size=8)  +
  scale_color_manual(name="FVA Performance",
                     values = c("Improving"="#7A00E6", "Worsening"="#ED6C4E", "Neutral"="grey60")) +
  geom_text(color="white", size=3) +
  coord_flip(ylim = c(-15, 15)) + 
  labs(title="FVA in wMAPE percentage point", 
       subtitle="Countries cumulating 90% of 0-touch Volume",
       caption = legend_date_scope) + 
  labs(
    x = NULL, 
    y = "wMAPE pp", 
  ) +
  theme_minimal()+
  theme(
    legend.position = "bottom"
  )




# Flipped
# base_plot <- 
market_fva %>% 
  filter(FVA %in% c("Imp", "Wor")) %>% 
  ggplot() +
  geom_col(
    aes(
      x = reorder(Old_z_touch_segm, (FVA_NetVol)),
      y = FVA_Vol,
      fill = FVA,
      # fill = case_when(
      #   FVA_NetVol >= 0 ~ "Improving",
      #   FVA_NetVol < 0 ~ "Worsening",
      #   T ~ ""
      # ),
      alpha = Old_z_touch_segm
    ),
    position = "stack", width = .95
  ) +
  scale_fill_manual(values = c("#ED6C4E", "#7A00E6")) +
  scale_alpha_manual(values = c(1, 0.4)) +
  coord_flip() +
  facet_wrap(~ reorder(Country, -FVA_NetVol),
             ncol = 1,
             strip.position = "left"
  ) +
  geom_point(
    aes(
      x = Old_z_touch_segm,
      y = FVA_NetVol,
      size = 5,
    ),
    shape = 21, # Can also be the size for additional information, might add clutter
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








# Checks ------------------------------------------------------------------

# is abserr calculated correctly?
mutate(df_sku, abserr_check = abs(Stat_fcst - Vol_w_stat) - abserr_stat + abs(Final_fcst - Vol_w_stat) - abserr_final) %>%
  filter(abserr_check > 2) %>%
  arrange(-abserr_check)


# Filter out where no stat forecast is available
df_sku <- df_sku %>% filter(Vol_w_stat > 0)

# keep the scope defined in variables
df_sku <- df_sku %>% filter(GBU %in% scope_gbu &
                              Old_z_touch_segm %in% scope_categ)






# Add pos FVA info
df2 <- df_sku %>% mutate(FVA = ifelse(Enrichment_vol >= 0, "Improving", "Deteriorating"))

# group by market and summarize
market_df <- df2 %>% group_by(Market, FVA, Old_z_touch_segm)



############################  TOP 10
market_df <- market_df %>% filter(Market %in% c("FRANCE", "UNITED STATES", "UNITED KINGDOM", "CHINA", "JAPAN", "ITALY", "GERMANY", "SPAIN", "BRAZIL", "RUSSIA"))
############################

############################  GEM KEY MARKETS
market_df <- market_df %>%
  filter(Market %in% c(
    "FRANCE",
    "ITALY",
    "SPAIN", "PORTUGAL",
    "GERMANY", "AUSTRIA", "SWITZERLAND",
    "UNITED KINGDOM", "IRELAND",
    "BRAZIL", "RUSSIA", "ALGERIA",
    "TURKEY",
    "UNITED ARAB EMIRATES", "YEMEN", "OMAN", "SAUDI ARABIA", "QATAR", "KUWAIT", "BAHRAIN"
  )) %>%
  filter(Asset == "Global Core Assets")
############################

############################ Same markets as top 90% Market view
market_df <- market_df %>%
  filter(Market %in% pull(df_top90, Market))

############################




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
    shape = 21, # Can also be the size for additional information, might add clutter
    colour = "white",
    fill = ifelse(net_fva$Enrichment_vol > 0, "#00B050", "#6C1C0B"),
    size = 5,
    show.legend = FALSE
  )


# base_plot <- ggplot(posneg_fva) +
#   geom_col(aes(x = reorder(Old_z_touch_segm, abs(Enrichment_vol)),
#                y = Enrichment_vol,
#                fill = FVA,
#                alpha = Old_z_touch_segm),
#            position = "stack", width = .95
#            ) +
#   # ifelse(isTRUE(length(scope_categ) == 1), guides(alpha = FALSE), guides()) +
#   scale_fill_manual(values = c("#ED6C4E", "#7A00E6")) +
#   scale_alpha_manual(values = c(1, 0.4)) +
#   # coord_flip() +
#   facet_wrap(~reorder(str_to_title(Market), -Net_Enrichment),
#              nrow = 1,
#              strip.position = "bottom") +
#   # Add the dot plot for the net_FVA in volume
#   geom_point(data = net_fva,
#              aes(x = Old_z_touch_segm,
#                  y = Enrichment_vol,
#                  size = abs(Enrichment_vol),
#              ),
#              shape = 21, # Can also be the size for additional information, might add clutter
#              colour = "white",
#              fill = ifelse(net_fva$Enrichment_vol>0,"#00B050","#6C1C0B"),
#              size = 5,
#              show.legend = FALSE
#   )

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
  ggtitle("Forecast value added - volume view", "Deteriorating and Improving enrichments in box volume")
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

