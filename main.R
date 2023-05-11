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
## (df_sku)  ----

# Dataset extension ----
## (df_extended) ----

source("dataset_extension.R")

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
source("prep_market_view.R") # from df_extended to market_nperc_fva_mape_base_plot_filtered

## Market View Plot ----
### FVA wMAPE lollipop plot ----

plot_lollipop_FVA_mape <- market_nperc_fva_mape_base_plot_filtered %>% 
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

### Forecast scope Volume bar plot ----

plot_volume_bar <- market_nperc_fva_mape_base_plot_filtered %>% 
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

# bbplot::finalise_plot(
#   plot_name = patchwork,
#   source = "Source: SCeye",
#   save_filepath = "../plot.png",
#   width_pixels = 640,
#   height_pixels = 450,
#   logo_image_path = "placeholder.png") 

### FVA Volume bar plot facets ---- 

temp_impwor <- market_fva %>% 
  filter(FVA %in% c("Imp", "Wor")) 
temp_net    <- market_fva %>% 
  filter(FVA == "Net") 


plot_fva_vol_bar <- ggplot() +
  geom_col(data=temp_impwor,
           show.legend = FALSE,
           aes(
             x = reorder(Old_z_touch_segm, FVA_Vol),
             y = FVA_Vol,
             fill = case_when(
               FVA_Vol >= 0 ~ "Improving",
               FVA_Vol < 0 ~ "Worsening",
               T ~ ""),
             # alpha = Old_z_touch_segm
             ),
           position = "stack", width = .95,
           ) +
  scale_fill_manual(values = c("Improving"="#7A00E6", 
                               "Worsening"="#ED6C4E" )) +
  # scale_alpha_manual(values = c(1, 0.4)) +
  # geom_label(aes(label = format(round(FVA_Vol/10^6,1))), 
  #            hjust = "outward", 
  #            size=4) +
  coord_flip() +
  facet_wrap(~ reorder(Country, -FVA_Vol),
             ncol = 1,
             strip.position = "left",
             scales = "free_y",) +
  geom_point(data=temp_net, mapping=aes(
               x = reorder(Old_z_touch_segm, FVA_Vol),
               y =   FVA_Vol,
               size = 5),
             shape = 21, # or size for additional information, might add clutter
             colour = "white",
             fill = ifelse(test = temp_net$FVA_Vol > 0, "#00B050", "#6C1C0B"),
             size = 5,
             show.legend = FALSE
  ) +
  theme(
    # Title
    plot.title = element_text(face = "bold", size = 14),
    
    # remove categories from axis (already covered by alpha)
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # legend.box = "horizontal",
    # legend.position = "bottom",
    # strip.placement = "outside",
    # strip.background = element_rect(fill = NA, color = "white"),
    # strip.text.y.left = element_text(angle = 0),
    strip.text = element_blank(),    #element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey90"),
  ) 


### FVA Volume bar plot simple ----

temp_impwor <- market_fva %>% 
  filter(FVA %in% c("Imp", "Wor")) 
temp_net    <- market_fva %>% 
  filter(FVA == "Net") 

plot_fva_vol_bar <- ggplot() +
geom_col(data=temp_impwor,
         show.legend = FALSE,
         aes(
           x = reorder(Country, FVA_Vol),
           y = FVA_Vol,
           fill = case_when(
             FVA_Vol >= 0 ~ "Improving",
             FVA_Vol < 0 ~ "Worsening",
             T ~ ""),
         ),
         position = "stack", width = .95,
) +
  scale_fill_manual(values = c("Improving"="#7A00E6", 
                               "Worsening"="#ED6C4E" )) +
  # scale_alpha_manual(values = c(1, 0.4)) +
  # geom_label(aes(label = format(round(FVA_Vol/10^6,1))), 
  #            hjust = "outward", 
  #            size=4) +
  coord_flip() +
  geom_point(data=temp_net, mapping=aes(
    x = reorder(Country, FVA_Vol),
    y =   FVA_Vol,
    size = 5),
    shape = 21, # or size for additional information, might add clutter
    colour = "white",
    fill = ifelse(test = temp_net$FVA_Vol > 0, "#00B050", "#6C1C0B"),
    size = 5,
    show.legend = FALSE
  ) +
  labs(
    # title="FVA in wMAPE percentage point",
    # subtitle="Countries cumulating 90% of 0-touch Volume",
    # caption = legend_date_scope,
    x = NULL,
    y = "Volume (M Units)"
  ) +
  theme_minimal()+
  theme(
    # Title
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
  ) 


### Patchwork ----

(patchwork <- plot_volume_bar + plot_lollipop_FVA_mape + plot_fva_vol_bar &
   # plot_layout(widths = c(1,2,2))

 plot_annotation(
   title = glue("{scope_gbu} | {scope_categ} | Countries weighting {scales::percent(market_top_n_vol)} of volume"),
   subtitle = 'Which Market enrichment is improving or worsening the baseline?',
   caption = glue("Data extract on 2023-09-05 | From: {legend_date_scope}")
 )
)
