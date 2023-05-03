# Libraries ---------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(readxl)
library(scales)
library(glue)
# library(extrafont)
library(ggh4x)
loadfonts(device = "win", quiet = TRUE) 

# Variables ---------------------------------------------------------------

scope_categ <- c("Zero Touch", "Enrichment") # c("Enrichment", "Zero Touch")
scope_gbu <- c("GEM", "SPC", "CHC") # c("GEM", "SPC", "CHC")
bias_threshold <- .1

sceye_extract <- file.choose()
options(digits=2)

# Data loading and cleansing ----------------------------------------------

# File read

df_sku <- read_excel(sceye_extract,
                     col_types = c("text", # GBU
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
                                   "text") # REGION
)
# Rename columns
colnames(df_sku) <- c("GBU", "Country", "Date", "GMID", "P_Fam", "Vol_w_stat", "Vol", "Stat_fcst", "Final_fcst", 
                      "Old_z_touch_segm", "Franchise", "Asset", "MAPE_ExcludeRuptures", "REGION") 

# coerce integers (not done correclty w/ read_excel)
df_sku <- df_sku %>% mutate_at(c("Vol_w_stat", "Vol", "Stat_fcst", "Final_fcst"), as.integer)
# datetime to date
df_sku <- df_sku %>% mutate(Date = as.Date(Date))
# Cleanse P_Fam
df_sku <- df_sku %>% mutate(P_Fam = str_replace(P_Fam, "_.*", "")) 

#
#
# df_sku: raw extract from SCEYE


# add info ----------------------------------------------------------------

# Over/Underforecasting. On final forecast vs sales in volume

df_extended <- 
  df_sku %>% mutate(Bias = case_when(Vol/Final_fcst < (1- bias_threshold) ~ "Overforecast", 
                                     Vol/Final_fcst > (1 + bias_threshold) ~ "Underforecast", 
                                     .default = "Unbiased"))
# group by maret/gmid and calculate spa

spa_hits <- df_extended %>% group_by(Country, GMID, Date, Asset, REGION, GBU, Franchise) %>%
  filter(GBU == "SPC", 
         REGION == "EUROPE", 
         MAPE_ExcludeRuptures == "No Impact") %>%
  summarise(SPA = sum(Vol)/sum(Final_fcst)) %>% 
  filter(!is.na(SPA) & !is.infinite(SPA)) %>% 
  ggplot(aes(x=SPA, fill=case_when(GBU == "SPC" ~ Franchise, GBU == "GEM" ~ Asset))) +
  geom_vline(aes(xintercept=1),   
             color="grey80") +
  geom_histogram(position="identity",
                 # colour="#7A00E6", 
                 # fill = "#7A00E6", 
                 alpha=0.2, binwidth = 0.1) +
  scale_x_continuous(limits = c(0, 2)) +
  theme_light() +
  stat_theodensity(aes(y = stat(count) * 0.1, color=case_when(GBU == "SPC" ~ Franchise, GBU == "GEM" ~ Asset))) +
  # facet_wrap(. ~ Country, scales = "free") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey90"), 
    # strip.placement = "outside",
    strip.background = element_rect(fill = NA, color = "white"),
    strip.text = element_text(face="bold", colour = "#23004C"),
    legend.box = "horizontal",
    legend.position = "bottom"
  ) +
  ggtitle("Distribution of Forecast hits SPA", "GenMed Oct22 to Mar23") +
  ylab("Frequency of hit") + xlab("SPA hit") 

ggplotly(spa_hits)

# Checks ------------------------------------------------------------------

# is abserr calculated correctly?
mutate(df_sku, abserr_check = abs(Stat_fcst - Vol_w_stat)-abserr_stat + abs(Final_fcst - Vol_w_stat)-abserr_final) %>% 
  filter(abserr_check > 2) %>% 
  arrange(-abserr_check)


# Filter out where no stat forecast is available 
df_sku <- df_sku %>% filter(Vol_w_stat > 0) 

# keep the scope defined in variables
df_sku <- df_sku %>% filter(GBU %in% scope_gbu &
                              Old_z_touch_segm %in% scope_categ) 






# Add pos FVA info
df2 <- df_sku %>% mutate(FVA = ifelse(Enrichment_vol>=0, "Improving", "Deteriorating"))

# group by market and summarize
market_df <- df2 %>% group_by(Market, FVA, Old_z_touch_segm)



############################  TOP 10
market_df <- market_df %>% filter(Market %in% c("FRANCE", "UNITED STATES", "UNITED KINGDOM", "CHINA", "JAPAN", "ITALY", "GERMANY", "SPAIN", "BRAZIL", "RUSSIA"))
############################

############################  GEM KEY MARKETS 
market_df <- market_df %>% 
  filter(Market %in% c("FRANCE", 
                       "ITALY",
                       "SPAIN", "PORTUGAL", 
                       "GERMANY", "AUSTRIA", "SWITZERLAND", 
                       "UNITED KINGDOM", "IRELAND", 
                       "BRAZIL", "RUSSIA", "ALGERIA", 
                       "TURKEY",
                       "UNITED ARAB EMIRATES", "YEMEN", "OMAN", "SAUDI ARABIA", "QATAR", "KUWAIT", "BAHRAIN")
  ) %>% 
  filter(Asset == "Global Core Assets")
############################

############################ Same markets as top 90% Market view
market_df <- market_df %>% 
  filter(Market %in% pull(df_top90, Market))

############################


net_fva <- market_df %>% 
  mutate(FVA = "Net", Market = gsub(" ", "\n", x = str_to_title(Market))) %>% 
  summarise(Enrichment_vol = sum(Enrichment_vol))

abs_fva <- market_df %>% 
  mutate(FVA = "Abs", Market = gsub(" ", "\n", x = str_to_title(Market))) %>% 
  summarise(Enrichment_vol = sum(abs(Enrichment_vol)))

posneg_fva <- market_df %>% 
  mutate(Market = gsub(" ", "\n", x = str_to_title(Market))) %>% 
  summarise(Enrichment_vol = sum(Enrichment_vol))

##### add net column to all here####

posneg_fva <- posneg_fva %>% 
  left_join(net_fva, 
            by = c("Market", "Old_z_touch_segm"),
            suffix = c("", ".y")) %>% 
  select(!FVA.y) %>% 
  rename(Net_Enrichment = Enrichment_vol.y)


net_fva <- net_fva %>% 
  mutate(Net_Enrichment = Enrichment_vol)

abs_fva <- abs_fva %>% 
  mutate(Abs_Enrichment = Enrichment_vol)


##########
market_fva <- bind_rows(net_fva, posneg_fva, abs_fva)

top_15_abs_enrich <- market_fva %>% 
  filter(Market %in% c(market_fva %>% 
                         arrange(FVA, -Enrichment_vol) %>% 
                         head(15) %>% 
                         pull(Market)),
         FVA != "Abs") %>% 
  select(!Abs_Enrichment)
market_fva <- top_15_abs_enrich

# Plot --------------------------------------------------------------------


# Flipped

base_plot <- ggplot(posneg_fva) +
  geom_col(aes(x = reorder(Old_z_touch_segm, abs(Enrichment_vol)),
               y = Enrichment_vol,
               fill = FVA,
               alpha = Old_z_touch_segm),
           position = "stack", width = .95
  ) +
  scale_fill_manual(values = c("#ED6C4E", "#7A00E6")) +
  scale_alpha_manual(values = c(1, 0.4)) +
  coord_flip() +
  facet_wrap(~reorder(str_to_title(Market), -Net_Enrichment), ncol = 1,
             strip.position = "left") +
  # Add the dot plot for the net_FVA in volume
  geom_point(data = net_fva,
             aes(x = Old_z_touch_segm,
                 y = Enrichment_vol,
                 size = abs(Enrichment_vol),
             ),
             shape = 21, # Can also be the size for additional information, might add clutter
             colour = "white",
             fill = ifelse(net_fva$Enrichment_vol>0,"#00B050","#6C1C0B"),
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
  scale_y_continuous(labels = label_comma(), 
                     limits = c(-ceiling(max(abs(posneg_fva$Enrichment_vol))),
                                ceiling(max(abs(posneg_fva$Enrichment_vol))))
  ) +
  ggtitle("Forecast value added - volume view", "Deteriorating and Improving enrichments in box volume")
labelled

styled <- labelled +
  theme_minimal()  + 
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
    strip.text = element_text(face="bold"),
    strip.text.y.left = element_text(angle = 0), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey90"), 
  ) + 
  guides(label = FALSE) +
  labs(fill = "FVA in volume", alpha = "Old segment") 

styled 
