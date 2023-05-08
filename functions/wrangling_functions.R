# Data loading and cleaning ----

fun_open_sceye_extract <- function() {
  sceye_new_file_confirm <- menu(choices = c("Yes", "No"), 
                                   title = "\nLoad a new extract of SCeye?")
  if(sceye_new_file_confirm == 1){
    sceye_extract <<- file.choose() 
  } else {
    sceye_extract <<- "22-10_23-03_3GBU.xlsx"
  }
  return(sceye_extract)
}

fun_reload_sceye_extract <- function() {
  # File read into df_sku
  df_sku <<- read_excel(sceye_extract,
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
  colnames(df_sku) <<- c(
    "GBU", "Country", "Date", "GMID", "P_Fam", 
    "Vol_w_stat", "Vol", "Stat_fcst", "Final_fcst",
    "Old_z_touch_segm", "Franchise", "Asset", "MAPE_ExcludeRuptures", "REGION"
  )
  
  # coerce integers (not done correctly w/ read_excel), datetime to date, Cleanse P_Fam
  df_sku <<- df_sku %>% 
    mutate_at(c("Vol_w_stat", "Vol", "Stat_fcst", "Final_fcst"), as.integer) %>% 
    mutate(Date = as.Date(Date)) %>% 
    mutate(P_Fam = str_replace(P_Fam, "_.*", ""))
  
  
  #
  #
  # df_sku: raw extract from SCEYE
}

