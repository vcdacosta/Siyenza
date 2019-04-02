
# .libPaths("C:/R/RLib")

#load installed packages into R#
library(tidyverse)
library(readxl)
library(lubridate)
library(splitstackshape)

`%ni%` <- Negate(`%in%`) 

GenerateInteragencyOutput <-  function(startOfSiyenza, 
                                       endOfSiyenza, 
                                       currentWeekStart,
                                       currentWeekEnd)
{
  startOfSiyenza   <-  as.POSIXct(startOfSiyenza)
  endOfSiyenza     <-  as.POSIXct(endOfSiyenza)
  currentWeekStart <-  as.POSIXct(currentWeekStart)
  currentWeekEnd   <-  as.POSIXct(currentWeekEnd)
  # 
  # 
  # startOfSiyenza   <-  as.POSIXct("2019-03-15")
  # endOfSiyenza     <-  as.POSIXct("2019-05-10")
  # currentWeekStart <-  as.POSIXct("2019-03-16")
  # currentWeekEnd   <-  as.POSIXct("2019-03-22")
  
  weeks_remaining <- isoweek(endOfSiyenza) - isoweek(currentWeekEnd)
  
  
  cdc_result <- read_excel("RAW/CDC_Siyenza_20190327.xlsx", sheet = "Siyenza") %>%
    filter(Week_End >= startOfSiyenza & Week_End <= date(currentWeekEnd) +1)
  
  usaid_result <- read_excel("RAW/USAID_Siyenza_20190329.xlsx", sheet = "USAID")
  
  df_merged <- bind_rows(cdc_result, usaid_result) %>% 
    rename(TPT_Initiated = "TPT initiated")
  
  
  df_all <-  df_merged %>%
    gather(indicator, value,HTS_TST_POS:TARG_WKLY_NETNEW) %>%
    # filter(indicator %in% c("TX_CURR_28")) %>% 
    mutate(value = case_when(Week_End < date(currentWeekEnd) - 7 & indicator %in% c("TX_CURR_28")  ~ 0,
                             Week_End <= date(startOfSiyenza) & indicator %ni% c("TX_CURR_28")  ~ 0, 
                             TRUE ~ value)) %>% 
    spread(indicator, value)
  
  df_replicate <- df_all %>% 
    filter(Week_End == date(currentWeekEnd)) %>% 
    gather(indicator, value, cLTFU:uLTFU) %>% 
    mutate(value = 0) %>% 
    spread(indicator, value)
  
  all <-  df_all
  
  for(i in 1:weeks_remaining)
  {
    df_all_replicate <- df_replicate %>% 
      mutate(Week_Start = as.POSIXct(date(currentWeekStart) + (7*i)),
             Week_End   = as.POSIXct(date(currentWeekEnd)+ (7*i)))
    all <- bind_rows(all, df_all_replicate)
    # print(i)
      
  }
  
  
  df_cum <-  df_all %>%
    gather(indicator, value, cLTFU:uLTFU, na.rm = TRUE) %>% 
    filter(indicator %in% c("HTS_TST_POS","TX_NEW", "TX_NEW_SAMEDAY", "TPT_Initiated")) %>% 
    group_by(Facility, Week_End, indicator) %>% 
    mutate(cum_value = cumsum(value)) %>% 
    ungroup %>% 
    mutate(indicator = paste0(indicator, "_CUM")) %>% 
    filter(Week_End == date(currentWeekEnd)) %>% 
    select(-value) %>% 
    spread(indicator, cum_value)
  
  df_final <-  bind_rows(all, df_cum) %>% 
    gather(indicator, value, cLTFU:TX_NEW_SAMEDAY_CUM, na.rm = TRUE) %>% 
    spread(indicator, value) %>% 
    replace(is.na(.), "") %>% 
    rename("TPT initiated" = TPT_Initiated)
  
  df_tx_curr_baseline <-  df_all %>% 
    gather(indicator, value, cLTFU:uLTFU, na.rm = TRUE) %>% 
    filter(indicator %in% c("TX_CURR_28")) %>% 
    mutate(indicator = case_when(Week_End == date(startOfSiyenza)
                                 ~ "TX_CURR_28_BASE", TRUE ~ "")) %>% 
    filter(indicator %in% c("TX_CURR_28_BASE"))
  
  df_tx_curr_todate <- df_all %>% 
    gather(indicator, value, cLTFU:uLTFU, na.rm = TRUE) %>% 
    filter(indicator %in% c("TX_CURR_28")) 
  
  max_tx_curr_date <-  max(df_tx_curr_todate$Week_End)
  
  # df_tx_curr_todate <-  df_tx_curr_todate %>% 
  #   mutate(indicator = case_when(Week_End == max_tx_curr_date ~ "TX_CURR_28_TODATE",
  #                                ))
  
  
  
  
  
  write.table(df, paste0("Outputs/interagencyDash_", Sys.Date(), ".txt"), sep = "\t", row.names = FALSE)
}

GenerateInteragencyOutput(startOfSiyenza = "2019-03-15",
                          endOfSiyenza = "2019-05-10",
                          currentWeekStart = "2019-03-16",
                          currentWeekEnd = "2019-03-22" )

