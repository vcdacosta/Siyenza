
# .libPaths("C:/R/RLib")

#load installed packages into R#
library(tidyverse)
library(readxl)
library(lubridate)
library(splitstackshape)

`%ni%` <- Negate(`%in%`) 

GenerateInteragencyOutput <-  function(tx_curr_startOfSiyenza,
                                       startOfSiyenza, 
                                       endOfSiyenza, 
                                       currentWeekStart,
                                       currentWeekEnd,
                                       df)
{
  tx_curr_startOfSiyenza   <-  as.POSIXct(tx_curr_startOfSiyenza)
  startOfSiyenza           <-  as.POSIXct(startOfSiyenza)
  endOfSiyenza             <-  as.POSIXct(endOfSiyenza)
  currentWeekStart         <-  as.POSIXct(currentWeekStart)
  currentWeekEnd           <-  as.POSIXct(currentWeekEnd)


  # tx_curr_startOfSiyenza  <-  as.POSIXct("2019-03-01")
  # startOfSiyenza          <-  as.POSIXct("2019-03-15")
  # endOfSiyenza            <-  as.POSIXct("2019-05-10")
  # currentWeekStart        <-  as.POSIXct("2019-03-30")
  # currentWeekEnd          <-  as.POSIXct("2019-04-05")

  weeks_remaining <- isoweek(endOfSiyenza) - isoweek(currentWeekEnd)
  
  tx_curr_dates <-  c("2019-03-01","2019-03-29","2019-04-12","2019-05-03", "2019-05-10")
  
  cdc_result <- read_excel("RAW/CDC_Siyenza_20190409.xlsx", sheet = "Siyenza") %>%
    # filter(Week_End >= startOfSiyenza & Week_End <= date(currentWeekEnd) +1)
    filter(Week_End >= date(tx_curr_startOfSiyenza) & Week_End <= date(currentWeekEnd))
  
  usaid_result <- read_excel("RAW/USAID_Siyenza_20190409.xlsx", sheet = "USAID") %>% 
    filter(Week_End >= date(tx_curr_startOfSiyenza) & Week_End <= date(currentWeekEnd))
  
  df_merged <- bind_rows(cdc_result,usaid_result) %>% 
    rename(TPT_Initiated = "TPT initiated") %>% 
    select(-CURR_RTC)
 
  # cdc_result,  
  
  df_all <-  df_merged %>%
    gather(indicator, value,HTS_TST_POS:TARG_WKLY_NETNEW) %>%
    # filter(indicator %in% c("TX_CURR_28")) %>% 
    mutate(value = case_when(
                             # Week_End < date(currentWeekEnd) - 7 & indicator %in% c("TX_CURR_28")  ~ 0,
                             Week_End == date(startOfSiyenza) & indicator %in% c("TX_CURR_28")  ~ 0,
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
    group_by(Facility, indicator) %>% 
    arrange(Week_End) %>% 
    mutate(cum_value = cumsum(value)) %>% 
    ungroup %>% 
    mutate(indicator = paste0(indicator, "_CUM"))%>% 
    filter(Week_End == date(currentWeekEnd)) %>%
    select(-value) %>% 
    spread(indicator, cum_value)
  
  df_final <-  bind_rows(all, df_cum) %>% 
    gather(indicator, value, cLTFU:TX_NEW_SAMEDAY_CUM, na.rm = TRUE) %>% 
    spread(indicator, value) %>% 
    replace(is.na(.), "") 
    
  
  df_tx_curr_baseline <-  df_all %>% 
    gather(indicator, value, cLTFU:uLTFU, na.rm = TRUE) %>% 
    filter(indicator %in% c("TX_CURR_28")) %>% 
    mutate(indicator = case_when(Week_End == date(tx_curr_startOfSiyenza)
                                 ~ "TX_CURR_28_BASE", TRUE ~ "")) %>% 
    filter(indicator %in% c("TX_CURR_28_BASE"))
  
  df_tx_curr_todate <- df_all %>% 
    gather(indicator, value, cLTFU:uLTFU, na.rm = TRUE) %>% 
    filter(indicator %in% c("TX_CURR_28")) 
  
  max_tx_curr_date <-  max(df_tx_curr_todate$Week_End)
  
  
  ###TODO:
  # if(max_tx_curr_date == date(currentWeekEnd))
  # {
    df_tx_curr_todate <-  df_tx_curr_todate %>% 
      filter(Week_End == max_tx_curr_date) %>% 
      mutate(indicator = "TX_CURR_28_TODATE") %>% 
      filter(indicator %in% "TX_CURR_28_TODATE")
    
  # }
  # else
  # {
  #   df_tx_curr_todate <- df_tx_curr_todate %>% 
  #     filter()
  #     
  # }
  df_tx_curr_merged <-  bind_rows(df_tx_curr_baseline, df_tx_curr_todate) 
  
  df_net_new <- df_tx_curr_merged %>% 
    select(-Week_Start, -Week_End) %>%
    spread(indicator, value) %>% 
    mutate(TX_NET_NEW_28_TODATE = (`TX_CURR_28_TODATE` - `TX_CURR_28_BASE`),
           Week_Start = date(as.POSIXct(max_tx_curr_date))-6,
           Week_End = date(as.POSIXct(max_tx_curr_date)),
           TX_NET_NEW_AVG = case_when(Week_End == date(max_tx_curr_date) ~ TX_NET_NEW_28_TODATE/4,
                                      TRUE ~ TX_NET_NEW_28_TODATE/2)) %>%
    gather(indicator, value, TX_CURR_28_BASE,TX_CURR_28_TODATE,TX_NET_NEW_28_TODATE, TX_NET_NEW_AVG) %>% 
    filter(indicator %in% c('TX_NET_NEW_28_TODATE', 'TX_NET_NEW_AVG')) %>% 
    mutate(Week_Start = as.POSIXct(Week_Start), 
           Week_End = as.POSIXct(Week_End))
 
  df_curr <-  bind_rows(df_tx_curr_merged,df_net_new)
  
  df_final <-  df_final %>% 
    gather(indicator, value, cLTFU:uLTFU, na.rm = TRUE ) %>% 
    mutate(value = as.numeric(value))
  
  df <-  bind_rows(df_final, df_curr) %>% 
    spread(indicator, value)%>% 
    replace(is.na(.), "") %>% 
    arrange(Facility,Week_End)
  
  
  write.table(df, paste0("Outputs/interagencyDash_", Sys.Date(), ".txt"), sep = "\t", row.names = FALSE)
  
  return(df)
  
}


GenerateDataQualityReport <- function(df,currentWeekEnd,df_quality)
{
  
  currentWeekEnd           <-  as.POSIXct(currentWeekEnd)
  
  df_quality <-  df %>% 
    mutate(TX_NEW_CURRENT = case_when(Week_End == date(currentWeekEnd) ~ TX_NEW, TRUE ~ ""),
           TX_NET_NEW_Montly_Target = case_when(Week_End == date(currentWeekEnd) ~ (8*as.numeric(TARG_WKLY_NETNEW)), TRUE ~ 0 )) %>% 
    select(FundingAgency, PrimePartner,Facility,
           TX_CURR_28_BASE, TX_CURR_28_TODATE, TX_NET_NEW_28_TODATE,
           TX_NEW_CURRENT,TX_NET_NEW_Montly_Target) %>% 
    gather(indicator, value,TX_CURR_28_BASE:TX_NET_NEW_Montly_Target, na.rm=TRUE) %>% 
    mutate(value = as.numeric(value)) %>% 
    drop_na(value) %>% 
    filter(value != 0) %>% 
    spread(indicator, value) %>% 
    mutate(change_TX_CURR = (TX_NET_NEW_28_TODATE/TX_CURR_28_BASE),
           NEW_vs_NET_NEW = (TX_NEW_CURRENT/TX_NET_NEW_28_TODATE),
           issue = case_when(change_TX_CURR >= 0.1 & NEW_vs_NET_NEW <= 0.25 ~
                               ">10% INCREASE IN TX_CURR, LOW TX_NEW vs NET_NEW %",
                             NEW_vs_NET_NEW <= 0.25 ~ "LOW %  TX_NEW vs NET_NEW",
                             TX_NET_NEW_28_TODATE < 0 ~"NEGATIVE NET NEW",
                             TRUE ~ "")) %>% 
    filter(issue != "")
  
  write.table(df_quality, paste0("Outputs/qualitycheck_", Sys.Date(), ".txt"), sep = "\t", row.names = FALSE)
  
  
  return(df_quality)
  
}

df <- GenerateInteragencyOutput(tx_curr_startOfSiyenza  = "2019-03-01",
                          startOfSiyenza = "2019-03-15",
                          endOfSiyenza = "2019-05-10",
                          currentWeekStart = "2019-03-30",
                          currentWeekEnd = "2019-04-05")


# tx_curr_startOfSiyenza  <-  as.POSIXct("2019-03-01")
# startOfSiyenza          <-  as.POSIXct("2019-03-15")
# endOfSiyenza            <-  as.POSIXct("2019-05-10")
# currentWeekStart        <-  as.POSIXct("2019-03-23")
# currentWeekEnd          <-  as.POSIXct("2019-03-29")

df_quality <-  GenerateDataQualityReport(df, currentWeekEnd = "2019-04-05")




