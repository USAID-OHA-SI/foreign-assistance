## PROJECT:  USG - Foreign Assistance
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Spatial distribution of funding by sector
## DATE: 2020-05-08


# LIBRARIES -----------------------
library(tidyverse)
library(lubridate)
library(glitr)

# GLABAL VARS ----------------------------

datain <- "Data"
dataout <- "Dataout"

fae19 <- "FAE_FY19_transactions.csv"
fae20 <- "FAE_FY20_transactions.csv"

fae_files <- list.files(datain, full.names = TRUE)

# DATA ----------------------------

fae_data <- fae_files %>% 
    map_df(read_csv)

fae_data <- fae_data %>% 
    select(country_code, country_name, region_name, 
           income_group = income_group_name, 
           agency = funding_agency_acronym,
           sub_agency = subagency_acronym,
           channel = channel_name,
           channel_category = channel_category_name,
           channel_subcategory = channel_subcategory_name,
           dac_category = dac_category_name,
           dac_sector = dac_sector_name,
           dac_purpose = dac_purpose_name,
           funding_account = funding_account_name,
           assist_category = assistance_category_name,
           sector_name = USG_sector_name,
           aid_type = aid_type_group_name,
           activity_name,
           transaction_type = transaction_type_name,
           fiscal_year, current_amount, constant_amount
           )

fae_data %>% 
    colnames()

# Digging into the data

## Regions
fae_by_reg <- fae_data %>% 
    filter(fiscal_year == 2020) %>% 
    group_by(region_name) %>% 
    summarise(
        activities_count = n_distinct(activity_name),
        sectors_count = n_distinct(sector_name),
        aids_type = n_distinct(aid_type),
        current_amount = sum(current_amount, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(perc_current_amount = round(current_amount / sum(current_amount) * 100, 2)) %>% 
    arrange(desc(current_amount)) 

fae_by_reg %>% 
    filter(region_name != 'World') %>% 
    ggplot(aes(reorder(region_name, desc(perc_current_amount)), perc_current_amount)) +
    geom_col(fill = 'lightblue') +
    coord_flip() +
    labs(title = "USG - Foreign Assistance", 
         subtitle = "FY20 - % of global aid by region",
         x = NULL, y = NULL,
         caption = "USAID - GH/OHA/SIEI/SI") +
    si_style_xgrid()


## Countries
fae_by_cntry <- fae_data %>% 
    filter(fiscal_year == 2020) %>% 
    group_by(region_name, country_code, country_name) %>% 
    summarise(
        activities_count = n_distinct(activity_name),
        sectors_count = n_distinct(sector_name),
        aids_type = n_distinct(aid_type),
        current_amount = sum(current_amount, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(perc_current_amount = round(current_amount / sum(current_amount) * 100, 2)) %>% 
    arrange(desc(current_amount)) 

fae_by_cntry %>% #View()
    filter(region_name != 'World', perc_current_amount >= 1) %>% 
    ggplot(aes(reorder(country_code, desc(perc_current_amount)), perc_current_amount)) +
    geom_col(fill = 'lightblue') +
    facet_wrap(~region_name, ncol = 1, scales = "free") +
    labs(title = "USG - Foreign Assistance", 
         subtitle = "FY20 - % of global aid by country",
         x = NULL, y = NULL,
         caption = "USAID - GH/OHA/SIEI/SI") +
    si_style_ygrid()


## Sectors

fae_data %>%
    distinct(aid_type)

fae_data %>%
    distinct(dac_category)

fae_data %>%
    distinct(dac_sector)

fae_data %>%
    filter(dac_category == 'Health and Population') %>% 
    distinct(dac_sector)
    
fae_health <- fae_data %>% 
    filter(fiscal_year == 2020, aid_type %in% c('Project-Type', 'Technical Assistance'), dac_category == 'Health and Population') %>% 
    filter(country_code != 'WLD') %>% 
    group_by(country_code, country_name, dac_sector) %>% 
    summarise(
        activities_count = n_distinct(activity_name),
        current_amount = sum(current_amount, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(perc_current_amount = round(current_amount / sum(current_amount) * 100, 2)) %>% 
    arrange(desc(current_amount)) #%>% View()

fae_health_sectors <- fae_data %>% 
    filter(fiscal_year == 2020, aid_type %in% c('Project-Type', 'Technical Assistance'), dac_category == 'Health and Population') %>% 
    filter(country_code != 'WLD') %>% 
    group_by(country_code, country_name) %>% 
    summarise(
        sectors_count = n_distinct(dac_sector),
        activities_count = n_distinct(activity_name),
        current_amount = sum(current_amount, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(perc_current_amount = round(current_amount / sum(current_amount) * 100, 2)) %>% 
    arrange(desc(current_amount)) #%>% View()
