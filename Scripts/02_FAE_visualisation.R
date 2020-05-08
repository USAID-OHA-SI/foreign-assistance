## PROJECT:  USG - Foreign Assistance
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Spatial distribution of funding by sector
## DATE: 2020-05-08


# LIBRARIES ------------------------------
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(glitr)

# Dependencies ---------------------------

source("Scripts/01_FAE_transactions.R")

# GLABAL VARS ----------------------------

datain <- "Data"
dataout <- "Dataout"

# FUNCTIONS ------------------------------
ken = ne_countries(country = "kenya")

sp::plot(ken)

geoviz <- function(cntry_code, cntry_name, sectors, acts, budget) {
    
    print(cntry_code)
    
    #geo <- ne_countries(country = name, scale = 'medium', returnclass = 'sf')

    # viz <- geo %>%
    #     ggplot() +
    #     geom_sf()
    
    #viz = sp::plot(geo)

    #print(viz)

    #return(viz)
}

# Data Viz -------------------------------

fae_health_sectors %>% 
    glimpse()

fae_health_sectors %>% #View()
    filter(sectors_count == 1) %>% 
    apply(1, function(x) {
        geoviz(as.list(x))
    })


