# Article 1
# Culture statistics - international trade in cultural goods
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Culture_statistics_-_international_trade_in_cultural_goods


library(restatapi)
id <- "cult_trd_prd"


#### Table 1 filters ####

tab1_dates <- c(2017,2022) 
tab1_country_keys <- c("EU27_2020", "EU28")
tab1_partner_keys <- c("EXT_EU27_2020", "EXT_EU28")


# Retrieve filtered data

tab1_data <- get_eurostat_data(id,
                               filters = list(geo = tab1_country_keys, 
                                              partner = tab1_partner_keys),
                               date_filter = tab1_dates,
                               label = T
)



