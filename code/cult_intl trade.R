# Article 1
# Culture statistics - international trade in cultural goods
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Culture_statistics_-_international_trade_in_cultural_goods


library(restatapi)
id <- "cult_trd_prd"


#### Table 1: Extra-EU trade in cultural goods, EU, 2017 and 2022####

# Filters

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





#### Figure 1: Evolution of extra-EU imports and exports in cultural goods and in total goods, EU, 2011-2022 ####

# Filters
fig1_id2 <- "ext_lt_introEU27_2020"
fig1_id2_type_key <- "TOTAL" 
fig1_id2_country_keys <- "EU27_2020"
fig1_id2_exp_keys <- c("MIO_EXP_VAL", "MIO_IMP_VAL")

fig1_dates <- 2011:2022
fig1_country_keys <- c("EU27_2020", "EU28")
fig1_partner_keys <- c("EXT_EU27_2020", "EXT_EU28")

# Retrieve filtered data
fig1_data <- get_eurostat_data(id,
                               filters = list(geo = fig1_country_keys, 
                                              partner = fig1_partner_keys),
                               date_filter = fig1_dates,
                               label = T
                               )

fig1_data_2 <- get_eurostat_data(fig1_id2,
                                 filters=
                                   list(geo = fig1_id2_country_keys,
                                        sitc06 = fig1_id2_type_key,
                                        indic_et = fig1_id2_exp_keys),
                                 date_filter = fig1_dates,
                                 label = T
                                )


#### Table 2: International trade in cultural goods, 2017 and 2022 ####

# Filters 
tab2_id <- "cult_trd_prd"
tab2_dates <- c(2017,2022)

# Retrieve filtered data
tab2_data <- get_eurostat_data(tab2_id,
                               date_filter = tab2_dates,
                               label = T)




