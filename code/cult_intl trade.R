# Article 1
# Culture statistics - international trade in cultural goods
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Culture_statistics_-_international_trade_in_cultural_goods


library(restatapi)
library(kableExtra)
library(dplyr)
library(tidyr) # to pivot tables

id <- "cult_trd_prd"


#### Table 1: Extra-EU trade in cultural goods, EU, 2017 and 2022####

# Filters

tab1_dates <- c(2017,2022) 
tab1_country_keys <- c("EU27_2020") # , "EU28") 
# Seems like EU27_2020 is sufficient because it has values before 2020 also,
# possibly with deduction of values from Great Britain.

tab1_partner_keys <- c("EXT_EU27_2020") #, "EXT_EU28")


# Retrieve filtered data
tab1_data <- get_eurostat_data(id,
                               filters = list(geo = tab1_country_keys, 
                                              partner = tab1_partner_keys),
                               date_filter = tab1_dates,
                               label = T
                               )



# Creating the table

# Lets first group the results for each category and time to 
# make the creation process easier

tab1_2017 <- tab1_data[tab1_data$time == 2017 & tab1_data$unit == "Thousand euro",]
tab1_2022 <- tab1_data[tab1_data$time == 2022 & tab1_data$unit == "Thousand euro",]

tab1_2017 <- tab1_2017 %>%
             pivot_wider( names_from = stk_flow,
                          values_from = values,
                          values_fn = list(values = first)
                        ) %>%
             mutate(Trade_balance = Exports - Imports,
                    Cover_ratio = Exports/Imports) %>%
             select(-c(time, unit, partner, geo))
    
tab1_2022 <- tab1_2022 %>%
  pivot_wider( names_from = stk_flow,
               values_from = values,
               values_fn = list(values = first)
  ) %>%
  mutate(Trade_balance = Exports - Imports,
         Cover_ratio = Exports/Imports) %>%
  select(-c(time, unit, partner, geo))   

tab1_combined <- merge(tab1_2017, tab1_2022, by="prod_ct", suffixes = c("_2017", "_2022"))


tab1_combined <- tab1_combined %>%
                 mutate(Rel_ch_ex = round(100* (Exports_2022 - Exports_2017) / Exports_2017,1),
                        Rel_ch_im = round(100*(Imports_2022 - Imports_2017) / Imports_2017,1)
                        )

# Rounding values to fit the table format

tab1_combined <- tab1_combined %>%
                 mutate(Exports_2017 = round(Exports_2017/1000,1),
                        Imports_2017 = round(Imports_2017/1000,1),
                        Trade_balance_2017 = round(Trade_balance_2017/1000, 1),
                        Cover_ratio_2017 = round(Cover_ratio_2017, 2),
                        
                        Exports_2022 = round(Exports_2022/1000,1),
                        Imports_2022 = round(Imports_2022/1000,1),
                        Trade_balance_2022 = round(Trade_balance_2022/1000, 1),
                        Cover_ratio_2022 = round(Cover_ratio_2022, 2)
                        )
                                                                  



# Trying to recreate the same style/colour as in the article

styled_table <- tab1_combined %>%
  kable("html", escape = FALSE, align = rep('c', ncol(tab1_combined)), 
        col.names = c(" ", "2017 Exports (€ million)", "2017 Imports (€ million)", 
                      "2017 Trade Balance", "2017 Cover Ratio", "2022 Exports (€ million)", 
                      "2022 Imports (€ million)", "2022 Trade Balance", "2022 Cover Ratio", 
                      "Relative Change Exports (%)", "Relative Change Imports (%)"),
        table.attr = "style='font-family: Arial; font-size: 14px; border-collapse: collapse;'",
        format.args = list(decimal.mark = ',', big.mark = ' ')) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  add_header_above(
    c(
      " " = 1, 
      "2017" = 4, 
      "2022" = 4, 
      "Relative change in value, 2022 to 2017" = 2
    )
  ) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(1:11, border_right = TRUE, border_left = TRUE) %>%
  row_spec(0, bold = TRUE, background = "#E1D5E7", color = "black")


print(styled_table)





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

