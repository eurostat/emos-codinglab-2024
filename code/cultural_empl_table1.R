#############################################################
# Article "Culture statistics - cultural employment"
# URL: https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Culture_statistics_-_cultural_employment
#############################################################


library(restatapi)
library(data.table)
library(reshape2)
library(kableExtra)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)

id <- "cult_emp_sex"
get_eurostat_dsd(id)
get_eurostat_data(id,label=T,name=T,keep_flags=T)

#first try to reproduce the table

dataset <- get_eurostat_data(id="cult_emp_sex",date_filter=c(2019:2022))

filtered_data <- dataset %>% 
  filter(sex == "T") %>% 
  select(geo, unit, time, values) %>% 
  arrange(desc(time)) %>% 
  head()

kable_styling(
  kable(filtered_data, format = "html", escape = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE, background = "#FFF2CC") %>% 
    kableExtra::column_spec(1, bold = TRUE)  # first column in bold
  , full_width = FALSE, position = "left"
) %>%
  kableExtra::add_header_above(c(" " = 1, "Number" = 3))

#spread the values across the table
filtered_data <- dataset %>% 
  filter(sex == "T") %>% 
  select(geo, unit, time, values) %>% 
  arrange(desc(time))

spread_data <- filtered_data %>%
  group_by(unit) %>%  
  spread(key = time, value = values)

print(spread_data)

#now the years appear on the columns, let's make two different for each unit variable
pc_emp <- spread_data[spread_data$unit == "PC_EMP", ]
ths_per <- spread_data[spread_data$unit == "THS_PER", ]

#doubt: don't know how to transpose the data to have the unit variables as the header

