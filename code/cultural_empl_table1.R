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

countries <- as.factor(c("EU27_2020", "BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", 
                         "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "RO", 
                         "SI", "SK", "FI", "SE", "IS", "NO", "CH", "UK", "ME", "MK", "RS", "TR"))

dataset <- get_eurostat_data(id="cult_emp_sex",date_filter=c(2019:2022))

filtered_data <- dataset %>% 
  filter(sex == "T") %>% 
  select(geo, unit, time, values) %>% 
  arrange(desc(time)) %>% 
  head()

kable_styling(
  kable(filtered_data, format = "html", escape = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE, background = "#FFF2CC") %>% # Estilizar la cabecera
    kableExtra::column_spec(1, bold = TRUE)  # Hacer la primera columna de países en negrita
  , full_width = FALSE, position = "left"
) %>%
  kableExtra::add_header_above(c(" " = 1, "Number" = 3))

#let's try another approach to spread the values across the table
dataset <- get_eurostat_data(id = "cult_emp_sex", date_filter = c(2019:2022))

filtered_data <- dataset %>% 
  filter(sex == "T") %>% 
  select(geo, unit, time, values) %>% 
  arrange(desc(time))

# Organizar los años en columnas
spread_data <- filtered_data %>%
  group_by(unit) %>%  # Agrupar por ubicación geográfica y unidad
  spread(key = time, value = values)

# Imprimir la tabla organizada
print(spread_data)


