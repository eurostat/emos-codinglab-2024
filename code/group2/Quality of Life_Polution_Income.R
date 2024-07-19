#Comment:Still need to figure out how to do the spaces between the countries 
# and still need to adjust the colours of the graph 
# and change names of the income groups
# and add footnotes


#Recommended Packages
library(restatapi)
library(giscoR)

library(data.table)
library(chron)
library(reshape2)

library(kableExtra)
library(ggplot2)
library(tmap)

library(highcharter)
library(plotly)

#Quality of Life Indicators: Reported Noise from Neighbours grouped by income

id <- get_eurostat_data("ilc_mddw01",date_filter=2020,flags=T)
names(id)

#ordering and filtering of data
id <- subset(id, !(geo %in% c("EA18", "EA19")))
id <- id[order(-id$values), ]
id$geo <- factor(id$geo, levels = unique(id$geo))
id <- id[!(id$incgrp %in% "A_MD60"), ]

#plotting
ggplot(id, aes(x = geo, y = values, fill = incgrp)) +
  theme_minimal() +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Population reporting noise from neighbours or from the street, by income situation, 2020")


