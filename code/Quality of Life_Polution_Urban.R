#Comment:Still need to figure out how to do the spaces between the countries 
# and still need to adjust the colours of the graph 
# and change names of the urbanization groups 

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

#Quality of Life Indicators: Reported Noise from Neighbours

id <- get_eurostat_data("ilc_mddw05",date_filter=2020,flags=T)
names(id)

#ordering and filtering of data
id <- subset(id, !(geo %in% c("EA18", "EA19")))
id <- id[order(-id$values), ]
id$geo <- factor(id$geo, levels = unique(id$geo))


#plotting
plot1<- ggplot(id, aes(x = geo, y = values, fill = deg_urb )) +
  theme_minimal() +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Population reporting noise from neighbours or from the street, by degree of urbanisation, 2020", y = "%")

plot1 <- ggplotly(plot1)

plot1


