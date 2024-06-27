#############################################################
### Quality of life indicators - overall experience of life
#############################################################
#https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Quality_of_life_indicators_-_overall_experience_of_life#EU_citizens_were_rather_satisfied_with_their_lives_in_2022
############################################################

#install and load packages
pacman::p_load(restatapi, 
               giscoR,
               grid,
               ggplot2,
               reshape2,
               tmap,
               dplyr,
               tidyr,
               cowplot,
               RColorBrewer,
               highcharter,  
               tidyverse)



#get the dataset
dsd<- get_eurostat_dsd("ilc_pw01")[concept=='geo']

ilc_pw01 <- get_eurostat_data("ilc_pw01", filters = c(unit = "RTG", isced11 = "TOTAL", indic_wb = "LIFESAT"), label=TRUE)

#########
#Figure 1
#########
#Overall life satisfaction by country, 2022

excluded_countries <- c("European Union - 27 countries (from 2020)","Switzerland", "Norway", "Serbia", "Montenegro", "Türkiye")

#countries to plot
countries <- subset(ilc_pw01, sex == "Total" & age == "16 years or over" & time == "2022" & !(geo %in% excluded_countries))

#countries subset to select the EU_value for the plot
countries_subset<- subset(ilc_pw01, sex == "Total" & age == "16 years or over" & time == "2022")

EU_value=countries_subset$values[countries_subset$geo== "European Union - 27 countries (from 2020)"]

#plot Figure 1 and save it
library(xlsx)

png("Figure1.png")


plot0 <- ggplot(countries,aes(x = reorder(geo, values[geo != "European Union - 27 countries (from 2020)"], decreasing = TRUE), y = values)) +
  geom_col(width = 0.6, fill = "#2644A7") +
  geom_hline(yintercept = EU_value, color = "#B09120",linetype = "solid",linewidth =1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10),breaks = seq(0, 10, by = 1)) +
  scale_x_discrete(labels = countries$geo) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color="black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 0, color="black"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = "black", linetype = "solid"),
        axis.ticks = element_line(),
        axis.ticks.length.y  = unit(0, "cm"),
        plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.margin = unit(c(1, 1, 3, 1), "lines"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, hjust = 0),
        legend.box.margin = margin(t = 10, unit = "pt"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),)+
  labs(x = NULL, y = NULL, title="Overall life satisfaction, 2022", subtitle= "(mean scale 0-10)", caption= "Figure 1: Overall life satisfaction, 2022\n(mean scale 0-10)\nSource: Eurostat: ilc_pw01")+
  scale_fill_manual(name = NULL,
                   values = c("Overall life satisfaction" = "#2644A7", "EU average" = "#B09120"),
                  labels = c("Overall life satisfaction", "EU average"))

print(plot0)

dev.off()

#plot0

wb_0 <- createWorkbook()
sheet_1 <- createSheet(wb_0, sheetName = "Overall life satisfaction 2022")
addPicture("Figure1.png", sheet_1, scale = 1, startRow = 1, startColumn = 1)

saveWorkbook(wb_0, "Overall life satisfaction.xlsx")




############
#Map 1
###########
#Overall life satisfaction by age group, 2022

library(sf) 

#load the data 
ilc_pw01_map <- get_eurostat_data("ilc_pw01", filters = c(unit = "RTG", isced11 = "TOTAL", indic_wb = "LIFESAT"))

#filter data
excluded_countries_map <- c("EU27_2020","CH", "NO", "RS", "ME", "TR")

countries_map <- ilc_pw01_map %>%
  filter(sex == "T" & age %in% c("Y16-19", "Y_GE65") & time == 2022 & !(geo %in% excluded_countries_map))

#compute the difference between satisfaction level of 16-29 and above 65 years old
#and new dataset to use
countries_diff <- countries_map %>%
  pivot_wider(names_from = age, values_from = values) %>%
  mutate(difference = `Y16-19` - `Y_GE65`)

#get nuts data
#only EU countries
#nuts <- gisco_get_nuts(year = 2021, nuts_level = 0, resolution = "01")

#EU countries and neighbors
nuts <- gisco_get_countries(year = 2020,  resolution = "01")

#merge the nuts data with the overall life satisfaction differences by age data
merged_data <- merge(nuts, countries_diff, by.x = "CNTR_ID", by.y = "geo", all.x = TRUE)


#add a column for the legend 
merged_data$bins <- cut(
  merged_data$difference,
  breaks = c(-Inf, -0.3, 0, 0.3, 1, Inf),
  labels = c("<-0.3", "-0.3-<0", "0-<0.3", "0.3-<1", ">=1"),
  include.lowest = TRUE
)

centroids <- st_centroid(merged_data)


#create a map for Malta
malta_data <- merged_data %>% filter(CNTR_ID == "MT")

malta_data <- st_transform(malta_data, crs = 4326)

malta_map1 <- ggplot(malta_data) +
  geom_sf(aes(fill = bins)) +
  geom_sf_text(aes(label = round(difference, 1)), size = 2.5, color = "black") +
  scale_fill_manual(values = c(
    "<-0.3" = "#FF5F1D",
    "-0.3-<0" = "#FFAC81",
    "0-<0.3" = "#CDDCF0",
    "0.3-<1" = "#829BCD",
    ">=1" = "#466EB4"), 
    na.value = "lightgrey") +
  labs(fill = "") + 
  guides(fill = guide_colorbar(reverse = TRUE)) + 
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "none",
        plot.margin = unit(c(3, 1, 1, 1), "cm") ) + 
  annotate("text", x = -Inf, y = Inf, label = "Malta", vjust = 1.5, hjust = -1.5, size = 6, fontface = "plain")


plot(malta_map1)


#create a map of Europe
europe_map <- ggplot(merged_data) +
  geom_sf(aes(fill = bins)) +
  geom_sf_text(aes(label = round(difference, 1)), size = 2.5, color = "black") +
  scale_fill_manual(values = c(
    "<-0.3" = "#FF5F1D",
    "-0.3-<0" = "#FFAC81",
    "0-<0.3" = "#CDDCF0",
    "0.3-<1" = "#829BCD",
    ">=1" = "#466EB4"),
  na.value = "lightgrey"
  ) +
  theme_bw() +
  coord_sf(crs = 3035, xlim = c(2377294, 7453440), ylim = c(1313597, 5628510)) +
  labs(title = "Overall life satisfaction by age group, 2022", 
       subtitle= "Difference between the satisfaction level of 16-29 and 65+ year olds (mean scale 0-10)") +
  guides(fill = guide_legend(keywidth = unit(0.6, "cm"), keyheight = unit(0.3, "cm"), drop = TRUE)) +
  annotate("text", x = 2380000, y = 1300000, 
           label = "Source: Eurostat: ilc_pw01",
           size = 2, color = "black", hjust = 0) +
  theme(legend.position = c(0.7, 0.55), 
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        panel.background = element_blank()
  )

plot(europe_map)


#combine the map of Europe and Malta and save it

png("Map1.png")

map1<- ggdraw(europe_map) +
  draw_plot(malta_map1, 0.6, 0.6, .4, .4) 

print(map1)

dev.off()

#map1

wb_map <- createWorkbook()
sheet_map <- createSheet(wb_map, sheetName = "Life satisfaction by age group")
addPicture("Map1.png", sheet_map, scale = 1, startRow = 1, startColumn = 1)

saveWorkbook(wb_map, "Life satisfaction by age group.xlsx")




#########
#Figure 2
#########

#Life satisfaction by level of education, 2022

#get the data
ilc_pw01_2 <- get_eurostat_data("ilc_pw01", filters = c(unit = "RTG", indic_wb = "LIFESAT"),label=TRUE)

excluded_countries2 <- c("Switzerland", "Norway", "Serbia", "Montenegro", "Türkiye", "Kosovo*")

education_countries <- subset(ilc_pw01_2, sex == "Total" & age == "16 years or over" & time == "2022" & isced11 != "All ISCED 2011 levels" & !(geo %in% excluded_countries2))

#colors for the plot
custom_colors <- c("Less than primary, primary and lower secondary education (levels 0-2)" = "#F8AE21","Upper secondary and post-secondary non-tertiary education (levels 3 and 4)" = "yellow",  "Tertiary education (levels 5-8)" = "#2644A7") 

#countries subset to select the EU_value and the other countries for the plot
eu_data <- subset(education_countries, geo == "European Union - 27 countries (from 2020)")
country_data <- subset(education_countries, geo != "European Union - 27 countries (from 2020)")

#order the countries by the tertiary education level
country_data <- country_data %>%
  arrange(desc(ifelse(isced11 == "Tertiary education (levels 5-8)", values, NA)))

#re-arrange the dataset
ordered_data <- bind_rows(eu_data, country_data)


country_names <- unique(ordered_data$geo)


#plot and save it
png("Figure2.png")


plot2 <- ggplot(ordered_data, aes(x = factor(geo, levels = country_names), y = values, fill = isced11)) +
  scale_fill_manual(values = custom_colors, labels = c("Less than secondary education", "Secondary education", "Tertiary education")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.4) +
  labs(title = "Life satisfaction by level of education, 2022",subtitle ="mean scale (0-10)", x = NULL, y = NULL, caption="Source: Eurostat: ilc_pw01") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9), breaks = seq(0, 9, by = 1)) +
  coord_cartesian(ylim = c(0, 9)) +
  theme(plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.margin = unit(c(1, 1, 3, 1), "lines"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, hjust = 0),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 0, color = "black"),
        axis.ticks = element_line(),
        axis.ticks.length.y = unit(0, "cm"),
        axis.line.x = element_line(colour = "black", linetype = "solid"),
        legend.position = "bottom",     
        legend.title = element_blank(),
        legend.text= element_text(face="bold")) +
  guides(fill = guide_legend(keywidth = unit(0.3, "cm"), keyheight = unit(0.2, "cm")))+
  scale_shape_manual(values = 16)+
  scale_x_discrete(labels = function(x) ifelse(x == 'European Union - 27 countries (from 2020)', 'EU', x))

print(plot2)

dev.off()

#plot2

wb_2 <- createWorkbook()
sheet_2 <- createSheet(wb_2, sheetName = "Life satisfaction by level of education")
addPicture("Figure2.png", sheet_2, scale = 1, startRow = 1, startColumn = 1)

saveWorkbook(wb_2, "Life satisfaction by level of education.xlsx")





#########
#Figure 3
#########

#Overall life satisfaction by level of urbanisation 2022

#get the dataset
ilc_pw02 <- get_eurostat_data("ilc_pw02", filters = c(unit = "RTG", indic_wb = "LIFESAT"), label=TRUE)

excluded_countries2 <- c("Switzerland", "Norway", "Serbia", "Montenegro", "Türkiye", "Kosovo*")

urbanisation_countries <- subset(ilc_pw02, time == "2022" & hhcomp== "Total" & quant_inc== "Total" & deg_urb != "Total" & !(geo %in% excluded_countries2) )

#colors for the plot
custom_colors_urb <- c("Cities" = "#F8AE21","Towns and suburbs"= "yellow", "Rural areas" = "#2644A7") 

#countries subset to select the EU_value and the other countries for the plot
eu_data_urb <- subset(urbanisation_countries, geo == "European Union - 27 countries (from 2020)")
country_data_urb <- subset(urbanisation_countries, geo != "European Union - 27 countries (from 2020)")

#order the countries by the cities degree of urbanisation
country_data_urb <- country_data_urb %>%
  arrange(desc(ifelse(deg_urb == "Cities", values, NA)))

#re-arrange the data
ordered_data_urb <- bind_rows(eu_data_urb, country_data_urb)
country_names_urb <- unique(ordered_data_urb$geo)


#plot and save it

png("Figure3.png")

plot3 <- ggplot(ordered_data_urb, aes(x = factor(geo, levels = country_names_urb), y = values, fill = deg_urb)) +
  scale_fill_manual(values = custom_colors_urb, labels = c("Cities", "Small Towns", "Rural Areas")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.4) +
  labs(title = "Overall life satisfaction by level of urbanisation 2022", subtitle ="mean scale (0-10)", x = NULL, y = NULL, caption="Source: Eurostat: ilc_pw02") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9), breaks = seq(0, 9, by = 1)) +
  coord_cartesian(ylim = c(0, 9)) +
  theme(plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.margin = unit(c(1, 1, 3, 1), "lines"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, hjust = 0),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 0, color = "black"),
        axis.ticks = element_line(),
        axis.ticks.length.y = unit(0, "cm"),
        axis.line.x = element_line(colour = "black", linetype = "solid"),
        legend.position = "bottom",     
        legend.title = element_blank(),
        legend.text= element_text(face="bold")) +
  guides(fill = guide_legend(keywidth = unit(0.3, "cm"), keyheight = unit(0.2, "cm")))+
  scale_shape_manual(values = 16)+
  scale_x_discrete(labels = function(x) ifelse(x == 'European Union - 27 countries (from 2020)', 'EU', x))

print(plot3)

dev.off()

#plot3

wb_3 <- createWorkbook()
sheet_3 <- createSheet(wb_3, sheetName = "Life satisfaction by level of urbanisation")
addPicture("Figure3.png", sheet_3, scale = 1, startRow = 1, startColumn = 1)

saveWorkbook(wb_3, "Life satisfaction by level of urbanisation.xlsx")





#########
#Figure 4
#########

#Overall life satisfaction by demographic characteristics at EU level, 2018 and 2022

#get the dataset
household_countries <- subset(ilc_pw02, time == c("2018","2022") & hhcomp != "Total" & quant_inc != "Total" & deg_urb == "Total" & geo== "European Union - 27 countries (from 2020)" )

#data on the household composition
hhcomp_data <- household_countries %>%
  filter(hhcomp != "Total") %>%
  mutate(category = hhcomp)

#data on the income
quant_inc_data <- household_countries %>%
  filter(quant_inc != "Total") %>%
  mutate(category = quant_inc)

#data on the gender (ilc_pw01)
gender_data <- subset(ilc_pw01, sex %in% c("Males", "Females") & age == "16 years or over" & time %in% c("2018", "2022") & geo == "European Union - 27 countries (from 2020)") %>%
  mutate(category = sex)

combined_data <- bind_rows(hhcomp_data,gender_data, quant_inc_data)

#only one row per category and year by aggregating the data
aggregated_data <- combined_data %>%
  group_by(category, time) %>%
  summarise(values = mean(values, na.rm = TRUE)) %>%
  ungroup()

#split the aggregated data in 2018 and 2022
data_2018 <- filter(aggregated_data, time == 2018)
data_2022 <- filter(aggregated_data, time == 2022)

#add spacing rows for a better visualization in the plot
cat_1_space <- length(unique(hhcomp_data$category))
cat_2_space <- cat_1_space + length(unique(gender_data$category))

data_2022 <- data_2022 %>%
  mutate(category = as.character(category)) %>%
  add_row(category = "", time = factor(2022, levels = levels(data_2022$time)), values = NA, .before = (cat_1_space+1)) %>%
  add_row(category = " ", time = factor(2022, levels = levels(data_2022$time)), values = NA, .before = (cat_2_space+2))

data_2018 <- data_2018 %>%
  mutate(category = as.character(category)) %>%
  add_row(category = "", time = factor(2018, levels = levels(data_2018$time)), values = NA, .before = (cat_1_space+1)) %>%
  add_row(category = " ", time = factor(2018, levels = levels(data_2018$time)), values = NA, .before = (cat_2_space+2))

data_2022$category <- factor(data_2022$category, levels = unique(data_2022$category))
data_2018$category <- factor(data_2018$category, levels = unique(data_2018$category))


#plot and save it

png("Figure4.png")


plot4 <- ggplot() +
  geom_bar(data = data_2022, aes(x = category, y = values, fill = time), 
           stat = "identity", width = 0.3) +
  geom_point(data = data_2018, aes(x = category, y = values, color = "2018"), 
             shape = 15, size = 3) +
  labs(title = "Overall life satisfaction by demographic characteristics at EU level, 2018 and 2022", 
       subtitle = "mean scale (0-10)", x = NULL, y = NULL,caption="All data are estimates\nSource: Eurostat: ilc_pw01, ilc_pw02") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks = seq(0, 8, by = 1)) +
  coord_cartesian(ylim = c(0, 8)) +
  theme(plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.margin = unit(c(1, 1, 3, 1), "lines"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, hjust = 0),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 0, color = "black"),
        axis.ticks = element_line(),
        axis.ticks.length.y = unit(0, "cm"),
        axis.line.x = element_line(colour = "black", linetype = "solid"),
        legend.position = "bottom",     
        legend.title = element_blank(),
        legend.text = element_text(face = "bold")) +
  scale_fill_manual(values = c("2022" = "gold3")) +
  scale_color_manual(values = c("2018" = "blue")) +
  guides(fill = guide_legend(title = "Year"))

print(plot4)

dev.off()

#plot4

wb_4 <- createWorkbook()
sheet_4 <- createSheet(wb_4, sheetName = "Life satisfaction by demographic")
addPicture("Figure4.png", sheet_4, scale = 1, startRow = 1, startColumn = 1)

saveWorkbook(wb_4, "Life satisfaction by demographic.xlsx")



















