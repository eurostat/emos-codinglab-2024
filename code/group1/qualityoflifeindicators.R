### Quality of life indicators - overall experience of life

library(restatapi)
library(ggplot2)
library(reshape2)

#https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Quality_of_life_indicators_-_overall_experience_of_life#EU_citizens_were_rather_satisfied_with_their_lives_in_2022

#get the dataset
dsd<- get_eurostat_dsd("ilc_pw01")[concept=='geo']

ilc_pw01 <- get_eurostat_data("ilc_pw01", filters = c(unit = "RTG", isced11 = "TOTAL", indic_wb = "LIFESAT"), label=TRUE)


#Figure 1
#Satisfaction by country

excluded_countries <- c("European Union - 27 countries (from 2020)","Switzerland", "Norway", "Serbia", "Montenegro", "Türkiye")

countries <- subset(ilc_pw01, sex == "Total" & age == "16 years or over" & time == "2022" & !(geo %in% excluded_countries))

countries2<- subset(ilc_pw01, sex == "Total" & age == "16 years or over" & time == "2022")

EU_value=countries2$values[countries2$geo== "European Union - 27 countries (from 2020)"]

plot0 <- ggplot(countries, aes(x = reorder(geo, values[geo != "European Union - 27 countries (from 2020)"], decreasing = TRUE), y = values)) +
  geom_col(width = 0.6, fill = "#2644A7") +
  geom_hline(yintercept = EU_value, color = "#B09120",linetype = "solid",linewidth =1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10),breaks = seq(0, 10, by = 1)) +
  #scale_x_discrete(labels = country_names[unique(countries$geo)]) +
  scale_x_discrete(labels = countries$geo) +
  coord_cartesian(ylim = c(0, 10)) +
  #theme_minimal() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),         panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color="black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 0, color="black"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = "black", linetype = "solid"),
        #axis.line = element_blank(),
        #axis.line = element_line(color = "black"),
        axis.ticks = element_line(),
        axis.ticks.length.y  = unit(0, "cm")
        #axis.ticks = element_line()
  )+
  labs(x = NULL, y = NULL, title="Overall life satisfaction, 2022")

print(plot0)

#save the plot
library(openxlsx)

ggsave("my_plot.png", plot = plot0, width = 8, height = 6, dpi = 300)

#cant use the following code because the plot is not in a data.frame
write.xlsx(plot0, file = "plot0.xlsx", sheetName = "Plot0", rowNames = FALSE)




#Map 1
#Overall life satisfaction by age group, 2022
library(giscoR)
library(tmap)

ilc_pw01_map <- get_eurostat_data("ilc_pw01", filters = c(unit = "RTG", isced11 = "TOTAL", indic_wb = "LIFESAT"))

excluded_countries_map <- c("EU27_2020","CH", "NO", "RS", "ME", "TR")

countries_map <- ilc_pw01_map %>%
  filter(sex == "T" & age %in% c("Y16-19", "Y_GE65") & time == 2022 & !(geo %in% excluded_countries_map))

countries_diff <- countries_map %>%
  pivot_wider(names_from = age, values_from = values) %>%
  mutate(difference = `Y16-19` - `Y_GE65`)

map <- gisco_get_nuts(resolution = "10", nuts_level = "0", year = "2021")

merged_data <- map %>%
  left_join(countries_diff, by = c("NUTS_ID" = "geo"))


map1 <- tm_shape(merged_data) +
  tm_polygons("difference", 
              title = "Overall life satisfaction by age group, 2022", 
              subtitle= "Difference between the satisfaction level",
              palette = "RdYlBu",
              midpoint=NA,
              breaks = c(-Inf, -0.3, 0, 0.3, 1, Inf)) +
  tm_layout(legend.outside = FALSE)


print(map1)




#Figure 2

library(dplyr)
library(tidyr)

ilc_pw01_2 <- get_eurostat_data("ilc_pw01", filters = c(unit = "RTG", indic_wb = "LIFESAT"),label=TRUE)

excluded_countries2 <- c("Switzerland", "Norway", "Serbia", "Montenegro", "Türkiye", "Kosovo*")

education_countries <- subset(ilc_pw01_2, sex == "Total" & age == "16 years or over" & time == "2022" & isced11 != "All ISCED 2011 levels" & !(geo %in% excluded_countries2))

custom_colors <- c("Less than primary, primary and lower secondary education (levels 0-2)" = "#F8AE21","Upper secondary and post-secondary non-tertiary education (levels 3 and 4)" = "yellow",  "Tertiary education (levels 5-8)" = "#2644A7") 


eu_data <- subset(education_countries, geo == "European Union - 27 countries (from 2020)")
country_data <- subset(education_countries, geo != "European Union - 27 countries (from 2020)")


country_data <- country_data %>%
  arrange(desc(ifelse(isced11 == "Tertiary education (levels 5-8)", values, NA)))

ordered_data <- bind_rows(eu_data, country_data)

country_names <- unique(ordered_data$geo)



plot2 <- ggplot(ordered_data, aes(x = factor(geo, levels = country_names), y = values, fill = isced11)) +
  scale_fill_manual(values = custom_colors, labels = c("Less than secondary education", "Secondary education", "Tertiary education")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.4) +
  labs(title = "Life satisfaction by level of education, 2022", x = NULL, y = NULL, fill = "Education Level") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9), breaks = seq(0, 9, by = 1)) +
  coord_cartesian(ylim = c(0, 9)) +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 0, color = "black"),
        legend.position = "bottom",     
        legend.title = element_text(face = "bold"),
        axis.ticks = element_line(),
        axis.ticks.length.y = unit(0, "cm"),
        axis.line.x = element_line(colour = "black", linetype = "solid")) +
  guides(fill = guide_legend(override.aes = list(size = 1))) +
  scale_shape_manual(values = 16)+
  scale_x_discrete(labels = function(x) ifelse(x == 'European Union - 27 countries (from 2020)', 'EU', x))


print(plot2)





#Figure 3
#Overall life satisfaction by level of urbanisation 2022

ilc_pw02 <- get_eurostat_data("ilc_pw02", filters = c(unit = "RTG", indic_wb = "LIFESAT"), label=TRUE)

excluded_countries2 <- c("Switzerland", "Norway", "Serbia", "Montenegro", "Türkiye", "Kosovo*")

urbanisation_countries <- subset(ilc_pw02, time == "2022" & hhcomp== "Total" & quant_inc== "Total" & deg_urb != "Total" & !(geo %in% excluded_countries2) )

custom_colors4 <- c("Cities" = "#F8AE21","Towns and suburbs"= "yellow", "Rural areas" = "#2644A7") 
eu_data2 <- subset(urbanisation_countries, geo == "European Union - 27 countries (from 2020)")
country_data2 <- subset(urbanisation_countries, geo != "European Union - 27 countries (from 2020)")


country_data2 <- country_data2 %>%
  arrange(desc(ifelse(deg_urb == "Cities", values, NA)))

ordered_data2 <- bind_rows(eu_data2, country_data2)

country_names2 <- unique(ordered_data2$geo)



plot3<- ggplot(ordered_data2, aes(x = factor(geo, levels=country_names2), y = values, fill = deg_urb)) +
  scale_fill_manual(values = custom_colors4, labels = c("Cities", "Small Towns", "Rural Areas")) +
  geom_bar(stat = "identity",position = position_dodge(width = 0.6), width=0.4) +
  labs(title = "Overall life satisfaction by level of urbanisation,2022",x = NULL ,y = NULL, fill = "Urbanisation degree") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9),breaks = seq(0, 9, by = 1)) +
  coord_cartesian(ylim = c(0, 9)) +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color="black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 0, color="black"),
        legend.position = "bottom",     
        legend.title = element_text(face = "bold"),
        axis.ticks = element_line(),
        axis.ticks.length.y  = unit(0, "cm"),
        axis.line.x = element_line(colour = "black", linetype = "solid"))+
  guides(fill = guide_legend(override.aes = list(size = 1)))+
  scale_shape_manual(values = 16)+
  scale_x_discrete(labels = function(x) ifelse(x == 'European Union - 27 countries (from 2020)', 'EU', x))



print(plot3)


#Figure 4
#Overall life satisfaction by demographic characteristics at EU level, 2018 and 2022

library(reshape2)

household_countries <- subset(ilc_pw02, time == c("2018","2022") & hhcomp != "Total" & quant_inc != "Total" & deg_urb == "Total" )

hhcomp_data <- household_countries %>%
  filter(hhcomp != "Total") %>%
  mutate(category = hhcomp)

quant_inc_data <- household_countries %>%
  filter(quant_inc != "Total") %>%
  mutate(category = quant_inc)

combined_data <- bind_rows(hhcomp_data, quant_inc_data)

mean_satisfaction <- combined_data %>%
  group_by(category, time) %>%
  summarize(mean_values = mean(values, na.rm = TRUE))

satisfaction_wide <- dcast(mean_satisfaction, category ~ time, value.var = "mean_values")

satisfaction_wide$category <- factor(satisfaction_wide$category, levels = c("One adult", "Two adults", "Three or more adults", "Households without dependent children", "Households with dependent children", "First quintile", "Second quintile", "Third quintile", "Fourth quintile", "Fifth quintile"))


plot4 <- ggplot(satisfaction_wide, aes(x = category)) +
  geom_col(aes(y = `2022`, fill = "2022"), width = 0.4) +
  geom_point(aes(y = `2018`, color = "2018"), shape = 22, size = 3, fill = "blue") +
  scale_fill_manual(values = c("2022" = "gold3")) +
  scale_color_manual(values = c("2018" = "blue")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks = seq(0, 8, by = 1)) +
  labs(title = "Overall life satisfaction by demographic characteristics at EU level, 2018 and 2022", 
       x = NULL, y = NULL) +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black"),
        axis.text.y = element_text(vjust = 0.5, hjust = 0, color = "black"),
        axis.line.x = element_line(colour = "black", linetype = "solid")) +
  legend("bottom", legend = c("2018", "2022"), 
         fill = c("blue", "gold3"), 
         title = "Year")

print(plot4)



p <- ggplot(satisfaction_wide, aes(x = category))

p <- p + geom_col(aes(y = `2022`, fill = "2022"), width = 0.4)
p <- p + geom_point(aes(y = `2018`, color = "2018"), shape = 22, size = 3, fill = "blue")

p <- p + scale_fill_manual(values = c("2022" = "gold3"))
p <- p + scale_color_manual(values = c("2018" = "blue"))

p <- p + scale_y_continuous(expand = c(0, 0), limits = c(0, 8), breaks = seq(0, 8, by = 1))

p <- p + labs(title = "Overall life satisfaction by demographic characteristics at EU level, 2018 and 2022", 
              x = NULL, y = NULL)
p <- p + theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
               plot.title = element_text(hjust = 0.5), 
               panel.background = element_rect(fill = "white"), 
               panel.grid.major.x = element_blank(),
               panel.grid.minor = element_blank(), 
               axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, color = "black"),
               axis.text.y = element_text(vjust = 0.5, hjust = 0, color = "black"),
               axis.line.x = element_line(colour = "black", linetype = "solid"))

p <- p + theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Year", title.position = "top", title.hjust = 0.5),
         color = guide_legend(title = "Year", title.position = "top", title.hjust = 0.5))

print(p)
