### Quality of life indicators - overall experience of life

library(restatapi)
library(ggplot2)
library(reshape2)

#get the dataset
ilc_pw01 <- get_eurostat_data("ilc_pw01", filters = c(unit = "RTG", isced11 = "TOTAL", indic_wb = "LIFESAT"))

eu_iso_a2 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL" ,"ES", "FI","FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
               "RO", "SE", "SI", "SK" ,"EU27_2020")

ilc_pw01 <- ilc_pw01[(ilc_pw01$geo %in% eu_iso_a2), ]

country_names <- toupper(c("AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria", "CY" = "Cyprus", "CZ" = "Czech Republic", "DE" = "Germany", "DK" = "Denmark", "EE" = "Estonia", "EL" = "Greece", "ES" = "Spain", "FI" = "Finland", "FR" = "France", "HR" = "Croatia", "HU" = "Hungary", "IE" = "Ireland", "IT" = "Italy", "LT" = "Lithuania", "LU" = "Luxembourg", "LV" = "Latvia", "MT" = "Malta", "NL" = "Netherlands", "PL" = "Poland", "PT" = "Portugal", "RO" = "Romania", "SE" = "Sweden","SI" = "Slovenia", "SK" = "Slovakia"))

filtered_countries <- countries[countries$geo %in% names(country_names), ]

#Figure 1
#Satisfaction by country
countries <- subset(ilc_pw01, sex == "T" & age == "Y_GE16" & time == "2022" & geo != "EU27_2020")

countries2<- subset(ilc_pw01, sex == "T" & age == "Y_GE16" & time == "2022")

EU_value=countries2$values[countries2$geo== "EU27_2020"]

plot0 <- ggplot(countries, aes(x = reorder(geo, values[geo != "EU27_2020"], decreasing = TRUE), y = values)) +
  geom_col(width = 0.6, fill = "#2644A7") +
  geom_hline(yintercept = EU_value, color = "#F8AE21",linetype = "solid",linewidth =1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10),breaks = seq(0, 10, by = 1)) +
  #scale_x_discrete(labels = country_names[unique(countries$geo)]) +
  scale_x_discrete(labels = country_names[unique(toupper(filtered_countries$geo))]) +
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


#Map 1
#Overall life satisfaction by age group, 2022






#Figure 2
ilc_pw01_2 <- get_eurostat_data("ilc_pw01", filters = c(unit = "RTG", indic_wb = "LIFESAT"))

eu_iso_a2 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL" ,"ES", "FI","FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
               "RO", "SE", "SI", "SK" ,"EU27_2020")

ilc_pw01_2 <- ilc_pw01_2[(ilc_pw01_2$geo %in% eu_iso_a2), ]

country_names <- toupper(c("AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria", "CY" = "Cyprus", "CZ" = "Czech Republic", "DE" = "Germany", "DK" = "Denmark", "EE" = "Estonia", "EL" = "Greece", "ES" = "Spain", "FI" = "Finland", "FR" = "France", "HR" = "Croatia", "HU" = "Hungary", "IE" = "Ireland", "IT" = "Italy", "LT" = "Lithuania", "LU" = "Luxembourg", "LV" = "Latvia", "MT" = "Malta", "NL" = "Netherlands", "PL" = "Poland", "PT" = "Portugal", "RO" = "Romania", "SE" = "Sweden","SI" = "Slovenia", "SK" = "Slovakia"))

#filtered_countries <- countries[countries$geo %in% names(country_names), ]

education_countries <- subset(ilc_pw01_2, sex == "T" & age == "Y_GE16" & time == "2022" & isced11 != "TOTAL" )

custom_colors <- c("ED0-2" = "#F8AE21","ED3_4" = "yellow",  "ED5-8" = "#2644A7") 

#try to put EU level first
#education_countries$geo <- factor(education_countries$geo, levels = c("EU27_2020", levels(education_countries$geo)))
#try to order the life satisfaction rate by the tertiary education 

plot2<- ggplot(education_countries, aes(x = reorder(geo, desc(isced11 == "ED5-8")), y = values, fill = isced11)) +
  scale_fill_manual(values = custom_colors, labels = c("Less than secondary education", "Secondary education", "Tertiary education")) +
  geom_bar(stat = "identity",position = position_dodge(width = 0.6), width=0.4) +
  labs(title = "Life satisfaction by level of education,2022",x = NULL ,y = NULL, fill = "Education Level") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9),breaks = seq(0, 9, by = 1)) +
  scale_x_discrete(labels = country_names[unique(toupper(filtered_countries$geo))]) +
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
  scale_shape_manual(values = 16)


print(plot2)


#Figure 3
#Overall life satisfaction by level of urbanisation 2022

ilc_pw02 <- get_eurostat_data("ilc_pw02", filters = c(unit = "RTG", indic_wb = "LIFESAT"))

eu_iso_a2 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL" ,"ES", "FI","FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
               "RO", "SE", "SI", "SK" ,"EU27_2020")

ilc_pw02 <- ilc_pw02[(ilc_pw02$geo %in% eu_iso_a2), ]

country_names <- toupper(c("AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria", "CY" = "Cyprus", "CZ" = "Czech Republic", "DE" = "Germany", "DK" = "Denmark", "EE" = "Estonia", "EL" = "Greece", "ES" = "Spain", "FI" = "Finland", "FR" = "France", "HR" = "Croatia", "HU" = "Hungary", "IE" = "Ireland", "IT" = "Italy", "LT" = "Lithuania", "LU" = "Luxembourg", "LV" = "Latvia", "MT" = "Malta", "NL" = "Netherlands", "PL" = "Poland", "PT" = "Portugal", "RO" = "Romania", "SE" = "Sweden","SI" = "Slovenia", "SK" = "Slovakia"))


urbanisation_countries <- subset(ilc_pw02, time == "2022"& hhcomp== "TOTAL"& quant_inc== "TOTAL" & deg_urb != "TOTAL" )

custom_colors4 <- c("DEG1" = "#F8AE21","DEG2"= "yellow", "DEG3" = "#2644A7") 

plot3<- ggplot(urbanisation_countries, aes(x = reorder(geo, desc(deg_urb == "DEG1")), y = values, fill = deg_urb)) +
  scale_fill_manual(values = custom_colors4, labels = c("Cities", "Small Towns", "Rural Areas")) +
  geom_bar(stat = "identity",position = position_dodge(width = 0.6), width=0.4) +
  labs(title = "Overall life satisfaction by level of urbanisation,2022",x = NULL ,y = NULL, fill = "Urbanisation degree") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9),breaks = seq(0, 9, by = 1)) +
  scale_x_discrete(labels = country_names[unique(toupper(filtered_countries$geo))]) +
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
  scale_shape_manual(values = 16)


print(plot3)


#Figure 4
#Overall life satisfaction by demographic characteristics at EU level, 2018 and 2022

#NOT COMPLETE

household_countries <- subset(ilc_pw02, time == c("2018","2022") & hhcomp != "TOTAL" & quant_inc != "TOTAL" & deg_urb == "TOTAL" )


data_2018 <- subset(household_countries, time == "2018")
data_2022 <- subset(household_countries, time == "2022")

ggplot() +
  geom_bar(data = data_2022, aes(x = quant_inc, y = values, fill =time),
           stat = "identity", position = position_dodge(width = 0.4), width = 0.3) +
  geom_bar(data = data_2022, aes(x = hhcomp, y = values, fill = time),
           stat = "identity", position = position_dodge(width = 0.4), width = 0.3) +
  geom_col(width = 0.6, fill = "gold3") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8),breaks = seq(0, 8, by = 1)) +
  labs(title = "Overall life satisfaction by demographic characteristics at EU level, 2018 and 2022", x = NULL,y = NULL) +
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
  guides(fill = guide_legend(override.aes = list(size = 1))
         
  )











