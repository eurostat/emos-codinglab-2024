library(restatapi)
library(ggplot2)
library(reshape2)

ilc_pw01 <- get_eurostat_data("ilc_pw01", filters = c(unit = "RTG", isced11 = "TOTAL", indic_wb = "LIFESAT"))
eu_iso_a2 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "EL", 
               "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
               "RO", "SE", "SI", "SK", "CH", "NO", "IS", "XK", "ME", "MK", "TR", "RS", "AL","EU27_2020")
ilc_pw01 <- ilc_pw01[(ilc_pw01$geo %in% eu_iso_a2), ]

#Satisfaction by country
countries <- subset(ilc_pw01, sex == "T" & age == "Y_GE16" & time == "2022")
plot0 <- ggplot(countries, aes(x = reorder(geo, values, decreasing = TRUE), y = values)) +
  geom_col(width = 0.6, fill = "#2644A7") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1.01 * max(countries$values))) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.ticks.x = element_line()) +
  labs(x = NULL, y = "Average rating of satisfaction by Country")

print(plot0)

#By Age and sex
age_sex <- subset(ilc_pw01, (time == "2022"| time == "2013") & geo == "EU27_2020")
options(repr.plot.width=5, repr.plot.height=5)
plot1 <- ggplot(age_sex, aes(x=age , y=values, fill= sex,)) + 
  geom_bar(data=subset(age_sex, sex == "F" & time=="2022" ),aes(x = age, y=values), fill="#F8AE21", stat = "identity") + 
  geom_bar(data=subset(age_sex,sex == "M" &time=="2022"),aes(x = age, y=-values), fill="#2644A7", stat = "identity") +
  geom_bar(data=subset(age_sex, sex == "F" & time=="2013" ),aes(x = age, y=values),fill="transparent", colour="#FFE879", size=1, stat = "identity") + 
  geom_bar(data=subset(age_sex,sex == "M" &time=="2013"),aes(x = age, y=-values), fill="transparent", colour="#c5cde6", size=1, stat = "identity") +
  #scale_x_discrete(limits= c("<5","5-9", "10-14", "15-19","20-24","25-29","30-34", "35-39", "40-44","45-49","50-54","55-59","60-64","65-69","70-74", "75-79","80-84", "85+")) +
  coord_flip()+
  labs(title = "Average rating of satisfaction by age",
       caption = "Solid colour: 2023 Bordered: 2013") +
  theme_bw()

print(plot1)

#By Year
geo_time <- subset(ilc_pw01, sex == "T" & age == "Y_GE16" )

plot2 <- ggplot(data = geo_time, aes(x = time, y = values, group = geo, color = geo)) +
  geom_line() +  
  theme_bw()
print(plot2)