### 

library(restatapi)
library(ggplot2)
library(reshape2)
#library(dplyr)
library(giscoR)
library(data.table)
library(chron)
library(kableExtra)
library(tmap)
library(highcharter)
library(plotly)


#get the dataset
prc_hicp_aind_eu <- get_eurostat_data("prc_hicp_aind", filters = c(unit = "RCH_A_AVG", geo = "EU27_2020"))
prc_hicp_aind_eu$time <- as.numeric(as.character(prc_hicp_aind_eu$time))
prc_hicp_aind_eu_1 <- subset(prc_hicp_aind_eu, (coicop =="CP00" | coicop == "CP0932" | coicop == "CP0941")&time >=2013)

plot1 <- ggplot(data = prc_hicp_aind_eu_1, aes(x = time, y = values, group = coicop, color = coicop)) +
  geom_line() +  
  theme_bw()
print(plot1)


#Figure 2 - countries by year
#2013-2023
prc_hicp_aind_00 <- get_eurostat_data("prc_hicp_aind", filters = c(unit = "RCH_A_AVG", COICOP = "CP00"))

eu_iso_a2 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL" ,"ES", "FI","FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", 
               "RO", "SE", "SI", "SK" ,"EU27_2020")

prc_hicp_aind_00 <- prc_hicp_aind_00[(prc_hicp_aind_00$geo %in% eu_iso_a2), ]

prc_hicp_aind_00$time <- as.numeric(as.character(prc_hicp_aind_00$time))

prc_hicp_aind_00_ <- subset(prc_hicp_aind_00, time >=2013)

plot2 <- ggplot(data = prc_hicp_aind_00_, aes(x = time, y = values, group = geo, color = geo)) +
  geom_line() +  
  theme_bw()
print(plot2)

#Figure 3 - countries in 2023
prc_hicp_aind_2023 <- get_eurostat_data("prc_hicp_aind", filters = c(COICOP = "CP00", time = "2023"))
prc_hicp_aind_2023 <- prc_hicp_aind_2023[(prc_hicp_aind_2023$geo %in% eu_iso_a2), ]
prc_hicp_aind_rch <- subset(prc_hicp_aind_2023, unit == "RCH_A_AVG")
countries_rch <- subset(prc_hicp_aind_rch, geo != "EU27_2020")
EU_value_rch=prc_hicp_aind_rch$values[prc_hicp_aind_rch$geo== "EU27_2020"]

countries_rch$text <- paste("Country:", countries_rch$geo, "<br>", "Value:", countries_rch$values)

plot3 <- ggplot(countries_rch, aes(x = reorder(geo, values[geo != "EU27_2020"], decreasing = TRUE), y = values, text = paste("Country:", geo, "<br>", "Value:", values))) +
  geom_col(width = 0.6, fill = "#2644A7") +
  geom_hline(yintercept = EU_value_rch, color = "#F8AE21",linetype = "solid",linewidth =1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20),breaks = seq(0, 20, by = 1)) +
  #scale_x_discrete(labels = country_names[unique(countries$geo)]) +
  scale_x_discrete(labels = country_names[unique(toupper(filtered_countries$geo))]) +
  coord_cartesian(ylim = c(0, 20)) +
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
  labs(x = NULL, y = NULL, title="The all-item HICP for sporting goods and services, EU, 2023 (%, annual rate of change)")

print(plot3)

plot3_int <- ggplotly(plot3)
print(plot3_int)

#Figure 4 - countries in 2023
prc_hicp_aind_inx <- subset(prc_hicp_aind_2023, unit == "INX_A_AVG")
countries_inx <- subset(prc_hicp_aind_inx, geo != "EU27_2020")
EU_value_inx=prc_hicp_aind_inx$values[prc_hicp_aind_inx$geo== "EU27_2020"]

plot4 <- ggplot(countries_inx, aes(x = reorder(geo, values[geo != "EU27_2020"], decreasing = TRUE), y = values)) +
  geom_col(width = 0.6, fill = "#2644A7") +
  geom_hline(yintercept = EU_value_inx, color = "#F8AE21",linetype = "solid",linewidth =1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 180),breaks = seq(0, 180, by = 20)) +
  #scale_x_discrete(labels = country_names[unique(countries$geo)]) +
  scale_x_discrete(labels = country_names[unique(toupper(filtered_countries$geo))]) +
  coord_cartesian(ylim = c(0, 200)) +
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
  labs(x = NULL, y = NULL, title="The all-item HICP for sporting goods and services, EU, 2023 (annual average index)")
print(plot4)

#Figure 5
prc_hicp_aind_rch_eu <- get_eurostat_data("prc_hicp_aind", filters = c(geo = "EU27_2020", unit = "RCH_A_AVG"))
prc_hicp_aind_rch_eu <- subset(prc_hicp_aind_rch_eu, coicop =="CP00" | coicop == "CP09411" | coicop == "CP09412" | coicop == "CP09322" | coicop == "CP09321" )
prc_hicp_aind_rch_eu$time <- as.numeric(as.character(prc_hicp_aind_rch_eu$time))
prc_hicp_aind_2018_23 <- prc_hicp_aind_rch_eu[time >= 2018 & time <= 2023, .("2018_2023" = mean(values)), by = coicop]
prc_hicp_aind_2022_23 <- prc_hicp_aind_rch_eu[time >= 2022 & time <= 2023, .("2022_2023" = mean(values)), by = coicop]

merged_table <- merge(prc_hicp_aind_2018_23, prc_hicp_aind_2022_23, on = "coicop")
merged_long <- reshape2::melt(merged_table, id.vars = "coicop")
colors <- c("2018_2023" = "#F8AE21", "2022_2023" = paste0("#F8AE21", "50"))

ggplot(merged_long, aes(x = coicop, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Values for each coicop",
       x = "coicop",
       y = "Average Value",
       fill = "Dataset") + 
  scale_fill_manual(values = colors) +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))