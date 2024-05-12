# Consumer prices of recreational and sports 
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Consumer_prices_of_recreational_and_sporting_goods_and_services

library(restatapi)
library(ggplot2)
library(kableExtra)
library(reshape2)

#The all-items HICP and the HICPs for sporting goods and services, EU, 2013-2023
caption = "Note: the data refer to the official EU aggregate. Its country coverage changes in line with the addition of new EU 
Member States and integrates them using a chain-linked index formula.
Source: Eurostat (online data code: prc_hicp_aind)"

prc_hicp_aind_eu <- get_eurostat_data("prc_hicp_aind", filters = c(unit = "RCH_A_AVG", geo = "EU27_2020"), label = T)
prc_hicp_aind_eu$time <- as.numeric(as.character(prc_hicp_aind_eu$time))
prc_hicp_aind_eu_1 <- subset(prc_hicp_aind_eu, (coicop =="All-items HICP" | coicop == "Equipment for sport, camping and open-air recreation" | coicop == "Recreational and sporting services") &time >=2013)

plot1 <- ggplot(data = prc_hicp_aind_eu_1, aes(x = time, y = values, group = coicop, color = coicop)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 2, shape = 21, aes(fill = coicop)) +
  labs(title = "The all-items HICP and the HICPs for sporting goods and services, EU, 2013-2023",
       subtitle = "(%, annual rate of change)",
       x = "",
       y = "(% annual rate of change)",
       caption = caption) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.direction = "vertical",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title.position = "plot",
        plot.caption = element_text(size = 7, hjust = 0),
        plot.caption.position = "plot") +
  scale_y_continuous(limits = c(-1, 10), breaks = seq(-1, 10, 1)) + 
  scale_x_continuous(breaks = 2013:2023, labels = function(x) ifelse(x %% 2 == 1, x, "")) + 
  scale_color_manual(values = c("All-items HICP" = "#B655BD", "Equipment for sport, camping and open-air recreation" = "#B09120", "Recreational and sporting services" = "#2644A7")) +
  scale_fill_manual(values = c("All-items HICP" = "#B655BD", "Equipment for sport, camping and open-air recreation" = "#B09120", "Recreational and sporting services" = "#2644A7")) 

print(plot1)

#Harmonised indices of consumer prices for selected sporting goods and services
prc_hicp_aind_rch_eu <- get_eurostat_data("prc_hicp_aind", filters = c(geo = "EU27_2020", unit = "RCH_A_AVG"), label = T)
prc_hicp_aind_rch_eu <- subset(prc_hicp_aind_rch_eu, coicop =="All-items HICP" | coicop == "Recreational and sporting services - Participation" | coicop == "Recreational and sporting services - Attendance" | coicop == "Equipment for camping and open-air recreation" | coicop == "Equipment for sport" )
prc_hicp_aind_rch_eu$time <- as.numeric(as.character(prc_hicp_aind_rch_eu$time))
prc_hicp_aind_2018_23 <- prc_hicp_aind_rch_eu[time >= 2018 & time <= 2023, .("2018_2023" = mean(values)), by = coicop]
prc_hicp_aind_2022_23 <- prc_hicp_aind_rch_eu[time >= 2022 & time <= 2023, .("2022_2023" = mean(values)), by = coicop]
prc_hicp_aind_merged <- merge(prc_hicp_aind_2018_23, prc_hicp_aind_2022_23, on = "coicop")
prc_hicp_aind_merged <- reshape2::melt(prc_hicp_aind_merged, id.vars = "coicop")

prc_hicp_aind_merged$group <- ifelse(prc_hicp_aind_merged$coicop == "All-items HICP"&prc_hicp_aind_merged$variable == "2018_2023", "CP00 2018_2023", 
                                     ifelse(prc_hicp_aind_merged$coicop == "All-items HICP"&prc_hicp_aind_merged$variable == "2022_2023", "CP00 2022_2023",
                                            ifelse(prc_hicp_aind_merged$variable == "2018_2023", "Others 2018_2023", "Others 2022_2023")))

plot2 <- ggplot(prc_hicp_aind_merged, aes(x = coicop, y = value, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Harmonised indices of consumer prices for selected sporting goods and services, EU,\nannual average rates of change 2018-2023 and 2022-2023",
       subtitle = "(%)", 
       fill = "coicop",
       caption = caption) + 
  scale_fill_manual(values = c("CP00 2018_2023" = "#9cade8", 
                               "CP00 2022_2023" = "#2644A7",
                               "Others 2018_2023" = "#e2bbe5", 
                               "Others 2022_2023" = "#B655BD"),
                    breaks = c("Others 2018_2023","Others 2022_2023"),
                    labels = c("2018-2023","2022-2023")) +
  theme_minimal() +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 7, hjust = 0),
        plot.caption.position = "plot",
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        legend.justification = c(0,0)
  ) +
  scale_y_continuous(breaks = seq(0,8,1))

print(plot2)

#HICP for sporting goods and services – focus on countries
prc_hicp_aind <- get_eurostat_data("prc_hicp_aind", filters = list(unit = "RCH_A_AVG", coicop = c("All-items HICP", "Recreational and sporting services - Participation", "Recreational and sporting services - Attendance", "Equipment for camping and open-air recreation", "Equipment for sport")), date_filter=seq(2018,2023,1), label = T)
prc_hicp_aind$time <- as.numeric(as.character(prc_hicp_aind$time))
prc_hicp_aind$time_group <- ifelse(prc_hicp_aind$time >= 2018 & prc_hicp_aind$time <= 2023, "2018-2023", NA)
prc_hicp_aind$time_group[prc_hicp_aind$time >= 2022 & prc_hicp_aind$time <= 2023] <- paste(prc_hicp_aind$time_group[prc_hicp_aind$time >= 2022 & prc_hicp_aind$time <= 2023], "2022-2023", sep=",")
prc_hicp_aind$time_group <- ifelse(prc_hicp_aind$time_group == "2018-2023,2022-2023", "2018-2023 & 2022-2023", prc_hicp_aind$time_group)

cntr <- as.factor(c("European Union - 27 countries (from 2020)", "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
                    "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania",
                    "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria", "Poland","Portugal", "Romania",
                    "Slovenia", "Slovakia", "Finland", "Sweden","United Kingdom", "Iceland", "Liechtenstein",
                    "Norway", "Switze e o rland", "Montenegro", "North Macedonia", "Albania", "Serbia", "Turkey"))
prc_hicp_aind <- prc_hicp_aind[(prc_hicp_aind$geo %in% cntr), ]
prc_hicp_aind$geo <- as.character(prc_hicp_aind$geo)
prc_hicp_aind$geo <- factor(prc_hicp_aind$geo, levels = cntr)
cntr <- as.character(cntr)

pivot_table <- dcast(prc_hicp_aind, geo ~ coicop + time_group, value.var = "values", fun.aggregate = sum)
colnames(pivot_table) <- c("Countries ", "2018-2023", "2022-2023","2018-2023","2022-2023","2018-2023","2022-2023","2018-2023","2022-2023","2018-2023","2022-2023")

kable_styling <- kable(pivot_table, format = "html", escape = F, digits = 1) %>%
  kable_styling(c("striped", "bordered")) %>%
  add_header_above(c("-" = 1, "All-items HICP" = 2, "Equipment for sport" = 2, "Equipment for camping and open-air recreation" = 2, "Recreational and sporting services - Attendance" = 2, "Recreational and sporting services - Participation" = 2), background='#f0dcf1') %>%
  group_rows(" ", 29, 30) %>%
  group_rows(" ", 30, 33) %>%
  group_rows(" ", 33, 35) %>%
  row_spec(row = 0, background = "#f0dcf1") %>%
  row_spec(row = 1, background = "#e1bae4", bold = TRUE) %>%
  column_spec(1, bold=TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, font_size = 12, fixed_thead = TRUE)

kable_styling
