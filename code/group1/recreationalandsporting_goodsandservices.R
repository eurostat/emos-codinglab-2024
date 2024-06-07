# Consumer prices of recreational and sports 
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Consumer_prices_of_recreational_and_sporting_goods_and_services

library(restatapi)
library(ggplot2)
library(kableExtra)
library(reshape2)
library(openxlsx)
library(xlsx)

#The all-items HICP and the HICPs for sporting goods and services, EU, 2013-2023
caption = "Note: the data refer to the official EU aggregate. Its country coverage changes in line with the addition of new EU 
Member States and integrates them using a chain-linked index formula.
Source: Eurostat (online data code: prc_hicp_aind)"

prc_hicp_aind_eu <- get_eurostat_data("prc_hicp_aind", filters = c(unit = "RCH_A_AVG", geo = "EU27_2020"), label = T)
prc_hicp_aind_eu$time <- as.numeric(as.character(prc_hicp_aind_eu$time))
prc_hicp_aind_eu_1 <- subset(prc_hicp_aind_eu, (coicop =="All-items HICP" | coicop == "Equipment for sport, camping and open-air recreation" | coicop == "Recreational and sporting services") &time >=2013)

#png("plot1.png")

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
plot1

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


png("plot2.png")
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
dev.off()

print(plot2)

#HICP for sporting goods and services – focus on countries

#2022-2023 -> value of 2023, 2018-2023 -> average from 2019 to 2023
prc_hicp_aind <- get_eurostat_data("prc_hicp_aind", filters = list(unit = "RCH_A_AVG", coicop = c("All-items HICP", "Recreational and sporting services - Participation", "Recreational and sporting services - Attendance", "Equipment for camping and open-air recreation", "Equipment for sport")), date_filter=seq(2018,2023,1), label = T)
prc_hicp_aind$time <- as.numeric(as.character(prc_hicp_aind$time))
pivot_table<- dcast(prc_hicp_aind, geo + coicop ~ time, value.var = "values", fun.aggregate = mean)
pivot_table[, 4:8] <- sapply(pivot_table[, 4:8], as.numeric)
pivot_table$`2018_2023` <- rowMeans(pivot_table[, 4:8], na.rm = TRUE)
pivot_table <- pivot_table[, c("geo", "coicop", "2018_2023", "2023")]
colnames(pivot_table) <- c("geo", "coicop", "2018-2023", "2022-2023")

reshaped_2018_2023 <- dcast(pivot_table, geo ~ coicop, value.var = "2018-2023")
names(reshaped_2018_2023)[-1] <- paste(names(reshaped_2018_2023)[-1], "2018-2023", sep = "_")
reshaped_2022_2023 <- dcast(pivot_table, geo ~ coicop, value.var = "2022-2023")
names(reshaped_2022_2023)[-1] <- paste(names(reshaped_2022_2023)[-1], "2022-2023", sep = "_")

final_df <- merge(reshaped_2018_2023, reshaped_2022_2023, by = "geo")
final_df <- final_df[,c("geo","All-items HICP_2018-2023", "All-items HICP_2022-2023", "Equipment for sport_2018-2023", "Equipment for sport_2022-2023", "Equipment for camping and open-air recreation_2018-2023", "Equipment for camping and open-air recreation_2022-2023", "Recreational and sporting services - Attendance_2018-2023", "Recreational and sporting services - Attendance_2022-2023", "Recreational and sporting services - Participation_2018-2023", "Recreational and sporting services - Participation_2022-2023")]
cntr <- as.factor(c("European Union - 27 countries (from 2020)", "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
                    "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania",
                    "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria", "Poland","Portugal", "Romania",
                    "Slovenia", "Slovakia", "Finland", "Sweden","United Kingdom", "Iceland", "Liechtenstein",
                    "Norway", "Switze e o rland", "Montenegro", "North Macedonia", "Albania", "Serbia", "Turkey", "Kosovo"))

final_df <- final_df[(final_df$geo %in% cntr), ]
final_df$geo <- as.character(final_df$geo)
final_df$geo <- factor(final_df$geo, levels = cntr)
final_df <- final_df[order(final_df$geo), ]
cntr <- as.character(cntr)
colnames(final_df) <- c("Countries ", "2018-2023", "2022-2023","2018-2023","2022-2023","2018-2023","2022-2023","2018-2023","2022-2023","2018-2023","2022-2023")
final_df

kable_styling <- kable(final_df, format = "html", escape = F, digits = 1, stripe_color = "blue!10",latex_options = "basic", row.names = FALSE) %>%
  
  add_header_above(c("-" = 1, "All-items HICP" = 2, "Equipment for sport" = 2, "Equipment for camping and open-air recreation" = 2, "Recreational and sporting services - Attendance" = 2, "Recreational and sporting services - Participation" = 2), background='#f0dcf1') %>%
  group_rows(" ", 1, 1, label_row_css = "border-bottom: 1px solid black; margin: 0; padding: 0;") %>%
  group_rows(" ", 2, 28, label_row_css = "border-bottom: 1px solid black;margin: 0; padding: 0;") %>%
  group_rows(" ", 29, 30, label_row_css = "border-bottom: 1px solid black;margin: 0; padding: 0;") %>%
  group_rows(" ", 30, 33, label_row_css = "border-bottom: 1px solid black;margin: 0; padding: 0;") %>%
  group_rows(" ", 33, 35, label_row_css = "border-bottom: 1px solid black;margin: 0; padding: 0;") %>%
  row_spec(row = 0, background = "#f0dcf1", extra_css = "white-space: nowrap;") %>%
  row_spec(row = 1, background = "#e1bae4", bold = TRUE) %>%
  column_spec(1, bold=TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = F, font_size = 12, fixed_thead = TRUE)

kable_styling

# EXCEL
wb <- createWorkbook()
sheet_1 <- createSheet(wb, sheetName = "HICP by year")
addPicture("plot1.png", sheet_1, scale = 1, startRow = 1, startColumn = 1)
sheet_2 <- createSheet(wb, sheetName = "Harmonised indices")
addPicture("plot2.png", sheet_2, scale = 1, startRow = 1, startColumn = 1)
sheet_3 <- createSheet(wb, sheetName = "Countries")
addDataFrame(pivot_table, sheet_3, row.names = FALSE, startRow = 1, startColumn = 1)
saveWorkbook(wb, "Consumer prices of recreational and sporting goods and services.xlsx")