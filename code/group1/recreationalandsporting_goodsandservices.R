# Consumer prices of recreational and sports 
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Consumer_prices_of_recreational_and_sporting_goods_and_services

library(restatapi)
library(ggplot2)
library(kableExtra)
library(reshape2)
library(openxlsx)
library(xlsx)
library(plotly)



#The all-items HICP and the HICPs for sporting goods and services, EU, 2013-2023  ---------------------------------------------------------------------------

#Choose dataset and filters here
dataset <- "prc_hicp_aind"

categories <- c("All-items HICP",
                "Equipment for sport, camping and open-air recreation",
                "Recreational and sporting services")

start_year <- 2013
end_year <- 2023

geo <- "EU27_2020"
unit <- "RCH_A_AVG"

#Plot title, subtitle, caption here
title <- "The all-items HICP and the HICPs for sporting goods and services, EU, 2013-2023"
subtitle <- "(% annual rate of change)"
caption = "Note: the data refer to the official EU aggregate. Its country coverage changes in line with the addition of new EU 
Member States and integrates them using a chain-linked index formula.
Source: Eurostat (online data code: prc_hicp_aind)"

#Choose colors for plot here. Eurostat pallete B
euro_palette <- c("#B655BD", "#2644A7", "#B09120", "#672DC4", "#388AE2", "#AF155C")

palette <- c("All-items HICP" = euro_palette[1],
             "Equipment for sport, camping and open-air recreation" = euro_palette[2],
             "Recreational and sporting services" = euro_palette[3])



#CODE ---------------------

prc_hicp_aind_eu <- get_eurostat_data(dataset,  filters = list(geo = geo, coicop = categories, unit = unit), date_filter=seq(start_year,end_year,1), label = T)
prc_hicp_aind_eu$time <- as.numeric(as.character(prc_hicp_aind_eu$time))

png("plot1.png")

plot1 <- ggplot(data = prc_hicp_aind_eu, aes(x = time, y = values, group = coicop, color = coicop)) +
  geom_point(size = 2, shape = 21, aes(fill = coicop)) +
  geom_line(linewidth = 1) + 
  
  labs(title = title,
       subtitle = subtitle,
       x = "",
       y = subtitle,
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
  scale_y_continuous(limits = c(floor(min(prc_hicp_aind_eu$values)), ceiling(max(prc_hicp_aind_eu$values))), breaks = seq(floor(min(prc_hicp_aind_eu$values)), ceiling(max(prc_hicp_aind_eu$values)), 1)) + 
  scale_x_continuous(breaks = start_year:end_year, labels = function(x) ifelse(x %% 2 == 1, x, "")) + 
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) 

print(plot1)
dev.off()
plot1

library(highcharter)

interactive_plot <- hchart(prc_hicp_aind_eu, "line", hcaes(x = time, y = values, group = coicop)) %>%
  hc_title(text = title) %>%
  hc_subtitle(text = subtitle) %>%
  hc_xAxis(title = list(text = ""), categories = unique(prc_hicp_aind_eu$time), labels = list(format = "{value}")) %>%
  hc_yAxis(title = list(text = subtitle), min = floor(min(prc_hicp_aind_eu$values)), max = ceiling(max(prc_hicp_aind_eu$values))) %>%
  hc_tooltip(shared = TRUE, crosshairs = TRUE, pointFormat = '<b>{series.name}</b>: {point.y}<br/>') %>%
  hc_legend(align = "center", verticalAlign = "bottom", layout = "vertical") %>%
  hc_colors(euro_palette) %>%
  hc_plotOptions(line = list(marker = list(symbol = "circle", enabled = TRUE)))

interactive_plot






#Harmonised indices of consumer prices for selected sporting goods and services  ---------------------------------------------------------------------------

#Choose dataset and filters here
dataset <- "prc_hicp_aind"

group <- "All-items HICP"
categories <- c(group, 
                "Recreational and sporting services - Participation", 
                "Recreational and sporting services - Attendance", 
                "Equipment for camping and open-air recreation", "Equipment for sport","")

start_year_1 <- 2018
start_year_2 <- 2022
end_year <- 2023

geo <- "EU27_2020"
unit <- "RCH_A_AVG"

#Define plot title, subtitle and colors here
title <- "Harmonised indices of consumer prices for selected sporting goods and services, EU,\nannual average rates of change 2018-2023 and 2022-2023"
subtitle <- "(%)"

#color of year group 1 in each category
color_y1 <- "#B656BD"
  #color of year group 2 in each category
color_y2 <- "#f0dcf1"
  #color of year group 1 in all
color_y1_all <- "#2644A7"
  #color of year group 2 in all
color_y2_all <- "#9cade8"
  

#CODE ---------------

prc_hicp_aind_rch_eu <- get_eurostat_data(dataset, filters = list(geo = geo, coicop = categories, unit = unit), date_filter=seq(start_year_1,end_year,1), label = T)
prc_hicp_aind_rch_eu$time <- as.numeric(as.character(prc_hicp_aind_rch_eu$time))

year_group_1 <- paste(start_year_1, end_year, sep = "-")
year_group_2 <- paste(start_year_2, end_year, sep = "-")

prc_hicp_aind_g1 <- prc_hicp_aind_rch_eu[time >= start_year_1+1 & time <= end_year, .(mean(values)), by = coicop]
names(prc_hicp_aind_g1)[2] <- year_group_1
prc_hicp_aind_g2 <- prc_hicp_aind_rch_eu[time >= start_year_2+1 & time <= end_year, .(mean(values)), by = coicop]
names(prc_hicp_aind_g2)[2] <- year_group_2

prc_hicp_aind_merged <- merge(prc_hicp_aind_g1, prc_hicp_aind_g2, on = "coicop")
prc_hicp_aind_merged <- reshape2::melt(prc_hicp_aind_merged, id.vars = "coicop")

group_year_group_1 <- paste(group, year_group_1)
group_year_group_2 <- paste(group, year_group_2)
categories_year_group_1 <- paste("Others", year_group_1)
categories_year_group_2 <- paste("Others", year_group_2)

prc_hicp_aind_merged$group <- ifelse(prc_hicp_aind_merged$coicop == group&prc_hicp_aind_merged$variable == year_group_1, group_year_group_1, 
                                     ifelse(prc_hicp_aind_merged$coicop == group&prc_hicp_aind_merged$variable == year_group_2, group_year_group_2,
                                            ifelse(prc_hicp_aind_merged$variable == year_group_1, categories_year_group_1, categories_year_group_2)))


unique_coicop <- unique(as.character(prc_hicp_aind_merged$coicop))
unique_coicop <- unique_coicop[unique_coicop != group]
ordered_levels <- c(unique_coicop, group)
prc_hicp_aind_merged$coicop <- factor(prc_hicp_aind_merged$coicop, levels = ordered_levels)

colors <- c(group_year_group_1 = color_y1_all, 
            group_year_group_2 = color_y2_all, 
            categories_year_group_1 = color_y1, 
            categories_year_group_2 = color_y2)
names(colors) <- c(group_year_group_1, group_year_group_2, categories_year_group_1, categories_year_group_2)

png("plot2.png")

plot2 <- ggplot(prc_hicp_aind_merged, aes(x = coicop, y = value, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = title,
       subtitle = subtitle, 
       fill = "coicop",
       caption = caption) + 
  scale_fill_manual(values = colors,
                    breaks = c(categories_year_group_1,categories_year_group_2),
                    labels = c(year_group_1,year_group_2)) +
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
  scale_y_continuous(breaks = seq(0,ceiling(max(prc_hicp_aind_merged$value)),1))

print(plot2)
dev.off()

plot2











#HICP for sporting goods and services – focus on countries  ---------------------------------------------------------------------------


#Choose dataset and filters here
dataset <- "prc_hicp_aind"

categories <- c("All-items HICP", 
                "Equipment for sport",
                "Equipment for camping and open-air recreation", 
                "Recreational and sporting services - Participation", 
                "Recreational and sporting services - Attendance" 
)

start_year_1 <-2018
start_year_2 <- 2022
end_year <- 2023

unit <- "RCH_A_AVG"

#Define header colors here
color_a <- "#f0dcf1"
  color_b <- "#e1bae4"
    
  
  
  #CODE ------------------------
  prc_hicp_aind <- get_eurostat_data(dataset, filters = list(unit = unit, coicop = categories), date_filter=seq(start_year_1,end_year,1), label = T)
  
  prc_hicp_aind$time <- as.numeric(as.character(prc_hicp_aind$time))
  pivot_table<- dcast(prc_hicp_aind, geo + coicop ~ time, value.var = "values", fun.aggregate = mean)
  
  start_1_col <- which(names(pivot_table) == start_year_1) + 1
  start_2_col <- which(names(pivot_table) == start_year_2) + 1
  end_col <- which(names(pivot_table) == end_year)
  
  pivot_table[, start_1_col:end_col] <- sapply(pivot_table[, start_1_col:end_col], as.numeric)
  
  year_group_1 <- paste(start_year_1, end_year, sep = "-")
  year_group_2 <- paste(start_year_2, end_year, sep = "-")
  
  #Computing mean of year groups
  if (ncol(pivot_table[, start_1_col:end_col, drop = FALSE]) == 1) {
    pivot_table[[year_group_1]] <- pivot_table[, end_col]
  } else {
    pivot_table[[year_group_1]] <- round(rowMeans(pivot_table[, start_1_col:end_col], na.rm = TRUE),1)
  }
  
  if (ncol(pivot_table[, start_2_col:end_col, drop = FALSE]) == 1) {
    pivot_table[[year_group_2]] <- pivot_table[, end_col]
  } else {
    pivot_table[[year_group_2]] <- round(rowMeans(pivot_table[, start_2_col:end_col], na.rm = TRUE),1)
  }
  
  #converting to format
  pivot_table <- pivot_table[,c("geo","coicop", year_group_1, year_group_2)]
  
  melted_df <- melt(pivot_table, id.vars = c("geo", "coicop"), 
                    variable.name = "year_group", 
                    value.name = "values")
  
  melted_df <- dcast(melted_df, geo ~ coicop + year_group, value.var = "values", fun.aggregate = mean)
  column_names <- names(melted_df)
  
  
  #Filtering countries and create groups of countries
  eu_ctry_names<-do.call(rbind,lapply(get("cc",envir=.restatapi_env)$EU28,search_eurostat_dsd,dsd=get_eurostat_dsd(dataset),exact_match=TRUE))$name
  eu_ctry_names <- eu_ctry_names[-length(eu_ctry_names)]
  efta_countries <- c("Iceland", "Liechtenstein", "Norway", "Switzerland")
  eu_candidate_countries <- c("Albania", "Montenegro", "North Macedonia", "Serbia", "Turkey")
  
  cntr <- as.factor(c("European Union - 27 countries (from 2020)", eu_ctry_names, "United Kingdom", efta_countries, eu_candidate_countries))
  cntr <- factor(cntr, levels = cntr)
  cntr_groups <- data.frame(geo = cntr,
                            groups = c("EU", rep("EU27", length(eu_ctry_names)), "UK", rep("EFTA", length(efta_countries)), rep("EU Candidate", length(eu_candidate_countries))))
  final_df <- melted_df[(melted_df$geo %in% cntr), ]
  final_df <- merge(final_df, cntr_groups, by = "geo", all.x = TRUE)
  final_df <- final_df[order(factor(final_df$geo, levels = levels(cntr))), ]
  groups_info <- final_df$groups
  final_df<- final_df[-length(final_df)]
  
  
  #Defining header
  num_categories <- length(categories)
  colnames(final_df) <- c("Countries", rep(c(year_group_1, year_group_2), times = num_categories))
  header <- c("\u200B" = 1)
  for (name in column_names) {
    base_category <- sub("_.*", "", name)
    if (base_category %in% categories) {
      header[base_category] <- 2
    }
  }
  
  
  #Creating table
  kable_styling <- kable(final_df, format = "html", escape = F, digits = 1, 
                         row.names = FALSE) %>%
    add_header_above(header, background = color_a) %>%
    row_spec(row = 0, background = color_a, extra_css = "white-space: nowrap; border: none;") %>%
    row_spec(row = 1, background = color_b, bold = TRUE, extra_css = "border: none;") %>%
    column_spec(1, bold=TRUE)
  
  groups <- unique(groups_info)
  group_indices <- split(seq_len(nrow(final_df)), groups_info)
  
  for (group in groups) {
    kable_styling <- kable_styling %>%
      group_rows(" ", min(group_indices[[group]]), max(group_indices[[group]]), 
                 label_row_css = "border-bottom: 1px solid black; margin: 0; padding: 0;")
  }
  
  kable_styling <- kable_styling %>%
    kable_styling(bootstrap_options = c("striped", "hover"),
                  full_width = F, font_size = 12, fixed_thead = TRUE)
  
  kable_styling
  
  
  
  
  
  
  
  # EXCEL ---------------------------------------------------------------------------
  wb <- createWorkbook()
  sheet_1 <- createSheet(wb, sheetName = "HICP by year")
  addPicture("plot1.png", sheet_1, scale = 1, startRow = 1, startColumn = 1)
  sheet_2 <- createSheet(wb, sheetName = "Harmonised indices")
  addPicture("plot2.png", sheet_2, scale = 1, startRow = 1, startColumn = 1)
  sheet_3 <- createSheet(wb, sheetName = "Countries")
  addDataFrame(final_df, sheet_3, row.names = FALSE, startRow = 1, startColumn = 1)
  
  saveWorkbook(wb, "Consumer prices of recreational and sporting goods and services.xlsx")
  
  
  #Export the table as html and png
  library(webshot)
  library(htmltools)
  webshot::install_phantomjs(force = TRUE)
  
  save_kable(kable_styling, file = "table1.html")
  webshot("table1.html", "table1.png", selector = "table")
  