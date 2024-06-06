# Load the required packages

library(tidyverse)
library(grid)
library(png)
library(restatapi)
library(gridExtra)

####################
###### Graph1 ######
####################

# Get the id and read the data through dsd
id <- "prc_hicp_aind"
view(get_eurostat_dsd(id))

labels <- get_eurostat_dsd(id)%>%
  rename(coicop=code)

# Specify the years of interest
time_EU <- seq(from = 2013, to = 2023, by = 1)

# Specify the 7 categories of interest 
cult_var_names <- c("CP0952", "CP0951", "CP0913", "CP00", "CP0914", "CP0911", "CP0942")
data_api <- get_eurostat_data(id, 
                              filters = list(geo = "EU",coicop = cult_var_names, 
                                             unit = "RCH_A_AVG"), 
                              date_filter = time_EU) 
# Create the dataset with left join by category (coicop)
data_api <- left_join(data_api, labels, by="coicop")
view(data_api)

# Now define all the variables seperately 
time_graph1 <- unique(data_api$time)

all_hicp <- data_api %>%
  filter(coicop == "CP00") %>%
  select(values)

equip <- data_api %>%
  filter(coicop == "CP0913") %>%
  select(values)

info_equip <- data_api %>%
  filter(coicop == "CP0911") %>%
  select(values)

rec_media <- data_api %>%
  filter(coicop == "CP0914") %>%
  select(values)

cult_serv <- data_api %>%
  filter(coicop == "CP0942") %>%
  select(values)

books <- data_api %>%
  filter(coicop == "CP0951") %>%
  select(values)

newspapers <- data_api %>%
  filter(coicop == "CP0952") %>%
  select(values)

# Bind cols and create a new column for data frame names
combined_cols <- bind_cols(as_tibble(time_graph1), all_hicp, equip, info_equip,
                           rec_media, cult_serv, books, newspapers)

colnames(combined_cols) <- c("time","all_hicp","equip","info_equip",
                             "rec_media","cult_serv","books","newspapers")

#Transform to long format
combined_data_long <- combined_cols %>%
  pivot_longer(cols = -time, names_to = "variable", values_to = "value")

# Convert value column to numeric
combined_data_long$value <- as.numeric(combined_data_long$value)
combined_data_long$time <- as.numeric(combined_data_long$time)

# Create the factors for the legend
combined_data_long$variable <- factor(combined_data_long$variable, 
                                      levels = c("newspapers", "books", "info_equip", "all_hicp",
                                                 "cult_serv", "rec_media", "equip"))
# Create the vector for legend names 
labels_original <- c("Newspapers and periodicals", "Books", "Information Processing Equipment (3)", 
                     "All-items HICP", "Cultural Services (1)", "Recording Media (2)", "Equipment for the reception, recording and reproduction of sound and picture (4)")

# Define codes for each variable with palette 2 from Eurostat apart from all_hicp which didn't have a colour from the palettes
color_palette <- c(
  all_hicp = "#d7a2db",        
  equip = "#af155c",          
  info_equip = "#388ae2",     
  rec_media = "#672dc4",       
  cult_serv = "#2644a7",       
  books = "#b09120",           
  newspapers = "#b656bd"       
)

# Define the footnote
cap = paste("Note: average indices reflect changes in EU membership (when a Member State joins the EU, its HICPs are chained with the aggregate index at the time of the accession).
(1) Includes cinemas, theatres, concerts; museums, libraries, zoological gardens; television and radio fees, hire of equipment and accessories for culture; and other cultural services.
(2) Includes records, CDs, DVDs, tapes, cassettes, etc.
(3) Includes personal computers, visual display units, printers and miscellaneous accessories accompanying them; also computer software packages such as operating systems, applications, languages, etc.
(4) Includes TV sets, CD players, stereo systems, radios, etc.
Source: Eurostat (online data code: prc_hicp_aind)")

# Create the plot
plot1 <- ggplot(combined_data_long, aes(x = time, y = value,
                                        color = variable, shape = variable,
                                        linewidth = variable, size = variable)) +
  geom_point() +
  geom_line() +
  guides(color=guide_legend(ncol=2), shape=guide_legend(ncol=2), linewidth=F, size=F) +
  scale_shape_manual(values=c(15,17,4,21,18,4,19), labels=labels_original) +
  scale_linewidth_manual(values = c(1.1,1.1,1.1,1.8,1.1,1.1,1.1)) +
  labs(title="Harmonised indices of consumer prices for selected cultural goods and services, EU, 2013-2023",
       subtitle="(%, annual rate of change)", x = "", y = "", color = "Variable", shape = "Variable",
       cols='markers') +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  scale_y_continuous(breaks = seq(-10, 12, by = 2), limits = c(-10,10)) +
  scale_color_manual(values = color_palette, labels=labels_original) +
  scale_size_manual(values=c(1.8,1.8,1.8,2,1.8,1.8,1.8)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),  # Set background to white
        panel.grid.major.y = element_line(color = "gray"),  # Set major horizontal grid lines to gray
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 8, hjust = 0, vjust = 5),
        legend.position = "bottom", legend.title = element_blank())

logo_file <- "logo_RGB-POS.png"  # Eurostat logo
logo <- readPNG(logo_file)
logo_grob <- rasterGrob(logo, interpolate=T)

# Create a text grob for the footnote
footnote_grob <- textGrob(cap, x = unit(0, "npc"), y = unit(1, "npc"), 
                          just = c("left", "top"), gp = gpar(fontsize = 8))

# Combine the footnote and logo in a horizontal layout
footnote_with_logo <- arrangeGrob(footnote_grob, logo_grob, ncol = 2, widths = c(4, 1))

# Combine the plot and the footnote with logo in a vertical layout
combined_grob <- arrangeGrob(plot1, footnote_with_logo, ncol = 1, heights = c(4, 1))

# Display the combined plot with footnote and logo

grid.draw(combined_grob)

####################
###### Graph2 ######
####################

# Define a new dataset to not overwrite the previous
data_api2 <- data_api %>%
  mutate(time = as.numeric(as.character(time)))

# Filter 2018-2023 and 2022-2023 since that's what's required
values_2022_2023 <- data_api2 %>%
  filter(time > 2022 & time <= 2023) %>%            # Filter for the years 2022 to 2023
  group_by(name) %>%                             # Group by the category column
  summarize(value = mean(values, na.rm = TRUE)) %>% # Calculate the mean of values for each category
  select(name, value) %>%
  mutate(date = rep("2022-2023", 7)) %>%
  arrange(name)

values_2018_2023 <- data_api2 %>%
  filter(time > 2018 & time <= 2023) %>%            # Filter for the years 2018 to 2023
  group_by(name) %>%                             # Group by the category column
  summarize(value = mean(values, na.rm = TRUE)) %>% # Calculate the mean of values for each category
  select(name, value) %>%
  mutate(date = rep("2018-2023", 7)) %>%
  arrange(name)

# Join
data_long_2 <- bind_rows(values_2018_2023, values_2022_2023)
data_long2 <- data_long_2 %>%
  mutate(Color_Group = ifelse(name == "All-items HICP", paste(name, date), paste("Other", date))) %>%
  mutate_if(is.character,as.factor)

# Redefine the names of the categories as needed
var_categories <- unique(data_api$name)
var_categories[2] <- "Equipment for the reception, recording  
and reproduction of sound and picture (4)"
var_categories[3] <- "Information processing equipment (3)"
var_categories[4] <- "Recording Media (2)"
var_categories[5] <- "Cultural Services (1)"

var_categories <- sort(var_categories)
var_cat <- bind_rows(as_tibble(var_categories), as_tibble(var_categories))
colnames(var_cat) <- "category"
data_long2 <- bind_cols(data_long2, var_cat) %>% select(-name)

data_long2 <- data_long2 %>% 
  arrange(date) %>% 
  arrange(value) %>% 
  arrange(desc(Color_Group)) %>%
  mutate_if(is.character, as.factor)

data_long2$category <- factor(data_long2$category, levels=unique(data_long2$category))

# Define second footnote
cap2 = "Note: average indices reflect changes in EU membership (when a Member State joins the EU, its HICPs are chained with the aggregate index at the time of the accession).
 (1) Includes cinemas, theatres, concerts; museums, libraries, zoological gardens; television and radio fees, hire of equipment and accessories for culture; and other cultural services.
 (2) Includes records, CDs, DVDs, tapes, cassettes, etc.
 (3) Includes personal computers, visual display units, printers and miscellaneous accessories accompanying them; also computer software packages such as operating systems, applications, languages, etc. 
 (4) Includes TV sets, CD players, stereo systems, radios, etc.
 Source: Eurostat (online data code: prc_hicp_aind)"

# Plot the second graph 
plot2 <- ggplot(data_long2, aes(x = category, y = value, fill = Color_Group)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.8, reverse = TRUE), width = 0.7) +
  coord_flip() +  # This flips the axes so categories are on the y-axis
  scale_fill_manual(values = c(
    "All-items HICP 2022-2023" = "#e2bbe5", 
    "All-items HICP 2018-2023" = "#b656bd",
    "Other 2022-2023" = "#9cade8", 
    "Other 2018-2023" = "#2644a7"
  ),
  breaks = c("Other 2018-2023","Other 2022-2023"),
  labels = c("2018-2023","2022-2023")) +  # Customize the colors for each specific group
  labs(title = "Harmonised Indices of Consumer Prices for Selected Cultural Goods and Services",
       subtitle = "EU, Annual Average Rates of Change 2018-2023 and 2022-2023 (%)",
       x = "",
       y = ""
      ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7, hjust = 0),
        
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = c(seq(-4, 6, by = 2)), minor_breaks= c(seq(-4, 6, by = 2)))


# Display the plot with footnote and logo
footnote_grob2 <- textGrob(cap2, x = unit(0, "npc"), y = unit(1, "npc"), 
                          just = c("left", "top"), gp = gpar(fontsize = 8))

# Combine the footnote and logo in a horizontal layout
footnote_with_logo2 <- arrangeGrob(footnote_grob2, logo_grob, ncol = 2, widths = c(4, 1))

# Combine the plot and the footnote with logo in a vertical layout
combined_grob2 <- arrangeGrob(plot2, footnote_with_logo2, ncol = 1, heights = c(4, 1))

# Display the combined plot
grid.draw(combined_grob2)


####################
###### Table #######
####################

# Define the dataset for the table and filter to include required countries and regions
data_api_all <- get_eurostat_data(id, 
                                  filters = list(coicop = cult_var_names, 
                                                 unit = "RCH_A_AVG"), 
                                  date_filter = time_EU) 

data_api_all <- left_join(data_api_all, labels, by="coicop")
view(data_api_all)
unique(data_api_all$geo)

data_api_all_2 <- data_api_all %>%
  filter(!(geo %in% c("UK","US","EA","EA19","EA20","EU28","EU27_2020","EEA")))

data_api_all_2 <- data_api_all_2 %>% mutate(time=as.numeric(as.character(time)))

# Sort the times again 2018-2023 and 2022-2023
values_2022_2023_all <- data_api_all_2 %>%
  filter(time == 2023) %>%            
  group_by(name, geo) %>%                             
  summarize(value = mean(values, na.rm = TRUE)) %>% # Calculate the mean of values for each category
  select(name, value, geo) %>%
  mutate(date = rep("2022-2023")) %>%
  arrange(name)

values_2018_2023_all <- data_api_all_2 %>%
  filter(time > 2018 & time <= 2023) %>%            # Filter for the years 2018 to 2023
  group_by(name, geo) %>%                             # Group by the category column
  summarize(value = mean(values, na.rm = TRUE)) %>% # Calculate the mean of values for each category
  select(name, value, geo) %>%
  mutate(date = rep("2018-2023")) %>%
  arrange(name)

# Bind and redefine the categories as needed
combo <- bind_rows(values_2018_2023_all, values_2022_2023_all) %>%
  mutate(name = str_replace_all(name, c(
    "Equipment for the reception, recording and reproduction of sound and picture" = "Equipment for the reception, recording  \nand reproduction of sound and picture (4)",
    "Information processing equipment" = "Information processing equipment (3)",
    "Recording media" = "Recording Media (2)",
    "Cultural services" = "Cultural Services (1)"
  )))

#Get the elements to 13 from 7 and save in reverse order and arrange
labels_graph2 <- rev(data_long2$category[7:13])
  
combo$name <- factor(combo$name, levels=labels_graph2)
combo2 <- combo %>%
  arrange(date) %>%
  arrange(name)

# Recode the country names
country_names <- c("Albania (⁵)" = "AL", "Austria" = "AT", "Belgium" = "BE", "Bulgaria" = "BG", 
                   "Switzerland" = "CH", "Cyprus" = "CY", "Czech Republic" = "CZ", "Germany" = "DE", 
                   "Denmark" = "DK", "Estonia" = "EE", "Greece" = "EL", "Spain" = "ES", 
                   "EU" = "EU", "Finland" = "FI", "France" = "FR", "Croatia" = "HR", 
                   "Hungary" = "HU", "Ireland" = "IE", "Iceland" = "IS", "Italy" = "IT", 
                   "Lithuania" = "LT", "Luxembourg" = "LU", "Latvia" = "LV", "Montenegro (⁵)" = "ME", 
                   "North Macedonia (⁵)" = "MK", "Malta" = "MT", "Netherlands" = "NL", "Norway" = "NO", 
                   "Poland" = "PL", "Portugal" = "PT", "Romania" = "RO", "Serbia (⁵)" = "RS", 
                   "Sweden" = "SE", "Slovenia" = "SI", "Slovakia" = "SK", "Türkiye (⁵)" = "TR", 
                   "Kosovo (⁵)(⁶)" = "XK")

# Convert the 'geo' column to a factor and recode it
combo2$geo <- fct_recode(factor(combo2$geo), !!!country_names)

combo3 <- combo2 %>%
  pivot_wider(names_from = c(name, date), values_from = value)

# Specified order of country names
country_order <- c("EU", "Belgium", "Bulgaria", "Czech Republic", "Denmark", "Germany", 
                   "Estonia", "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", 
                   "Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria", 
                   "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden", 
                   "Iceland", "Norway", "Switzerland", "Montenegro (⁵)", "North Macedonia (⁵)", "Albania (⁵)", 
                   "Serbia (⁵)", "Türkiye (⁵)", "Kosovo (⁵)(⁶)")

# Convert the 'geo' column to a factor with the specified levels
combo3$geo <- factor(combo3$geo, levels = country_order)

# Sort the dataset by the 'geo' column
combo3 <- combo3[order(combo3$geo), ]
colnames(combo3) <- c(" ",rep(c("2018-2023","2022-2023"), times=7))


# Define the header and the footnote
header <- c(1, rep(2,7))
names(header) <- c(" ", as.character(labels_graph2))

names(header)[3] <- "Newspapers
and periodicals"
names(header)[4] <- "Cultural
Services (1)"
names(header)[6] <- "Recording
Media (2)"
names(header)[7] <- "Information
processing
equipment (3)"
names(header)[8] <- "Equipment for
the reception,
recording and
reproduction
of sound
and picture (4)"

footnote_text <- "(¹) Includes cinemas, theatres, concerts; museums, libraries, zoological gardens; television and radio fees, hire of equipment and accessories for culture; and other cultural services.<br>
                  (²) Includes records, CDs, DVDs, tapes, cassettes, etc.<br>
                  (³) Includes personal computers, visual display units, printers and miscellaneous accessories accompanying them; also computer software packages such as operating systems, applications, languages, etc.<br>
                  (⁴) Includes TV sets, CD players, stereo systems, radios, etc.<br>
                  (⁵) Definition differs, see metadata <a href='https://ec.europa.eu/eurostat/cache/metadata/en/prc_hicp_esms.htm'>here</a>.<br>
                  (⁶) This designation is without prejudice to positions on status, and is in line with UNSCR 1244/1999 and the ICJ Opinion on the Kosovo declaration of independence."

# Create the table
final_table <- kable(combo3, format = "html", escape = F, digits = 1,  
                     caption = "<span style='color: black; font-weight: bold;'>Table 1: The all-items HICP and HICPs for cultural goods and services, annual average rates of change 2018-2023 and 2022-2023 (%)") %>%
  group_rows(" ", 1, 1) %>%
  group_rows(" ", 2, 28) %>%
  group_rows(" ", 29, 31) %>%
  group_rows(" ", 32, 36) %>%
  group_rows(" ", 37, 37) %>%
  add_header_above(header, background = "#f0dcf1", include_empty = F) %>%
  row_spec(row = 0, background = "#f0dcf1") %>%
  row_spec(row = 1, background = "#e1bae4") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, font_size = 12, fixed_thead = TRUE) %>%
  footnote(general =footnote_text, escape = F)

# Output the table
final_table

