# Load tidyverse package

library(tidyverse)
library(readxl)
library(grid)
library(png)
# Read in specific rows including time from the same sheet
time <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 1", range = "D73:N73", col_names = F, col_types = "numeric")
all_hicp <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 1", range = "D75:N75", col_names = F)
equip <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 1", range = "D77:N77", col_names = F)
info_equip <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 1", range = "D78:N78", col_names = F)
rec_media <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 1", range = "D79:N79", col_names = F)
cult_serv <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 1", range = "D80:N80", col_names = F)
books <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 1", range = "D81:N81", col_names = F)
newspapers <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 1", range = "D82:N82", col_names = F)

# Bind rows and create a new column for data frame names
combined_rows <- bind_rows(time, all_hicp, equip, info_equip,
                           rec_media, cult_serv, books, newspapers)

# Add more rows as needed using the same pattern
combined_cols <- as_tibble(t(combined_rows))
colnames(combined_cols) <- c("time","all_hicp","equip","info_equip",
                             "rec_media","cult_serv","books","newspapers")

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


# Define codes for each variable
color_palette <- c(
  all_hicp = "#d7a2db",        
  equip = "#b0185e",           
  info_equip = "#388ae2",     
  rec_media = "#672dc4",       
  cult_serv = "#2644a7",       
  books = "#b09120",           
  newspapers = "#b656bd"       
)


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
       cols='markers', caption=cap) +
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

plot1

#               graph 2

category <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 2", range = "C63:C69", col_names = FALSE)
category[7, 1] <- "Equipment for the reception, recording  
and reproduction of sound and picture (4)"
category[3, 1] <- "Cultural Servies (1)"
category[5,1] <- "Recording Media (2)"
category[6, 1] <- "Information processing equipment (3)"
values_2018_2023 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 2", range = "E63:E69", col_names = FALSE)
values_2022_2023 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 2", range = "F63:F69", col_names = FALSE)
period <-  as.character(read_excel("EU_HICP_DATA_24.xlsx", sheet = "Figure 2", range = "E59:F59", col_names = FALSE))

data <- data.frame(
  Category = category[[1]],
  `2022-2023` = values_2022_2023[[1]],
  `2018-2023` = values_2018_2023[[1]])

cap2 = "Note: average indices reflect changes in EU membership (when a Member State joins the EU, its HICPs are chained with the aggregate index at the time of the accession).
 (1) Includes cinemas, theatres, concerts; museums, libraries, zoological gardens; television and radio fees, hire of equipment and accessories for culture; and other cultural services.
 (2) Includes records, CDs, DVDs, tapes, cassettes, etc.
 (3) Includes personal computers, visual display units, printers and miscellaneous accessories accompanying them; also computer software packages such as operating systems, applications, languages, etc. 
 (4) Includes TV sets, CD players, stereo systems, radios, etc.
 Source: Eurostat (online data code: prc_hicp_aind)"

# Pivot the data to a long format
data_long <- pivot_longer(data, cols = -Category,names_to = "Period",values_to = "Value")


labels_graph2 <- data_long$Category |> unique() |> rev()
data_long$Category <- factor(data_long$Category, levels = labels_graph2)
data_long$Period <- rev(factor(data_long$Period))

data_long <- data_long %>%
  mutate(Color_Group = ifelse(Category == "All-items HICP", paste(Category, Period), paste("Other", Period)))

plot <- ggplot(data_long, aes(x = Category, y = Value, fill = Color_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +  # This flips the axes so categories are on the y-axis
  scale_fill_manual(values = c(
    "All-items HICP X2018.2023" = "#e2bbe5", 
    "All-items HICP X2022.2023" = "#b656bd",
    "Other X2018.2023" = "#9cade8", 
    "Other X2022.2023" = "#2644a7"
  ),
  breaks = c("Other X2018.2023","Other X2022.2023"),
  labels = c("2018-2023","2022-2023")) +  # Customize the colors for each specific group
  labs(title = "Harmonised Indices of Consumer Prices for Selected Cultural Goods and Services",
       subtitle = "EU, Annual Average Rates of Change 2018-2023 and 2022-2023",
       x = "",
       y = "",
       caption = cap2) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7, hjust = 0),
        
        panel.grid.major.y = element_blank()) +
  
  scale_y_continuous(breaks = c(seq(-4, 6, by = 2)), minor_breaks= c(seq(-4, 6, by = 2)))


# Display the plot
print(plot)

#table
time_t <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "D10:Q10", col_names = F, col_types = "text")

all_hicp_t1 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "D11:D47", col_names = F, col_types = "numeric")
all_hicp_t2 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "E11:E47", col_names = F, col_types = "numeric")
newspapers_t1 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "F11:F47", col_names = F, col_types = "numeric")
newspapers_t2 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "G11:G47", col_names = F, col_types = "numeric")
cult_serv_t1 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "H11:H47", col_names = F, col_types = "numeric")
cult_serv_t2 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "I11:I47", col_names = F, col_types = "numeric")
books_t1 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "J11:J47", col_names = F, col_types = "numeric")
books_t2 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "K11:K47", col_names = F, col_types = "numeric")
rec_media_t1 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "L11:L47", col_names = F, col_types = "numeric")
rec_media_t2 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "M11:M47", col_names = F, col_types = "numeric")
info_equip_t1 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "N11:N47", col_names = F, col_types = "numeric")
info_equip_t2 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "O11:O47", col_names = F, col_types = "numeric")
equip_t1 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "P11:P47", col_names = F, col_types = "numeric")
equip_t2 <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "Q11:Q47", col_names = F, col_types = "numeric")

categories <- read_excel("EU_HICP_DATA_24.xlsx", sheet = "Table 1", range = "C11:C47", col_names = F)

combined_cols <- bind_cols(categories, all_hicp_t1, all_hicp_t2, newspapers_t1, newspapers_t2, cult_serv_t1, cult_serv_t2,
                           books_t1, books_t2, rec_media_t1, rec_media_t2, info_equip_t1, info_equip_t2, equip_t1, 
                           equip_t2)

colnames(combined_cols) <- c(" ",time_t)

combined_cols

library(knitr)
library(kableExtra)

header <- c(1, rep(2,7))
names(header) <- c(" ", rev(labels_graph2))

# Create the table
kable_styling <- kable(combined_cols, format = "html", escape = F, digits = 1) %>%
  group_rows(" ", 1, 1) %>%
  group_rows(" ", 2, 28) %>%
  group_rows(" ", 29, 31) %>%
  group_rows(" ", 32, 36) %>%
  group_rows(" ", 37, 37) %>%
  add_header_above(header, background = "#f0dcf1", include_empty = F) %>%
  row_spec(row = 0, background = "#f0dcf1") %>%
  row_spec(row = 1, background = "#e1bae4") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, font_size = 12, fixed_thead = TRUE)

# Output the table
kable_styling

