
#############################################################
### Quality of life indicators - overall experience of life
#############################################################
#https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Quality_of_life_indicators_-_overall_experience_of_life#EU_citizens_were_rather_satisfied_with_their_lives_in_2022
############################################################
## Gabriella Manuti manutigabriella@gmail.com
###########################################################

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

#install and load packages to save the figures in a excel file
pacman::p_load(plotly,
               png,
               writexl,
               gridExtra,
               openxlsx)


#All the figures and the map are saved as png in the same excel file, but in different worksheets
#To obtain an excel file for each figure, create a new workbook (the code is provided too)



#########
#Figure 1
#########
#Overall life satisfaction by country, 2022


####Parameters to set

#name of the data (id)
name_data <- "ilc_pw01"

#unit of measure
unit_data <- "RTG"  #0-10 scale

#international standard classification of education
edu_data <- "TOTAL"

#year of interest
year_data <- "2022"

#sex
sex_data <- "Total"

#age class
age_data <- "16 years or over"

#define plot title, subtitle, caption and colors
title <- "Overall life satisfaction, 2022"

subtitle <- "(mean scale 0-10)"

caption <- paste0("Figure 1: Overall life satisfaction, 2022\n(mean scale 0-10)\nSource: Eurostat: ilc_pw01")

#first color for the countries and the second one for the EU_level
colors <- c("#2644A7","#B09120")




#get the dataset

#Full names of the countries and then set label=TRUE in the get_eurostat_data
dsd<- get_eurostat_dsd(name_data)[concept=='geo']

ilc_pw01 <- get_eurostat_data(name_data, filters = c(unit = unit_data, isced11 = edu_data, indic_wb = "LIFESAT"), label=TRUE)



#list the countries excluded from the plot
excluded_countries <- c("European Union - 27 countries (from 2020)","Switzerland", "Norway", "Serbia", "Montenegro", "Türkiye")

#countries to plot (as a subset of the main dataset)
countries <- subset(ilc_pw01, sex == sex_data & age == age_data & time == year_data & !(geo %in% excluded_countries))

#subset of the main dataset to get the EU_value for the plot
countries_subset <- subset(ilc_pw01, sex == sex_data & age == age_data & time == year_data)

EU_value <- countries_subset$values[countries_subset$geo== "European Union - 27 countries (from 2020)"]



#plot Figure 1 and save it


figure1 <- ggplot(countries,aes(x = reorder(geo, values[geo != "European Union - 27 countries (from 2020)"], decreasing = TRUE), y = values)) +
  geom_col(width = 0.6, fill = colors[1]) +
  geom_hline(yintercept = EU_value, color = colors[2],linetype = "solid",linewidth =1) +
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
        legend.title = element_blank())+
  labs(x = NULL, y = NULL, title=title, subtitle= subtitle)

#to add the legend (not working)
#scale_fill_manual(name = NULL,
#values = c("Overall life satisfaction" = colors[1], "EU average" = colors[2]),
#labels = c("Overall life satisfaction", "EU average"))



#place Eurostat logo in the caption
logo_file <- "logo.png" 
logo <- readPNG(logo_file)
logo_grob <- rasterGrob(logo, interpolate=T)


#create a text grob for the footnote (caption)
footnote_grob_fig1 <- textGrob(caption, x = unit(0.05, "npc"), y = unit(1, "npc"), 
                               just = c("left", "top"), gp = gpar(fontsize = 9))

#combine the footnote and logo
footnote_with_logo_grob_fig1 <- arrangeGrob(footnote_grob_fig1, logo_grob, ncol = 2, widths = c(5, 1))


#combine the plot and the footnote_with_logo_grob
combined_grob_fig1 <- arrangeGrob(figure1, 
                                  footnote_with_logo_grob_fig1, 
                                  ncol = 1, 
                                  heights = unit.c(unit(1, "npc") - unit(1.5, "cm"), unit(2, "cm")))

#display the final plot - Figure 1 
grid.newpage()
grid.draw(combined_grob_fig1)



#save the final plot as a PNG file
ggsave("combined_plot.png", combined_grob_fig1, width = 10, height = 9)

#create a workbook
wb <- createWorkbook()

#add a worksheet
addWorksheet(wb, "Figure 1")

#insert the image into the worksheet
insertImage(wb, "Figure 1", "combined_plot.png", startRow = 1, startCol = 1)

#save the workbook to a file
saveWorkbook(wb, "Quality_of_life_indicators.xlsx", overwrite = TRUE)




############
#Map 1
###########

#Overall life satisfaction by age group, 2022

#To obtain Map 1, the nuts data about EU countries and neighbors and data about life satisfaction are needed. Then the difference between satisfaction level of 16-29 and above 65 years old is computed

#load the library
library(sf) 

####Parameters to set

#name of the data (id) for life satisfaction
name_data_map1 <- "ilc_pw01"

#unit of measure
unit_data_map1 <- "RTG"  #0-10 scale

#international standard classification of education
edu_data_map1 <- "TOTAL"

#sex
sex_data_map1 <- "T"

#age class
age_data_map1 <- c("Y16-29", "Y_GE65")

#year of interest
year_data_map1 <- 2022




#define plot title, subtitle and caption
title_map1 <- "Overall life satisfaction by age group, 2022"

subtitle_map1 <- "Difference between the satisfaction level of 16-29 and 65+ year olds (mean scale 0-10)"

caption_map1 <- paste0("Map 1: Overall life satisfaction by age group, 2022\nSource: Eurostat(ilc_pw01)")



#nuts for EU countries and neighbors
nuts <- gisco_get_countries(year = 2020,  resolution = "01")

#alternative: only for EU countries
#nuts <- gisco_get_nuts(year = 2021, nuts_level = 0, resolution = "01")


#get the data 
ilc_pw01_map <- get_eurostat_data(name_data_map1, filters = c(unit = unit_data_map1, isced11 = edu_data_map1, indic_wb = "LIFESAT"))


#list the countries excluded from the map
excluded_countries_map <- c("EU27_2020","CH", "NO", "RS", "ME", "TR")


#subset of the main dataset
countries_map <- ilc_pw01_map %>%
  filter(sex == "T" & age %in% age_data_map1 & time == year_data_map1 & !(geo %in% excluded_countries_map))


#compute the difference between satisfaction level of 16-19 and above 65 years old
#and store it ('difference') in a new dataset
countries_diff <- countries_map %>%
  pivot_wider(names_from = age, values_from = values) %>%
  mutate(difference = `Y16-29` - `Y_GE65`)



#merge the nuts data with the overall life satisfaction differences by age data
merged_data <- merge(nuts, countries_diff, by.x = "CNTR_ID", by.y = "geo", all.x = TRUE)


#add a column (called 'bins') in the 'merged_data' for the legend (for a better coding and visualisation)
#set the breaks and the labels for the legend
merged_data$bins <- cut(
  merged_data$difference,
  breaks = c(-Inf, -0.3, 0, 0.3, 1, Inf),
  labels = c("<-0.3", "-0.3-<0", "0-<0.3", "0.3-<1", ">=1"),
  include.lowest = TRUE
)

centroids <- st_centroid(merged_data)


##Two maps are created: one for Europe and one for Malta. Then the Malta map is overlapped on the main Europe map

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
        plot.margin = unit(c(1, 1, 1, 1), "cm") ) + 
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
  labs(title = title_map1, subtitle= subtitle_map1) +
  guides(fill = guide_legend(keywidth = unit(0.6, "cm"), keyheight = unit(0.3, "cm"), drop = TRUE)) +
  #annotate("text", x = 2380000, y = 1300000, 
  #label = "Source: Eurostat: ilc_pw01",
  #size = 2, color = "black", hjust = 0) +
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
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0, vjust = 0.5, face = "bold"),
        plot.title.position = "plot")

#annotate("text", x = 2380000, y = 1300000, 
# label = "Source: Eurostat:ilc_pw01",
# size = 2, color = "black", hjust = 0) +
#annotate("text", x = 5900000, y = 1350000, 
#label = "Administrative boundaries: © EuroGeographics © UN–FAO © Turkstat", 
#size = 2, color = "black", hjust = 0) +
#annotate("text", x = 5900000, y = 1300000, 
#  label = "Cartography: Eurostat – IMAGE, 12/2023", 
# size = 2, color = "black", hjust = 0)


plot(europe_map)



#combine the map of Europe and Malta and save it

map1 <- ggdraw(europe_map) +
  draw_plot(malta_map1, 0.6, 0.6, .4, .4) 

print(map1)


#place Eurostat logo in the caption
logo_file <- "logo.png" 
logo <- readPNG(logo_file)
logo_grob <- rasterGrob(logo, interpolate=T)


#create a text grob for the footnote (caption)
footnote_grob_map1 <- textGrob(caption_map1, x = unit(0.05, "npc"), y = unit(1, "npc"), 
                               just = c("left", "top"), gp = gpar(fontsize = 9))

#combine the footnote and logo
footnote_with_logo_grob_map1 <- arrangeGrob(footnote_grob_map1, logo_grob, ncol = 2, widths = c(5, 1))


#combine the plot and the footnote_with_logo_grob
combined_grob_map1 <- arrangeGrob(map1, 
                                  footnote_with_logo_grob_map1, 
                                  ncol = 1, 
                                  heights = unit.c(unit(1, "npc") - unit(1.5, "cm"), unit(2, "cm")))

#display the final plot - Figure 1 
grid.newpage()
grid.draw(combined_grob_map1)



#save the final map as a PNG file
ggsave("combined_map1.png", combined_grob_map1, width = 18, height = 18)

#create a new workbook
#wb <- createWorkbook()

#add a worksheet
addWorksheet(wb, "Map 1")

#insert the image into the worksheet
insertImage(wb, "Map 1", "combined_map1.png", startRow = 1, startCol = 1)

#save the workbook to a file
saveWorkbook(wb, "Quality_of_life_indicators.xlsx", overwrite = TRUE)




#########
#Figure 2
#########

#Life satisfaction by level of education, 2022

#Parameters to set

#name of the data (id)
name_data_fig2 <- "ilc_pw01"

#unit of measure
unit_data_fig2 <- "RTG"   #0-10 scale

#sex
sex_data_fig2 <- "Total"

#age class
age_data_fig2 <- "16 years or over"

#year of interest
year_data_fig2 <- "2022"

#international standard classification of education
#set the education level you want to exclude
edu_data_fig2 <- "All ISCED 2011 levels"


#alternative: set the levels you want but change the != in the subset
#edu_data_fig2 <- c("Less than primary, primary and lower secondary education (levels 0-2)", "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)", "Tertiary education (levels 5-8)")


#define plot title, subtitle, caption and colors
title_fig2 <- "Overall life satisfaction by level of education, 2022"

subtitle_fig2 <- "(mean scale 0-10)"

caption_fig2 <- paste0("Source: Eurostat: ilc_pw01 \nFigure 2: Overall life satisfaction by level of education, 2022 (mean scale 0-10)")

#first color for the Less than primary, primary and lower secondary education, second color for the Upper secondary an post-secondary  education and the third color for the Tertiary education
colors_fig2 <- c("#F8AE21","yellow", "#2644A7")



#get the data
ilc_pw01_2 <- get_eurostat_data(name_data_fig2, filters = c(unit = unit_data_fig2, indic_wb = "LIFESAT"),label=TRUE)


#list the countries excluded from the plot
excluded_countries_fig2 <- c("Switzerland", "Norway", "Serbia", "Montenegro", "Türkiye", "Kosovo*")

#countries to plot (as a subset of the main dataset)
education_countries <- subset(ilc_pw01_2, sex == sex_data_fig2 & age == age_data_fig2 & time == year_data_fig2 & isced11 != edu_data_fig2 & !(geo %in% excluded_countries_fig2))

#colors for the plot linked to the education levels
custom_colors <- c("Less than primary, primary and lower secondary education (levels 0-2)" = colors_fig2[1] ,"Upper secondary and post-secondary non-tertiary education (levels 3 and 4)" = colors_fig2[2] ,  "Tertiary education (levels 5-8)" = colors_fig2[3]) 


####
#re-arrange the data to plot them. First the values about the EU an then each country ordered by the Tertiary education level (decreasing order)

#set the level you want to use to order the data
level_to_order <- "Tertiary education (levels 5-8)"

#countries subset (from the subset) to select the EU_value and the other countries for the plot
eu_data <- subset(education_countries, geo == "European Union - 27 countries (from 2020)")
country_data <- subset(education_countries, geo != "European Union - 27 countries (from 2020)")

#order the countries by the chosen education level
country_data <- country_data %>%
  arrange(desc(ifelse(isced11 == level_to_order, values, NA)))

#re-arrange the dataset
ordered_data <- bind_rows(eu_data, country_data)


country_names <- unique(ordered_data$geo)


#plot and save it


figure2 <- ggplot(ordered_data, aes(x = factor(geo, levels = country_names), y = values, fill = isced11)) +
  scale_fill_manual(values = custom_colors, labels = c("Less than secondary education", "Secondary education", "Tertiary education")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.4) +
  labs(title = title_fig2 ,subtitle = subtitle_fig2, x = NULL, y = NULL) +
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



#place Eurostat logo in the caption
logo_file <- "logo.png" 
logo <- readPNG(logo_file)
logo_grob <- rasterGrob(logo, interpolate=T)

#create a text grob for the footnote (caption)
footnote_grob_fig2 <- textGrob(caption_fig2, x = unit(0.05, "npc"), y = unit(1, "npc"), 
                               just = c("left", "top"), gp = gpar(fontsize = 9))

#combine the footnote and logo
footnote_with_logo_grob_fig2 <- arrangeGrob(footnote_grob_fig2, logo_grob, ncol = 2, widths = c(5, 1))


#combine the plot and the footnote_with_logo_grob
combined_grob_fig2 <- arrangeGrob(figure2, 
                                  footnote_with_logo_grob_fig2, 
                                  ncol = 1, 
                                  heights = unit.c(unit(1, "npc") - unit(1.5, "cm"), unit(2, "cm")))

#display the final plot - Figure 2
grid.newpage()
grid.draw(combined_grob_fig2)



#save the final plot as a PNG file
ggsave("combined_plot_fig2.png", combined_grob_fig2, width = 12, height = 9)

#create a new workbook or add it to the already existing workbook "wb"
#wb <- createWorkbook()

#add a worksheet
addWorksheet(wb, "Figure 2")

#insert the image into the worksheet
insertImage(wb, "Figure 2", "combined_plot_fig2.png", startRow = 1, startCol = 1)

#save the workbook to a file
saveWorkbook(wb, "Quality_of_life_indicators.xlsx", overwrite = TRUE)





#########
#Figure 3
#########

#Overall life satisfaction by level of urbanisation 2022


#Parameters to set

#name of the data (id)
name_data_fig3 <- "ilc_pw02"

#unit of measure
unit_data_fig3 <- "RTG"   #0-10 scale

#year of interest
year_data_fig3 <- "2022"

#household composition level
hhcomp_data_fig3 <- "Total"

#income quantile
inc_data_fig3 <- "Total"

#degree of urbanisation
#set the degree of urbanisation you want to exclude
urb_data_fig3 <- "Total"

#alternative: set the levels you want but change the != in the subset
#urb_data_fig3 <- c("Cities", "Towns and suburbs", "Rural areas")



#define plot title, subtitle, caption and colors
title_fig3 <- "Overall life satisfaction by level of urbanisation 2022"

subtitle_fig3 <- "(mean scale 0-10)"

caption_fig3 <- paste0("Figure 3: Overall life satisfaction by level of urbanisation (mean scale 0-10) \nSource: Eurostat: ilc_pw02")

#first color for the Cities, second color for the Towns and suburbs and the third color for the Rural areas
colors_fig3 <- c("#F8AE21","yellow", "#2644A7")



#get the dataset
ilc_pw02 <- get_eurostat_data(name_data_fig3, filters = c(unit = unit_data_fig3, indic_wb = "LIFESAT"), label=TRUE)

#list the countries excluded from the plot
excluded_countries_fig3 <- c("Switzerland", "Norway", "Serbia", "Montenegro", "Türkiye", "Kosovo*")

#countries to plot (as a subset of the main dataset)
urbanisation_countries <- subset(ilc_pw02, time == year_data_fig3 & hhcomp== hhcomp_data_fig3 & quant_inc== inc_data_fig3 & deg_urb != urb_data_fig3 & !(geo %in% excluded_countries_fig3) )

#colors for the plot linked to each urbanisation level
custom_colors_urb <- c("Cities" = colors_fig3[1],"Towns and suburbs"= colors_fig3[2], "Rural areas" = colors_fig3[3]) 


####
#re-arrange the data to plot them. First the values about the EU an then each country ordered by the Cities urbanisation level (decreasing order)

#set the level you want to use to order the data
level_to_order_fig3 <- "Cities"

#countries subset to select the EU_value and the other countries for the plot
eu_data_urb <- subset(urbanisation_countries, geo == "European Union - 27 countries (from 2020)")
country_data_urb <- subset(urbanisation_countries, geo != "European Union - 27 countries (from 2020)")

#order the countries by the chosen degree of urbanisation
country_data_urb <- country_data_urb %>%
  arrange(desc(ifelse(deg_urb == level_to_order_fig3, values, NA)))

#re-arrange the dataset
ordered_data_urb <- bind_rows(eu_data_urb, country_data_urb)
country_names_urb <- unique(ordered_data_urb$geo)


#plot and save it


figure3 <- ggplot(ordered_data_urb, aes(x = factor(geo, levels = country_names_urb), y = values, fill = deg_urb)) +
  scale_fill_manual(values = custom_colors_urb, labels = c("Cities", "Small Towns", "Rural Areas")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.4) +
  labs(title = title_fig3, subtitle = subtitle_fig3, x = NULL, y = NULL) +
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


#print(figure3)


#place Eurostat logo in the caption
logo_file <- "logo.png" 
logo <- readPNG(logo_file)
logo_grob <- rasterGrob(logo, interpolate=T)

#create a text grob for the footnote (caption)
footnote_grob_fig3 <- textGrob(caption_fig3, x = unit(0.05, "npc"), y = unit(1, "npc"), 
                               just = c("left", "top"), gp = gpar(fontsize = 9))

#combine the footnote and logo
footnote_with_logo_grob_fig3 <- arrangeGrob(footnote_grob_fig3, logo_grob, ncol = 2, widths = c(5, 1))


#combine the plot and the footnote_with_logo_grob
combined_grob_fig3 <- arrangeGrob(figure3, 
                                  footnote_with_logo_grob_fig3, 
                                  ncol = 1, 
                                  heights = unit.c(unit(1, "npc") - unit(1.5, "cm"), unit(2, "cm")))

#display the final plot - Figure 3
grid.newpage()
grid.draw(combined_grob_fig3)



#save the final plot as a PNG file
ggsave("combined_plot_fig3.png", combined_grob_fig2, width = 12, height = 9)

#create a new workbook or add it to the already existing workbook "wb"
#wb <- createWorkbook()

#add a worksheet
addWorksheet(wb, "Figure 3")

#insert the image into the worksheet
insertImage(wb, "Figure 3", "combined_plot_fig3.png", startRow = 1, startCol = 1)

#save the workbook to a file
saveWorkbook(wb, "Quality_of_life_indicators.xlsx", overwrite = TRUE)






#########
#Figure 4
#########

#Overall life satisfaction by demographic characteristics at EU level, 2018 and 2022

#To obtain Figure 4 two different datasets are need: 
# - ilc_pw01 for the information about the sex
# - ilc_pw02 for the information about the household composition and the income quantile
#Both datasets are aggregated, i.e. only the EU level is taken into consideration. 


##Parameters to set for the ilc_pw01

#name of the data (id)
name_data_fig4_2 <- "ilc_pw01"

#international standard classification of education
edu_data_fig4 <- "TOTAL"

#sex
sex_data_fig4 <- c("Males", "Females")

#age class
age_data_fig4 <- "16 years or over"

#time of interest
year_data_fig4 <- c("2018", "2022")



##Parameters to set for the ilc_pw02

#name of the data (id)
name_data_fig4 <- "ilc_pw02"

#unit of measure
unit_data_fig4 <- "RTG"   #0-10 scale

#year of interest
year_data_fig4 <- c("2018","2022")

#household composition level
#set the household composition you want to exclude
hhcomp_data_fig4 <- "Total"

#income quantile
#set the income quantile you want to exclude
inc_data_fig4 <- "Total"

#degree of urbanisation
urb_data_fig4 <- "Total"




#define plot title, subtitle, caption and colors
title_fig4 <- "Overall life satisfaction by demographic characteristics at EU level, 2018 and 2022"

subtitle_fig4 <- "(mean scale 0-10)"

caption_fig4 <- paste0("All data are estimates\nSource: Eurostat: ilc_pw01, ilc_pw02")

#first color for 2018 and the second one for 2022
colors_fig4 <- c("#2644A7","#B09120")



#get the different data

ilc_pw01_fig4 <- get_eurostat_data(name_data_fig4_2, filters = c(unit = unit_data_fig4, isced11 = edu_data_fig4, indic_wb = "LIFESAT"), label=TRUE)

ilc_pw02_fig4 <- get_eurostat_data(name_data_fig4, filters = c(unit = unit_data_fig4, indic_wb = "LIFESAT"), label=TRUE)


#subset of the dataset and then re-arrange the data by adding a new column with the value to plot. The new column (called 'category') refers to the household composition, income level and gender 

#from the ilc_pw02
household_countries <- subset(ilc_pw02_fig4, time == year_data_fig4 & hhcomp != hhcomp_data_fig4 & quant_inc != inc_data_fig4 & deg_urb == urb_data_fig4 & geo == "European Union - 27 countries (from 2020)")


#data on the household composition
hhcomp_data <- household_countries %>%
  filter(hhcomp != "Total") %>%
  mutate(category = hhcomp)

#data on the income
quant_inc_data <- household_countries %>%
  filter(quant_inc != "Total") %>%
  mutate(category = quant_inc)


#data on the gender (from the ilc_pw01)
gender_data <- subset(ilc_pw01_fig4, sex %in% sex_data_fig4 & age == age_data_fig4 & time %in% year_data_fig4 & geo == "European Union - 27 countries (from 2020)") %>%
  mutate(category = sex)



#new dataset to use, where each subset is combined (same order of the final plot)
combined_data <- bind_rows(hhcomp_data,gender_data, quant_inc_data)


#check
#only one row per category and year by aggregating the data
aggregated_data <- combined_data %>%
  group_by(category, time) %>%
  summarise(values = mean(values, na.rm = TRUE)) %>%
  ungroup()


##For a better visualization in the plot, 'aggregated_data' is splitted using the year: 2018 and 2022. Then spaces are added. 


#split the aggregated data in 2018 and 2022
data_2018 <- filter(aggregated_data, time == 2018)
data_2022 <- filter(aggregated_data, time == 2022)


#add spacing rows for a better visualization in the plot
cat_1_space <- length(unique(hhcomp_data$category))
cat_2_space <- cat_1_space + length(unique(gender_data$category))


#Convert the category column to a character type. Then add a row with an empty category and NA values before the row number cat_1_space + 1. Then add another row with a single space in the category and NA values before the row number cat_2_space + 2.
#To guarantee a space between the household composition data and the gender data and between the gender data and the income quantile data, in both years.

data_2022 <- data_2022 %>%
  mutate(category = as.character(category)) %>%
  add_row(category = "", time = factor(2022, levels = levels(data_2022$time)), values = NA, .before = (cat_1_space+1)) %>%
  add_row(category = " ", time = factor(2022, levels = levels(data_2022$time)), values = NA, .before = (cat_2_space+2))


data_2018 <- data_2018 %>%
  mutate(category = as.character(category)) %>%
  add_row(category = "", time = factor(2018, levels = levels(data_2018$time)), values = NA, .before = (cat_1_space+1)) %>%
  add_row(category = " ", time = factor(2018, levels = levels(data_2018$time)), values = NA, .before = (cat_2_space+2))


#convert the category column back to a factor with levels ordered by their unique values in data_2022 and data_2018

data_2022$category <- factor(data_2022$category, levels = unique(data_2022$category))
data_2018$category <- factor(data_2018$category, levels = unique(data_2018$category))




#plot and save it

figure4 <- ggplot() +
  geom_bar(data = data_2022, aes(x = category, y = values, fill = time), 
           stat = "identity", width = 0.3) +
  geom_point(data = data_2018, aes(x = category, y = values, color = "2018"), 
             shape = 15, size = 3) +
  labs(title = title_fig4, subtitle = subtitle_fig4, x = NULL, y = NULL) +
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
  scale_fill_manual(values = c("2022" = colors_fig4[2])) +
  scale_color_manual(values = c("2018" = colors_fig4[1])) +
  guides(fill = guide_legend(title = "Year"))

print(figure4)


#place Eurostat logo in the caption
logo_file <- "logo.png" 
logo <- readPNG(logo_file)
logo_grob <- rasterGrob(logo, interpolate=T)

#create a text grob for the footnote (caption)
footnote_grob_fig4 <- textGrob(caption_fig4, x = unit(0.05, "npc"), y = unit(1, "npc"), 
                               just = c("left", "top"), gp = gpar(fontsize = 9))

#combine the footnote and logo
footnote_with_logo_grob_fig4 <- arrangeGrob(footnote_grob_fig4, logo_grob, ncol = 2, widths = c(5, 1))


#combine the plot and the footnote_with_logo_grob
combined_grob_fig4 <- arrangeGrob(figure4, 
                                  footnote_with_logo_grob_fig4, 
                                  ncol = 1, 
                                  heights = unit.c(unit(1, "npc") - unit(1.5, "cm"), unit(2, "cm")))

#display the final plot - Figure 4
grid.newpage()
grid.draw(combined_grob_fig4)



#save the final plot as a PNG file
ggsave("combined_plot_fig4.png", combined_grob_fig4, width = 12, height = 9)

#create a new workbook or add it to the already existing workbook "wb"
#wb <- createWorkbook()

#add a worksheet
addWorksheet(wb, "Figure 4")

#insert the image into the worksheet
insertImage(wb, "Figure 4", "combined_plot_fig4.png", startRow = 1, startCol = 1)

#save the workbook to a file
saveWorkbook(wb, "Quality_of_life_indicators.xlsx", overwrite = TRUE)









