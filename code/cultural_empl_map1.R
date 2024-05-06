#############################################################
# Article "Culture statistics - cultural employment"
# URL: https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Culture_statistics_-_cultural_employment
#############################################################

# install and load packages
install.packages("pacman")
library(pacman)       # for package loading with "p_load"

p_load(restatapi,     # for Eurostat data
        giscoR,       # for Eurostat geographical Data
        ggplot2,      # for graphics
        highcharter,  # for interactive elements in graphics
        tidyverse,    # for data cleaning
        # eurostat, 
        cowplot       # for layering maps with "ggdraw"
        )


#load data for article "Culture statistics - cultural employment"
cultural_empl <- get_eurostat_data("cult_emp_sex")


####################################
# Map 1 - interactive - N.
###################################

# filter data for needed variables  
# (unit: PC_EMP = Percent of Total Employment, 
#        THS_PERS = Total number per 1000 persons)
cultural_empl_map <- cultural_empl %>% 
             filter(time == 2022, sex == "T", 
             geo != "EU27_2020", unit == "PC_EMP") %>% 
             select(geo, values) 


# 1. first possibility with nuts data
# get nuts data
nuts <- gisco_get_nuts(year = 2021, nuts_level = 0)

# merge the nuts data with the cultural employment data
merged_data <- merge(nuts, cultural_empl_map, 
                     by.x = "geo", by.y = "geo", all.x = TRUE)

# create a data frame with only Malta for augmentation
malta <- merged_data[merged_data$geo == "MT", ]


# create a map that shows Malta only
malta_map <- ggplot(malta) +
  geom_sf(aes(fill = values)) + 
  scale_fill_continuous(breaks = c(3.45, 3.75, 4.05, 4.35, 4.65), 
  type = "viridis") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "none") 
  

# create a map of Europe
europe_map <- ggplot(merged_data) +
  geom_sf(aes(fill = values)) +
      scale_fill_continuous(breaks = c(3.45, 3.75, 4.05, 4.35, 4.65), 
          labels = c("≥ 4.6", "4.1 -< 4.6","4 -< 4.1", "3.5 -< 4", "< 3.5"),
          type = "viridis") +
  coord_sf(crs = 3035, xlim = c(2377294, 7453440), ylim = c(1313597, 5628510)) +
  labs(title = "Cultural employment, 2022", subtitle = "(% of total employment)") +
  guides(fill = guide_legend(keywidth = unit(0.6, "cm"), keyheight = unit(0.3, "cm"), 
                             title="EU = 3.8")) +
  annotate("text", x = 2380000, y = 1350000, 
         label = "Definition differs for Spain and France (see LFS metadata).",
         size = 2, color = "black", hjust = 0) +
  annotate("text", x = 2380000, y = 1300000, 
           label = "Eurostat (online data code: cult_emp_sex)",
           size = 2, color = "black", hjust = 0) +
  annotate("text", x = 5900000, y = 1350000, 
           label = "Administrative boundaries: © EuroGeographics © UN–FAO © Turkstat", 
           size = 2, color = "black", hjust = 0) +
  annotate("text", x = 5900000, y = 1300000, 
           label = "Cartography: Eurostat – IMAGE, 07/2023", 
           size = 2, color = "black", hjust = 0) +
theme(legend.position = c(0.7, 0.55), 
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 7),
      legend.key.size = unit(0.3, "cm"),
      legend.background = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(), 
      axis.ticks = element_blank(), 
      panel.background = element_blank()
      ) 

# combine the map of Europe and of Malta
ggdraw(europe_map) +
  draw_plot(malta_map, .6, .5, .7, .1, 
            width = 0.3, height = 0.3) 

# to do:
# 1. find better map data that includes all countries + neighbouring countries & 
#     shows a more precise form of Malta
# 2. fix coloring according to the eurostat color scheme
# 3. adjust layout according to the layout rules
# 4. add interactive feature







# 2. possibility with "ne_countries()" --> with bordering countries, no filling yet
# p_load(rnaturalearth) # for ne_countries()

# world <- ne_countries(scale = 50, returnclass = 'sf') 

# ggplot(world) + 
#  geom_sf(aes(fill = continent), color = 'black') +
#  coord_sf(crs = st_crs(3035),
#           xlim = c(2000000, 7000000), 
#           ylim = c(1500000, 5500000)) +
#  scale_fill_manual(values = c('gray', NA, 'gray', '#fcd752', 
#                               NA, NA, NA, NA), guide = 'none', 
#                    na.value = 'white') +
#  theme(panel.background = element_rect(fill = 'white'),
#        panel.grid = element_blank())





####################################
# Figure 1 - P.
####################################




###################################
# Table 1 - J.
###################################



###################################
# Table 2
####################################


###################################
# Table 3
###################################


###################################
# Table 4
###################################





###################################
# Table 2
###################################




###################################
# Table 3
###################################

