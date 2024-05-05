#############################################################
# Article "Culture statistics - cultural employment"
# URL: https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Culture_statistics_-_cultural_employment
#############################################################

# install and load packages
install.packages("pacman")
library(pacman)      # for package loading with "p_load"

p_load(restatapi,    # for Eurostat data
       giscoR,       # for Eurostat geographical Data
       ggplot2,      # for graphic design
       highcharter,   # for interactive elements in graphics
      tidyverse,
      eurostat
      )


#load data for article "Culture statistics - cultural employment"
cultural_empl <- get_eurostat_data("cult_emp_sex")


####################################
# Map 1 - interactive - N.
###################################
# load new packages for the map 
p_load(ggmap, mapproj, rnaturalearth, rnaturalearthdata, sf, tidyverse)



# filter data for needed variables  
# (unit: PC_EMP = Percent of Total Employment, 
#        THS_PERS = Total number per 1000 persons)
cultural_empl_map <- cultural_empl %>% 
             filter(time == 2022, sex == "T", 
             geo != "EU27_2020", unit == "PC_EMP") %>% 
             select(geo, values) 


# 1. first possibility with nuts data
nuts <- gisco_get_nuts(year = 2021, nuts_level = 0)
merged_data <- merge(nuts, cultural_empl_map, 
                     by.x = "geo", by.y = "geo", all.x = TRUE)


ggplot(merged_data) +
geom_sf(aes(fill = values)) +
  scale_fill_continuous(breaks = c(3.45, 3.75, 4.05, 4.35, 4.65), 
                        labels = c("< 3.5", "3.5 -< 4", "4 -< 4.1", "4.1 -< 4.6", "≥ 4.6"),
                        type = "viridis") +
coord_sf(
  crs = 3035, xlim = c(2377294, 7453440),
   ylim = c(1313597, 5628510)
 ) +
 labs(title = "Cultural employment, 2022", subtitle = "(% of total employment)") +
  guides(fill = guide_legend(override.aes = list(fill = c("blue", "green", "yellow", "orange", "red")),
                             keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm"))) +
  theme(legend.position = c(0.85, 0.85), 
        legend.text = element_text(size = 6),
         # legend.title = element_text("EU = 3.8"),
        legend.key.size = unit(0.3, "cm"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())




# 2. possibility with "ne_countries()" --> with bordering countries, no filling yet
world <- ne_countries(scale = 50, returnclass = 'sf') 

map <- ggplot(world) + 
  geom_sf(aes(fill = continent), color = 'black') +
  coord_sf(crs = st_crs(3035),
           xlim = c(2000000, 7000000), 
           ylim = c(1500000, 5500000)) +
  scale_fill_manual(values = c('gray', NA, 'gray', '#fcd752', 
                               NA, NA, NA, NA), guide = 'none', 
                    na.value = 'white') +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(linewidth = 0.1, 
                                        color = '#80808080'))
map




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

