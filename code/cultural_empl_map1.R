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
      tidyverse)


#load data for article "Culture statistics - cultural employment"
cultural_empl <- get_eurostat_data("cult_emp_sex")


####################################
# Map 1 - interactive - N.
###################################
# load new packages for the map 
p_load(ggmap, mapproj, rnaturalearth, rnaturalearthdata, sf, tidyverse)




# filter data for needed variables
cultural_empl_2022 <- cultural_empl[cultural_empl$time == 2022, ]
cultural_empl_2022 <- cultural_empl_2022[cultural_empl_2022$sex == "T"]
cultural_empl_2022 <- subset(cultural_empl_2022, geo != "EU27_2020")
cultural_empl_2022 <- subset(cultural_empl_2022, unit == "PC_EMP")


# 1. first possibility with nuts data
nuts <- gisco_get_nuts(year = 2021, nuts_level = 0)
merged_data <- merge(nuts, cultural_empl_2022, by.x = "geo", by.y = "geo", all.x = TRUE)


ggplot(merged_data) +
geom_sf(aes(fill = values)) +
  # ETRS89 / ETRS-LAEA
coord_sf(
  crs = 3035, xlim = c(2377294, 7453440),
   ylim = c(1313597, 5628510)
 ) +
 labs(title = "Cultural employment")






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

