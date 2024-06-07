#############################################################
# Article "Culture statistics - cultural employment"
# URL: https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Culture_statistics_-_cultural_employment
#############################################################

# install and load packages
pacman::p_load(restatapi,     # for Eurostat data
        giscoR,       # for Eurostat geographical Data
        ggplot2,      # for graphics
        highcharter,  # for interactive elements in graphics
        tidyverse,    # for data cleaning
        # eurostat, 
        cowplot       # for layering maps with "ggdraw"
        )





# color palette
clrs <- read.csv("C:\\Users\\User\\Documents\\emos-codinglab-2024\\code\\estat_colour_themes.txt",sep="\t",stringsAsFactors = F)
image(c(1:6),c(1:nrow(clrs)),matrix(1:(6*nrow(clrs)),6,nrow(clrs)),col=as.vector(t(apply(as.matrix(clrs[,c(3:8)]),2,rev))), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n",
      main = "Eurostat colour palettes")
text(c(0.5), c(1:nrow(clrs)), adj=c(0,1),rev(clrs$theme_desc), col = clrs$text_light)
text(c(1.5), c(1:nrow(clrs)), adj=c(0,1),rev(clrs$theme_desc), col = clrs$text_dark)

my_col <- clrs[2, 3:7]

####################################
# Map 1 - interactive 
###################################

#load data for article "Culture statistics - cultural employment"
cult_empl_sex <- get_eurostat_data("cult_emp_sex")


# filter data for needed variables  
# (unit: PC_EMP = Percent of Total Employment, 
#        THS_PERS = Total number per 1000 persons)
cultural_empl_map <- cult_empl_sex %>% 
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
  scale_fill_gradient(breaks = c(3.45, 3.75, 4.05, 4.35, 4.65),
                      low = my_col[5],  # Specify the low color
                      high = my_col[1]  # Specify the high color
  ) +
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
  scale_fill_gradientn(breaks = c(3.45, 3.75, 4.05, 4.35, 4.65),
                      labels = c("≥ 4.6", "4.1 -< 4.6","4 -< 4.1", "3.5 -< 4", "< 3.5"),
                      # low = my_col[5],  # Specify the low color
                      # high = my_col[1],  # Specify the high color
                      colours = my_col,
                      na.value = "lightgrey") +
  coord_sf(crs = 3035, xlim = c(2377294, 7453440), ylim = c(1313597, 5628510)) +
  labs(title = "Cultural employment, 2022", subtitle = "(% of total employment)") +
  guides(fill = guide_legend(keywidth = unit(0.6, "cm"), keyheight = unit(0.3, "cm"), 
                             title="EU = 3.8", low = my_col[5],
                             high = my_col[1])) +
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
#     shows a more precise form of Malta --> © EuroGeographics © UN–FAO © Turkstat
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
# Figure 1 
####################################

cult_empl_fig1 <- cult_empl_sex %>% filter(time %in% 2019:2022, sex == "T", 
                                       #  geo != "EU27_2020", 
                                           unit == "THS_PER")



# calculate the rate of change: (value_currentyear−value_previousyear)/value_previousyear*100

cult_empl_fig1 <-  cult_empl_fig1 %>%
                           # dplyr::arrange(geo, year) %>%
                            group_by(geo) %>%
                            mutate(rate_of_change = (values - lag(values)) / lag(values) * 100) %>%
                            filter(!is.na(rate_of_change))


# Specify the desired order of countries
desired_order <- c("EU27_2020", "CY", "LU", "IE", "SE", "NL", "PL", "LT", 
                   "DE", "IT", "RO", "PT", "MT", "FR", "AT", "FI", "DK", 
                   "SK", "ES", "EL",
                   "BE", "SI", "HU", "LV", "EE", "HR", 
                   "CZ", "BG", "NO", "IS", "CH", "RS", "MK", "TR", "ME")

# Reorder the 'geo' variable
cult_empl_fig1$geo <- factor(cult_empl_fig1$geo, levels = desired_order)

# Barplot
ggplot(cult_empl_fig1 , aes(x = geo, y = rate_of_change, fill = time)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_hline(yintercept = seq(-22, 18, by = 5), color = "black", linetype = "solid" )+
  labs(title = "Cultural employment ‒ annual rates of change, 2020-2022",
       subtitle = "(%)",
       x = "Country",
       y = "Rate of Change (%)",
       fill = "Year") +
  scale_y_continuous(breaks = seq(-22, 18, by = 5)) +
  scale_x_discrete(labels = c("EU27_2020" = "EU", "CY" = "Cyprus", "LU" = "Luxemburg", 
                              "IE" = "Ireland", "SE" = "Sweden", "NL" = "Netherlands", 
                              "PL" = "Poland", "LT" = "Lithuania", 
                              "DE" = "Germany (')", "IT" = "Italy", "RO" = "Romania", 
                              "PT" = "Portugal", "MT" = "Malta", "FR" = "France (')", 
                              "AT" = "Austria", "FI" = "Finland", "DK" = "Denmark", 
                              "SK" = "Slovakia", "ES" = "Spain (')", "EL" = "Greece",
                              "BE" = "Belgium", "SI" = "Slovenia", "HU" = "Hungary",
                              "LV" = "Latvia", "EE" = "Estonia", "HR" = "Croatia", 
                              "CZ" = "Czechia", "BG" = "Bulgaria", "NO" = "Norway", 
                              "IS" = "Iceland (')", "CH" = "Switzerland", "RS" = "Serbia", 
                              "MK" = "North Macedonia", "TR" = "Turkiye", "ME" = "Montenegro"
  )) + 
  scale_fill_manual(values = c("2020" = "#B09120", "2021" = "#2644A7", "2022" = "#E04040"), 
                    labels = c("2020" = "2019-2020", "2021" = "2020-2021", "2022" = "2021-2022")) +
  theme_minimal() +
  theme(legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
       # axis.ticks.x.bottom = element_line(),
      #  axis.line.x = element_line(),
        panel.grid = element_line(), 
        axis.ticks = element_blank(), 
        panel.background = element_blank(),
        #plot.margin = margin(40, 150, 80, 40),
       # plot.caption=element_text(hjust = 0)
      ) 



# to do: 
# adjust for missing values
# spacing between countries
# layout details --> fonts, sizes, etc.
# footnotes 
# right coloring scheme 

###################################
# Table 1 
###################################






###################################
# Figure 2: nearly finished
####################################



cult_emp_n2 <- get_eurostat_data("cult_emp_n2") %>% 
  filter(time != "2011") %>% 
  filter(nace_r2 %in% c("C18", "J58", "J59", "J60", "M74", "R90", "R91")) %>% 
  filter(geo %in% "EU27_2020")




# create labels and order for nace_r2 as a factor
cult_emp_n2$nace_r2 <- factor(cult_emp_n2$nace_r2, levels = c('R90', 'M74', 'C18', 'J58', 'R91', 'J59', 'J60'), 
                                                              labels = c(
                                                                  "Creative, arts and entertainment activities (NACE, R90)",
                                                                 "Other professional, scientific and technical activities \n(NACE, M74.1, M74.2, M74.3)",
                                                                  "Printing and reproduction of record media (NACE,\nC18)",
                                                                  "Selected publishing activities (NACE, part of J58)",
                                                                 "Libraries, archives, museums and other cultural\nactivities (NACE, R91)",
                                                                 "Motion picture, video and television programme\nproduction, sound recording and music publishing\nactivities (NACE, J59)",
                                                                 "Programming and broadcasting activities (NACE, J60)"))

# Plot
cult_emp_n2 %>%
  ggplot( aes(x=time, y= values, group= nace_r2, color=nace_r2)) +
  geom_line(size = 2) +
  # scale_color_viridis(discrete = TRUE) +
#  ggtitle("Evolution of cultural employment by selected NACE Rev. 2 activities, EU, 2012-2022", "(thousands)") + 
  labs(title = "Evolution of cultural employment by selected NACE Rev. 2 activities, EU, 2012-2022", 
       subtitle = "(thousands)", 
       caption = "Note: a break in time series for all countries for which 2021 data are available due to the implementation of the new Regulation (EU)2019/1700, also called the integrated European Social Statistics Framework Regulation (IESS FR)
(see LFS metadata)

Source: Eurostat(online data code: cult_emp_n2") +
  scale_y_continuous(breaks = seq(0, 1200, by = 200), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  geom_hline(yintercept = seq(0, 1200, by = 200), color = "black", linetype = "dashed") +
  theme(legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 12),
        legend.key.height = unit(3.5, "lines"),
        #legend.position.inside = c(0.9, 0.9),
        plot.title = element_text(face = "bold", size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x.bottom = element_line(),
        axis.line.x = element_line(),
        panel.grid = element_line(), 
        axis.ticks = element_blank(), 
        panel.background = element_blank(),
        plot.margin = margin(40, 150, 80, 40),
        plot.caption=element_text(hjust = 0)) 


# to do: 
# adjust layout / colours / fonts



#############################
# alternative code for footnotes

# annotate("text", x = -Inf, y = -Inf, # x = 0, y = -0.9,  
#           label = 
#           "Note: a break in time series for all countries for which 2021 data are available due to the implementation of the new Regulation (EU)2019/1700, also called the integrated European Social Statistics Framework Regulation (IESS FR) (see LFS metadata)",
#           size = 2, color = "black", hjust = 0, vjust = 0) +
# annotate("text",  x = Inf, y = -Inf,
#           label = "Source: Eurostat(online data code: cult_emp_n2",
#           size = 2, color = "black", hjust = 0) 


###################################
# Figure 3: not finished
###################################
lfsa_egan2 <- get_eurostat_data("lfsa_egan2")
cult_emp_age <- get_eurostat_data("cult_emp_age")
lfsa_egan<- get_eurostat_data("lfsa_egan")
cult_emp_edu <- get_eurostat_data("cult_emp_edu")
lfsa_egaed <- get_eurostat_data("lfsa_egaed")



# Plot sex

cult_emp_sex_fig3 <- cult_empl_sex %>% 
                     filter(time == 2022, geo %in% "EU27_2020", unit %in% "PC_EMP", 
                            sex != "T")


lfsa_egaed_22 <- lfsa_egaed %>% filter(time == 2022, geo %in% "EU27_2020")
                  
tot_empl_fig3 <- 


ggplot(cult_emp_sex_fig3, aes(x = "", y = values, fill = sex)) +
  geom_bar(stat = "identity", width = 0.5)

# momentary challenge: which dataset do I use to get the total employment values? 



###################################
# Figure 4: nearly finished 
###################################

# filter for needed variables 
cultural_empl_fig2 <- cult_empl_sex %>% 
    filter(time != "2011") %>% 
    filter(geo %in% "EU27_2020") %>% 
    filter(unit %in% "THS_PER") %>% 
    filter(sex != "T")


# create factor for sex with the right order and labels
cultural_empl_fig2$sex <- factor(cultural_empl_fig2$sex, levels = c("M", "F"), 
       labels = c("Male", "Female"))


# the line plot
cultural_empl_fig2 %>%
  ggplot( aes(x=time, y= values, group= sex, color= sex)) +
  geom_line(size = 2) +
  geom_point(size = 3) +
  labs(title = "Evolution of cultural employment in the EU by sex, 2012-22", 
       subtitle = "(thousands)", 
       caption = "Note: a break in time series for all countries for which 2021 data are available due to the implementation of the new Regulation (EU)2019/1700, also called the integrated
European Social Statistics Framework Regulation (IESS FR) (see LFS metadata)

Source: Eurostat(online data code: cult_emp_sex") +
  scale_y_continuous(breaks = seq(2500, 4500, by = 200), limits = c(2500, 4500), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  geom_hline(yintercept = seq(2500, 4500, by = 200), color = "black", linetype = "dashed") +
  theme(legend.text = element_text(face = "bold", size = 12),
        #legend.key.height = unit(3.5, "lines"),
        plot.title = element_text(face = "bold", size = 18),
        legend.background = element_blank(),
        #legend.position.inside = c(0.9, 0.9),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x.bottom = element_line(),
        axis.line.x = element_line(),
        panel.grid = element_line(), 
        axis.ticks = element_blank(), 
        panel.background = element_blank(),
        plot.margin = margin(40, 150, 80, 40),
        plot.caption=element_text(hjust = 0)) # +
       # coord_cartesian(xlim = c(2012, 2022), clip = 'off')


# Problems left: 
# how can I have the full dots at the beginning and the end without having a longer x-axis? 
# how to move the legend farther up? 
# layout/fonts/ colours are not right yet

###################################
# Table 2
###################################




###################################
# Table 3
###################################

