install.packages(c("tidyr", "dplyr","reshape2","tidyverse","giscoR","tmap","highcharter","plotly"), repos="https://cloud.r-project.org")
install.packages(c("ggplot2","ggforce","kableExtra","bookdown","rmarkdown"),repos="https://cloud.r-project.org")

# Eurostat package restatapi: we get the dev version
remotes::install_github("eurostat/restatapi")
