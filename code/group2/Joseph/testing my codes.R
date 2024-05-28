########### ============= packages =================
require(conflicted)
require(restatapi)
require(giscoR)
require(data.table)
require(chron)
require(reshape2)
require(kableExtra)
require(tidyverse)
require(tmap)
require(highcharter)
require(plotly)

########### ============= codes =================

id2<-"ilc_mddw02"
date2 = 2020

eu_ctry_names<-do.call(rbind,
                       lapply(get("cc",envir=.restatapi_env)$EU27_2020,
                              search_eurostat_dsd,
                              dsd=get_eurostat_dsd(id2),
                              exact_match=TRUE))

eu_cc<-get("cc",envir=.restatapi_env)$EU27_2020

efta_cc<-c("CH","NO","IS","LI")

data2 = get_eurostat_data(
  id2, 
  filters = list(
    incgrp = c("TOTAL","B_MD60"),
    freq = "Annual",
    hhtyp = "TOTAL",
    unit = "Percentage"
  ),date_filter = eval(date2),
  label=T,
  ignore.case=T,
  exact_match=F,
  perl=T,
  stringsAsFactors=F,
  force_local_filter=T)

dsd<-as.data.table(get_eurostat_dsd(id2))[concept=="geo",c("code","name")]

data3<-merge(data2, dsd, 
             by.x="geo",by.y="name", 
             all.x=T, all.y = F)

data3 <- data3[!code %in% c("EA19", "EA18", "EA")]

data3$geo<-gsub(" \\(.*\\)","",data3$geo)




data3$geo<-gsub("^Eu.*","EU",data3$geo)


name_ord <- data3[(code %in% eu_cc) & (incgrp == "Total")]

name_ord_eu<-name_ord[order(values)]$geo

name_ord<-data3[(code %in% efta_cc) & (incgrp == "Total")]

name_ord_efta<-name_ord[order(values)]$geo

name_ord<-data3[!(code %in% c(efta_cc,eu_cc,"EU27_2020")) & (incgrp == "Total")]

name_ord_othr<-name_ord[order(values)]$geo

name_ord<-c(name_ord_othr, " " ,name_ord_efta, "  " ,name_ord_eu, "   ", "EU")

data3 <- data3 |>
  select(geo, incgrp, values) |>
  mutate(incgrp = recode(incgrp,
                         "Below 60% of median equivalised income" = "Population at-risk-of-poverty (\U00B2)")) |>
  (\(df){
    dt_sep <- data.table::data.table(
      geo = c(" ", "  ", "   "),
      incgrp = rep("Total", 3),
      values = rep(NA, 3)
    )
    rbind(df, dt_sep)
  })()

data3$geo <- factor(data3$geo,levels = rev(name_ord))
data3$incgrp <- factor(data3$incgrp, levels = c("Total", "Population at-risk-of-poverty (\U00B2)"))

data3 <- data3 |>
  mutate(geo = recode(geo,
                      "France" = "France (\U00B3)",
                      "Luxembourg" = "Luxembourg (\U00B3)",
                      "Germany" = "Germany (\U00B3)(\U2075)",
                      "Ireland" = "Ireland (\U00B3)"))


ggplot(data = data3, aes(x = geo, y = values, fill = incgrp))+
  geom_bar(stat = "identity", position = "dodge", width = 0.5)+
  scale_fill_manual(values = c("orange","blue"))+ 
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(data2$values)+5),
                     breaks = seq(0, max(data2$values)+5, 5))+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "black",linetype = "solid"),
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(hjust = 0, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  labs(
    title = paste0("Population reporting exposure to pollution, ",
                   "grime or other environmental\nproblems, ",
                   "by income situation, ",date2," (% share)"),
    subtitle = "(%)",
    caption = paste0("\n(\U00B9) Estimate.",
                     "\n(\U00B2) People living below the national poverty ",
                     "threshold (60% of median equivalised income).",
                     "\n(\U00B3) Break in series.",
                     "\n(\U2074) 2019 instead of 2020.",
                     "\n(\U2075) Low reliability.",
                     "\n(\U2076) 2018 instead of 2020.",
                     "\nSource: Eurostat (online data code: ",id2,")"))


