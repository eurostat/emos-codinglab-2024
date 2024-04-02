library(ggplot2)
library(restatapi)
library(data.table)
library(reshape2)
library(plotly)
library(kableExtra)
library(tmap)
library(chron)
library(giscoR)
library(highcharter)

# https://github.com/eurostat/statistics-coded/blob/master/popul/population/population-structure-ageing_r.ipynb

ageclass <- c("Y_LT5", "Y5-9", "Y10-14", "Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39", "Y40-44",
              "Y45-49", "Y50-54", "Y55-59", "Y60-64", "Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y_GE85")
dataset1 <- get_eurostat_data(id="demo_pjangroup", filters=list(geo="EU27_2020", age=ageclass, sex=c("M","F")), 
                              date_filter=c(2004,2019))
levels(dataset1$age) <- list("<5"="Y_LT5", "5-9"= "Y5-9", "10-14"= "Y10-14", 
                             "15-19"="Y15-19", "20-24"="Y20-24", "25-29"="Y25-29", 
                             "30-34"="Y30-34", "35-39"="Y35-39", "40-44"= "Y40-44",
                             "45-49"= "Y45-49", "50-54"= "Y50-54", "55-59"= "Y55-59", 
                             "60-64"="Y60-64", "65-69"="Y65-69", "70-74"="Y70-74", 
                             "75-79"="Y75-79", "80-84"="Y80-84", "85+" ="Y_GE85")
tot2019 <- sum(dataset1[time=="2019"]$values)
tot2004 <- sum(dataset1[time=="2004"]$values)
options(repr.plot.width=5, repr.plot.height=5)
plot1 <- ggplot(dataset1, aes(x=age , y=values, fill= sex,)) + 
  
  geom_bar(data=subset(dataset1, sex == "F" & time=="2019" ),aes(y=(values)/tot2019*100), fill="orange", stat = "identity") + 
  geom_bar(data=subset(dataset1,sex == "M" &time=="2019"),aes(y=(-values)/tot2019*100), fill="steelblue", stat = "identity") +
  geom_bar(data=subset(dataset1, sex == "F" & time=="2004" ),aes(y=(values)/tot2004*100),fill="transparent", colour="#FFCC00", size=1, stat = "identity") + 
  geom_bar(data=subset(dataset1,sex == "M" &time=="2004"),aes(y=(-values)/tot2004*100), fill="transparent", colour="lightblue", size=1, stat = "identity") +
  scale_x_discrete(limits= c("<5","5-9", "10-14", "15-19","20-24","25-29","30-34", "35-39", "40-44","45-49","50-54","55-59","60-64","65-69","70-74", "75-79","80-84", "85+")) +
  coord_flip()+
  labs(title = "Population pyramids, EU-27, 2004 and 2019",
       subtitle = "(% of the total population)",
       caption = "Solid colour: 2019 Bordered: 2004") +
  theme_bw()

print(plot1)



countries <- as.factor(c("EU27_2020","BE", "BG","CZ","DK", "DE", "EE","IE","EL","ES","FR",
                         "HR", "IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO",
                         "SI","SK","FI","SE","UK","IS","LI","NO","CH","ME", "MK", "AL", "RS","TR"))
dataset <- get_eurostat_data(id="demo_pjanind", filters=list(geo=countries, indic_de=c("PC_Y0_14","PC_Y15_24","PC_Y25_49","PC_Y50_64","PC_Y65_MAX" ,"MEDAGEPOP", "YOUNGDEP1","OLDDEP1","DEPRATIO1","PC_Y80_MAX")), date_filter=c(2009,2019))
x <- c("PC_Y0_14","PC_Y15_24","PC_Y25_49","PC_Y50_64","PC_Y65_MAX")
dataset5 <- subset(dataset, dataset$indic_de %in% x)
times <- c(2009,2019)
for(i in 1:length(times)){
  year <- times[i]
  dataset5[dataset5$time==year & dataset5$indic_de=="PC_Y15_24", 4] <- dataset5[dataset5$time==year & dataset5$indic_de=="PC_Y15_24", 4]+
    dataset5[dataset5$time==year & dataset5$indic_de=="PC_Y25_49", 4]+
    dataset5[dataset5$time==year & dataset5$indic_de=="PC_Y50_64", 4]
}
levels(dataset5$indic_de)[levels(dataset5$indic_de)=="PC_Y15_24"] <- "PC_Y15_64"
dataset5 <- dataset5[dataset5$indic_de!="PC_Y25_49" & dataset5$indic_de!="PC_Y50_64",]
age0_14_2009 <- dataset5[dataset5$indic_de=="PC_Y0_14" & dataset5$time=="2009", -c(2,3)]
age0_14_2019 <- dataset5[dataset5$indic_de=="PC_Y0_14" & dataset5$time=="2019", -c(2,3)]
age0_14_2009 <- age0_14_2009[order(factor(age0_14_2009$geo, levels=unique(countries))),-1]
age0_14_2019 <- age0_14_2019[order(factor(age0_14_2019$geo, levels=unique(countries))),-1]
age15_64_2009 <- dataset5[dataset5$indic_de=="PC_Y15_64" & dataset5$time=="2009", -c(2,3)]
age15_64_2019 <- dataset5[dataset5$indic_de=="PC_Y15_64" & dataset5$time=="2019", -c(2,3)]
age15_64_2009 <- age15_64_2009[order(factor(age15_64_2009$geo, levels=unique(countries))),-1]
age15_64_2019 <- age15_64_2019[order(factor(age15_64_2019$geo, levels=unique(countries))),-1]
age65_MAX_2009 <- dataset5[dataset5$indic_de=="PC_Y65_MAX" & dataset5$time=="2009", -c(2,3)]
age65_MAX_2019 <- dataset5[dataset5$indic_de=="PC_Y65_MAX" & dataset5$time=="2019", -c(2,3)]
age65_MAX_2009 <- age65_MAX_2009[order(factor(age65_MAX_2009$geo, levels=unique(countries))),-1]
age65_MAX_2019 <- age65_MAX_2019[order(factor(age65_MAX_2019$geo, levels=unique(countries))),-1]
cntr <- as.factor(c("EU-27", "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
                    "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania",
                    "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria", "Poland","Portugal", "Romania",
                    "Slovenia", "Slovakia", "Finland", "Sweden","United Kingdom", "Iceland", "Liechtenstein",
                    "Norway", "Switzerland", "Montenegro", "North Macedonia", "Albania", "Serbia", "Turkey"))
dt <- cbind(cntr, age0_14_2009, age0_14_2019, age15_64_2009,age15_64_2019, age65_MAX_2009,age65_MAX_2019 )
colnames(dt) <- c("Countries ", "2009", "2019","2009","2019","2009","2019")
dt %>%
  kable("html")%>%
  kable_styling(c("striped", "bordered")) %>%
  add_header_above(c("-" = 1, "0-14 years old" = 2, "15-64 years old" = 2, "65 years old or over" = 2), background='bisque') %>%
  row_spec(0, background = 'bisque') %>%  
  row_spec(1, background='orange',bold=TRUE) %>%
  column_spec(1, bold=TRUE) %>%
  group_rows(" ", 29,30) %>%
  group_rows(" ", 30,34) %>%
  group_rows(" ", 34,38)
  
# https://github.com/eurostat/statistics-coded/blob/master/popul/population/imigration-legislation-enforcement_r.ipynb


options(timeout=300)
app_tot<- get_eurostat_data("migr_eipre")
map <- gisco_get_nuts(
  resolution = "60",
  nuts_level = "0",
  year = "2016"
)
app_map <- app_tot[time == "2020" & !(citizen %in% c("TOTAL","EUR_C_E_OTH","AME","AFR","ASI","EUR","OCE")) & age == "TOTAL" & sex == "T", sum(values, na.rm = T),geo]
setnames(app_map,"V1","Apprehended")
options(repr.plot.width=9, repr.plot.height=9,repr.plot.res=400)
map <- dplyr::left_join(map, app_map, by=c("NUTS_ID"="geo"))%>%
  tm_shape() +
  tm_fill("Apprehended",
          popup.vars = c("Apprehended","NUTS_ID", "NUTS_NAME"),
          palette = "Oranges", 
          breaks = c(0,1000, 5000, 25000, 50000, Inf),
          title = "Persons found to be illegally present"
  ) +
  tm_borders() +
  tm_layout(bg.color = "lightblue")
map


ref_tot <- get_eurostat_data("migr_eirfs")
app_tot <- get_eurostat_data("migr_eipre")
ord_tot <- get_eurostat_data("migr_eiord")
ret_tot <- get_eurostat_data("migr_eirtn")

refused <- ref_tot[geo == "EU27_2020" & reason == "TOTAL" & border == "TOTAL" & citizen == "TOTAL", .(time,values)]
setnames(refused, "values","refused")

apprehended <- app_tot[geo == "EU27_2020" & citizen == "TOTAL" & age == "TOTAL" & sex == "T" & unit == "PER" & apprehen == "TOTAL", .(time,values)]
setnames(apprehended, "values","apprehended")

ordered <- ord_tot[geo == "EU27_2020" & sex == "T" & citizen == "TOTAL" & age == "TOTAL", .(time, values)]
setnames(ordered, "values","ordered")

returned <- ret_tot[geo == "EU27_2020" & sex == "T" & citizen =="TOTAL" & age == "TOTAL" & c_dest == "TOTAL", .(time,values)]
setnames(returned, "values","returned")


dt <- dplyr::left_join(refused, apprehended)%>%
  dplyr::left_join(ordered)%>%
  dplyr::left_join(returned)

plot_ly(dt, x = ~time,y= ~refused,
        name = "Refused",type = "scatter",mode = 'lines+markers') %>%
  add_trace(y = ~apprehended, name = 'Apprehended', mode = 'lines+markers') %>%
  add_trace(y = ~ordered, name = 'Ordered to leave', mode = 'lines+markers') %>%
  add_trace(y = ~returned, name = 'Returned', mode = 'lines+markers') %>%
  layout(paper_bgcolor='gray20',
         plot_bgcolor='gray87',
         xaxis = list(color = 'black',tickangle = 0),
         yaxis = list(color = 'black', title = "Persons",exponentformat="none",separatethoudands = TRUE)
  ) %>%
  layout(hovermode = "x unified")




# https://github.com/eurostat/statistics-coded/blob/master/popul/labour-market/hr-science-technology_r.ipynb

year<-2020
age_group1<-'25 to 64' 
age_group2<-'15 to 74'
eu_cc<-get("cc",envir=.restatapi_env)$EU27_2020
efta_cc<-c("CH","NO","IS","LI")
#get_eurostat_dsd("hrst_st_nocc")
dt_sep<-data.table::data.table(geo=c(" ","  ","   "),group=rep("Scientists and Engineers",3),values=rep(NA,3),pct=rep(NA,3),name=c(" ","  ","   "))
dt_fig1<-get_eurostat_data("hrst_st_nocc",date_filter=year,filters=c("THS","HRSTO","^Scient","OC",gsub(" to ","-",age_group1)),exact_match=F,label=F)[geo!="EA19",c("category","isco08","geo","values")]
dt_fig1<-dt_fig1[grepl("^SE",category),isco08:=category]
dt_fig1<-as.data.table(dcast(dt_fig1,geo ~ isco08,value.var="values"))[,OC2_SE:=OC2-SE]
setnames(dt_fig1,c("geo","Professionals","Technicians and associate professionals","Scientists and Engineers","Other professionals (other than SE)"))
dt_fig1<-as.data.table(melt(dt_fig1[,c(1,3:5)],id.vars="geo"))
setnames(dt_fig1,c("geo","group","values"))
dt_fig1[,pct:=values/sum(values)*100,by=geo]
dsd<-as.data.table(get_eurostat_dsd("hrst_st_nocc"))[concept=="geo",c("code","name")]
dt_fig1<-merge(dt_fig1,dsd,by.x="geo",by.y="code",all.x=T)
dt_fig1$name<-gsub(" \\(.*\\)","",dt_fig1$name)
dt_fig1<-dt_fig1[geo!="EA20"]
dt_fig1$name<-gsub("^Eu.*","EU",dt_fig1$name)
name_ord<-dt_fig1[(geo %in% eu_cc)&grepl("^Scien",group)]
name_ord_eu<-name_ord[order(pct)]$name
name_ord<-dt_fig1[(geo %in% efta_cc)&grepl("^Scien",group)]
name_ord_efta<-name_ord[order(pct)]$name
name_ord<-dt_fig1[!(geo %in% c(efta_cc,eu_cc,"EU27_2020"))&grepl("^Scien",group)]
name_ord_othr<-name_ord[order(pct)]$name
name_ord<-c(name_ord_othr,' ',name_ord_efta,'  ',name_ord_eu,'   ','EU')
dt_fig1<-rbind(dt_fig1,dt_sep)
dt_fig1$name<-factor(dt_fig1$name,levels=name_ord)
gr_ord<-c("Technicians and associate professionals","Other professionals (other than SE)","Scientists and Engineers")
dt_fig1$group<-factor(dt_fig1$group,levels=gr_ord)
options(repr.plot.width=8, repr.plot.height=6,repr.plot.res=300)
fig1_colors<-c("#FAA519","#286EB4","#F06423") #,"#F6A26B","#71A8DF",,"#FDDBA3","#FCC975"
ggplot(dt_fig1, aes(x=name,y=pct,fill=group)) + theme_minimal() +
  geom_bar(stat='identity',width=0.5)+
  scale_fill_manual(values = fig1_colors)+
  scale_y_continuous(limits=c(0,101),breaks=seq(0,100,10),label =  function(x) paste0(x, "%"))+
  ggtitle("Figure 1: HRSTO by occupation, age 25-64, 2020 (%)") +
  ylab("")+
  xlab("")+
  coord_flip()+
  theme(legend.title = element_blank(),
        legend.position= "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())+             
  guides(fill=guide_legend(reverse=T))

# https://github.com/eurostat/statistics-coded/blob/master/economy/regional_household_income/statistics_coded_regional_HHI.ipynb


hh2 <- get_eurostat_data("nama_10r_2hhinc")
gdp2 <- get_eurostat_data("nama_10r_2gdp")
pop3 <- get_eurostat_data("nama_10r_3popgdp")
map <- gisco_get_nuts(
  resolution = "60",
  nuts_level = "2",
  year = "2016"
)
temp <- hh2 %>%
  filter(time == 2018 & na_item == "B5N" & unit == "PPS_EU27_2020_HAB") %>%
  select(geo, values) %>%
  rename(b5n = "values") %>%
  mutate(eu_index = round(b5n * 100 / b5n[geo == "EU27_2020"], 1))

sf <- dplyr::left_join(map, temp,by=c("NUTS_ID"="geo"))

options(repr.plot.width=9, repr.plot.height=9,repr.plot.res=400)
# tmap_mode("view")
sf %>%
  filter(CNTR_CODE != "UK") %>%
  tm_shape() +
  tm_fill("eu_index",
          popup.vars = c("eu_index", "NUTS_ID", "NUTS_NAME"),
          palette = "RdPu", # similar to Economy and Finance
          breaks = c(29, 63, 85, 103, 124, 192),
          title = "Net primary income per capita in PPS as % of EU average in 2018 "
  ) +
  tm_borders()


# https://github.com/eurostat/statistics-coded/blob/master/popul/living-conditions/living-conditions-time-use_r.ipynb


yr<-2010
eu_ctry_names<-do.call(rbind,lapply(get("cc",envir=.restatapi_env)$EU28,search_eurostat_dsd,dsd=get_eurostat_dsd("tus_00age"),exact_match=TRUE))$name
dt<-get_eurostat_data("tus_00age",filters=list(unit="spent",age="total",sex="total",acl00=c("sleep","eat","^employ"," (family|personal) care","^leisure","^study","except travel")),date_filter=eval(yr),label=T,ignore.case=T,exact_match=F,perl=T,stringsAsFactors=F,force_local_filter=T)
if (is.factor(dt$values)|is.character(dt$values)) dt<-dt[,values:=chron::times(paste0(values,":00"))]
dt<-dt[,c("acl00","geo","values")]
sdt<-dt[grepl("(ating|ther)",acl00),.(acl00="Eating and other personal care",values=sum(values)),by=geo]
dt<-rbind(dt[!grepl("(ating|ther)",acl00)],sdt)
fig1_colors<-c("#F6A26B","#F06423","#71A8DF","#286EB4","#FDDBA3","#FCC975","#FAA519")
ggplot(dt, aes(x=geo, y=values,fill=acl00)) + 
  geom_bar(position="stack",stat="identity")+
  scale_y_chron(format="%H:%M") +
  scale_fill_manual(values = fig1_colors)+
  ggtitle("Figure 1") +
  ylab ("")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dt_sep<-data.table::data.table(acl00=c("Sleep","Sleep"),geo=c(" ","  "),values=c(chron::times(NA),chron::times(NA)))
dt<-rbind(dt,dt_sep)
acls_ord<-c('Travel except travel related to jobs','Leisure, social and associative life','Household and family care','Study','Employment, related activities and travel as part of/during main and second job','Eating and other personal care','Sleep')
dt$acl00<- factor(dt$acl00,levels=acls_ord)
geo_ord<-c('Belgium','Germany','Estonia','Greece','Spain','France','Italy','Luxembourg','Hungary','Netherlands','Austria','Poland','Romania','Finland','United Kingdom',' ','Norway','  ','Serbia','Turkey')
dt$geo<-factor(dt$geo,levels=geo_ord)

options(repr.plot.width=9, repr.plot.height=6,repr.plot.res=300)
ggplot(dt, aes(x=geo, y=values,fill=acl00)) + theme_minimal() +
  geom_bar(position="stack",stat="identity",width=0.5)+
  scale_y_chron(format="%H:%M",breaks=seq(0,1,4/24)) +
  scale_fill_manual(values = fig1_colors)+
  ggtitle("Figure 1: Mean time spent on daily activities, all individuals by country, (hh:mm; 2008 to 2015)") +
  ylab("")+
  xlab("")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.x = element_blank())




dt<-get_eurostat_data("tus_00age",filters=list(unit="Participation time",age="total",sex="total",acl00=c("^study","^empl")),date_filter=eval(yr),label=T,ignore.case=T,exact_match=F,perl=T,stringsAsFactors=F,force_local_filter=T)
if (is.factor(dt$values)|is.character(dt$values)) dt<-dt[,values:=chron::times(paste0(values,":00"))]
dt<-dt[,c("acl00","geo","values")]  
dt_sep<-data.table::data.table(acl00=c("Study","Study"),geo=c(" ","  "),values=c(chron::times(NA),chron::times(NA)))
dt<-rbind(dt,dt_sep)
geo_ord<-dt[(geo %in% eu_ctry_names)&grepl("Empl",acl00)]
geo_ord<-geo_ord[order(values)]$geo
geo_ord<-c(geo_ord,' ','Norway','  ','Serbia','Turkey')
dt$geo<-factor(dt$geo,levels=geo_ord)
fig2_colors<-c("#FAA519","#286EB4")

options(repr.plot.width=9, repr.plot.height=6,repr.plot.res=300)
ggplot(dt, aes(x=geo, y=values,fill=acl00)) + theme_minimal() +
  geom_bar(position="dodge",stat="identity",width=0.7)+
  scale_y_chron(format="%H:%M",breaks=seq(0,1,1/24)) +
  scale_fill_manual(values = fig2_colors)+
  ggtitle("Figure 2: Participation time per day in study and employment, only individuals taking part in the activity, by country, (hh mm; 2008 to 2015)") +
  ylab("")+
  xlab("")+
  theme(legend.title = element_blank(),
        legend.position= "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
