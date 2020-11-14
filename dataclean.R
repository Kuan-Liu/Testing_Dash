library(dplyr) #data tool;
library(RCurl) #read data url;
# setwd("C:/DB Mount/GitHub/Testing_Dash")

#------------------ Read Canada data ------------------
# #read raw github data from the working group github;
x1 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_canada/testing_timeseries_canada.csv") #new time series data, apr13;
x2 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/testing_timeseries_prov.csv") #new time series data, apr13;
x3 <- getURI("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
# x3 <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")
x4 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_canada/cases_timeseries_canada.csv")
x5 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_canada/active_timeseries_canada.csv")
x6 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_canada/mortality_timeseries_canada.csv")
x7 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv")
x8 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/active_timeseries_prov.csv")
x9 <- getURL("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/mortality_timeseries_prov.csv")


can_t <- read.csv(text=x1, header = TRUE, sep = ",", encoding = 'UTF-8')
prov_t <- read.csv(text=x2, header = TRUE, sep = ",", encoding = 'UTF-8')
world_t <- read.csv(text=x3, header = TRUE, sep = ",", encoding = 'UTF-8')
can_c <- read.csv(text=x4, header = TRUE, sep = ",", encoding = 'UTF-8')
can_a <- read.csv(text=x5, header = TRUE, sep = ",", encoding = 'UTF-8')
can_d <- read.csv(text=x6, header = TRUE, sep = ",", encoding = 'UTF-8')
prov_c <- read.csv(text=x7, header = TRUE, sep = ",", encoding = 'UTF-8')
prov_a <- read.csv(text=x8, header = TRUE, sep = ",", encoding = 'UTF-8')
prov_d <- read.csv(text=x9, header = TRUE, sep = ",", encoding = 'UTF-8')


`%>%` <- magrittr::`%>%`
#------------------ canada data formating ------------------
#format dates;

can_t$date_testing<-as.Date(can_t$date_testing,format="%d-%m-%y")
prov_t$date_testing<-as.Date(prov_t$date_testing,format="%d-%m-%y")
world_t$Date<-as.Date(world_t$date,format="%d-%m-%y")

can_c$date_report<-as.Date(can_c$date_report,format="%d-%m-%y")
can_a$date_active<-as.Date(can_a$date_active,format="%d-%m-%y")
can_d$date_death_report<-as.Date(can_d$date_death_report,format="%d-%m-%y")

prov_c$date_report<-as.Date(prov_c$date_report,format="%d-%m-%y")
prov_a$date_active<-as.Date(prov_a$date_active,format="%d-%m-%y")
prov_d$date_death_report<-as.Date(prov_d$date_death_report,format="%d-%m-%y")


#merge all data by dates in a canada dataset;
#this data can be shared on the dashboard; #we can also share the canada data by province;
can<-merge(can_c[,c("date_report", "cases")], can_a, by.x = "date_report",by.y="date_active",all.x = T)
can<-merge(can, can_d[,c("date_death_report", "deaths")], by.x = "date_report",by.y="date_death_report",all.x = T)
can<-merge(can, can_t[,c("date_testing" ,"testing" ,"cumulative_testing","testing_info")], by.x = "date_report", by.y="date_testing",all.x = T)
# can[is.na(can)]<-0  #give value zero for missing;

can$population<-37589262
can$cumulative_testing_per1000<-round(( can$cumulative_testing/ can$population)*1000,3)
can$testing_per1000<-round(( can$testing/ can$population)*1000,3)

write.csv(can, "docs/can.csv",row.names = F)



#------------Province data formatting ------------



prov<-merge(prov_c[,c("province","date_report","cases")], prov_a, by.x = c("province","date_report"), by.y=c("province","date_active"),all.x = T)
prov<-merge(prov, prov_d[,c("province","date_death_report", "deaths")], by.x =c("province","date_report"),by.y=c("province","date_death_report"),all.x = T)
prov<-merge(prov, prov_t[,c("province","date_testing" ,"testing" ,"cumulative_testing","testing_info")], by.x = c("province","date_report"), by.y=c("province","date_testing"),all.x = T)
# prov[is.na(prov)]<-0  #give value zero for missing;


#format province labels;
prov$province[prov$province=="BC"]<-"British Columbia"
prov$province[prov$province=="NL"]<-"Newfoundland and Labrador"
prov$province[prov$province=="NWT"]<-"NorthWest"
prov$province[prov$province=="PEI"]<-"Prince Edward Island"

#add population size;
prov$population[prov$province=="Alberta"]<-4371316
prov$population[prov$province=="British Columbia"]<-5071336
prov$population[prov$province=="Manitoba"]<-1369465
prov$population[prov$province=="New Brunswick"]<-776827
prov$population[prov$province=="Newfoundland and Labrador"]<-521542
prov$population[prov$province=="Nova Scotia"]<-971395
prov$population[prov$province=="Nunavut"]<-38780
prov$population[prov$province=="NorthWest"]<-44826
prov$population[prov$province=="Ontario"]<-14566547
prov$population[prov$province=="Prince Edward Island"]<-156947
prov$population[prov$province=="Quebec"]<-8484965
prov$population[prov$province=="Saskatchewan"]<-1174462
prov$population[prov$province=="Yukon"]<-40854

prov$cumulative_testing_per1000<-round((prov$cumulative_testing/prov$population)*1000,3)
prov$testing_per1000<-round((prov$testing/prov$population)*1000,3)

write.csv(prov, "docs/prov.csv",row.names = F)

#------------ world data formatting ------------

# world<-read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv", header = T,
#                 sep = ",", encoding = 'UTF-8')
# world_clean<- world %>% filter(country %in% c("US","United Kingdom","Australia","Germany","Spain","Italy","Switzerland"), 
#                                type == "confirmed") 
# world_clean <- world_clean %>% select(country, date, cases)
# write.csv(world_clean, "docs/data/world_clean.csv", row.names = F)


world_t_clean<- world_t %>% filter(location %in% c("Canada","United States","United Kingdom",
                                                   "Italy","South Korea", "Australia")) 

world_clean <- world_t_clean[,1:27]

write.csv(world_clean, "docs/world_clean.csv", row.names = F)




#------------trajectory data testing since first 100 death ------------
world_clean$total_tests_per_thousand<- (world_clean$total_tests/(world_clean$total_cases/world_clean$total_cases_per_million))/1000

df_us <- world_clean %>% dplyr::filter( location == "United States") %>%
  dplyr::arrange(date) %>%
  dplyr::filter(total_deaths > 100)  %>%
  dplyr::select( us1=total_tests, us2=total_tests_per_thousand)

df_us$index <- 1:nrow(df_us)
df_us<-df_us[complete.cases(df_us),]

df_uk <- world_clean %>% dplyr::filter( location == "United Kingdom") %>%
  dplyr::arrange(date) %>%
  dplyr::filter(total_deaths > 100)  %>%
  dplyr::select( uk1=total_tests, uk2=total_tests_per_thousand)

df_uk$index <- 1:nrow(df_uk)
df_uk<-df_uk[complete.cases(df_uk),]

df_au <- world_clean %>% dplyr::filter( location == "Australia") %>%
  dplyr::arrange(date) %>%
  dplyr::filter(total_deaths > 100)  %>%
  dplyr::select( au1=total_tests, au2=total_tests_per_thousand)

df_au$index <- 1:nrow(df_au)
df_au<-df_au[complete.cases(df_au),]


df_it <- world_clean %>% dplyr::filter( location == "Italy") %>%
  dplyr::arrange(date) %>%
  dplyr::filter(total_deaths > 100)  %>%
  dplyr::select( it1=total_tests, it2=total_tests_per_thousand)

df_it$index <- 1:nrow(df_it)
df_it<-df_it[complete.cases(df_it),]

df_sko <- world_clean %>% dplyr::filter( location == "South Korea") %>%
  dplyr::arrange(date) %>%
  dplyr::filter(total_deaths > 100)  %>%
  dplyr::select(sko1=total_tests, sko2=total_tests_per_thousand)

df_sko$index <- 1:nrow(df_sko)
df_sko<-df_sko[complete.cases(df_sko),]


# df_fr <- world_clean %>% dplyr::filter( location == "France") %>%
#   dplyr::arrange(date) %>%
#   dplyr::filter(total_deaths > 100)  %>%
#   dplyr::select( fr1=total_tests, fr2=total_tests_per_thousand)
# 
# df_fr$index <- 1:nrow(df_fr)
# df_fr<-df_fr[complete.cases(df_fr),]


df_can <- world_clean %>% dplyr::filter( location == "Canada") %>%
  dplyr::arrange(date) %>%
  dplyr::filter(total_deaths > 100)  %>%
  dplyr::select(can1=total_tests,can2=total_tests_per_thousand)

df_can$index <- 1:nrow(df_can)
df_can<-df_can[complete.cases(df_can),]


df_trajectory <- df_us %>% 
  dplyr::left_join(df_can, by = "index") %>%
  dplyr::left_join(df_uk, by = "index") %>%
  dplyr::left_join(df_it, by = "index") %>%
  dplyr::left_join(df_sko, by = "index") %>%
  dplyr::left_join(df_au, by = "index") 

write.csv(df_trajectory, "docs/df_trajectory.csv",row.names = F)

#------------provincial data this is for the province trajectory plot------------
#now getting data for provinces, only those with more than 50 deaths;

#first let's add population size to the province data;


can_cum<- dplyr::filter(can, cumulative_deaths > 50)  %>% dplyr::select(cumulative_testing, cumulative_testing_per1000)
can_cum$index <- 1:nrow(can_cum)  
names(can_cum)[1:2]<-c("can1","can2")

ab_cum<- dplyr::filter(prov, cumulative_deaths > 50 & province=="Alberta")  %>% dplyr::select(cumulative_testing, cumulative_testing_per1000)
ab_cum$index <- 1:nrow(ab_cum)  
names(ab_cum)[1:2]<-c("ab1","ab2")

bc_cum<- dplyr::filter(prov, cumulative_deaths > 50 & province=="British Columbia") %>% dplyr::select(cumulative_testing, cumulative_testing_per1000)
bc_cum$index <- 1:nrow(bc_cum)  
names(bc_cum)[1:2]<-c("bc1", "bc2")

on_cum<- dplyr::filter(prov, cumulative_deaths > 50 & province=="Ontario")  %>% dplyr::select(cumulative_testing, cumulative_testing_per1000)
on_cum$index <- 1:nrow(on_cum)  
names(on_cum)[1:2]<-c("on1", "on2")

qc_cum<- dplyr::filter(prov, cumulative_deaths > 50 & province=="Quebec")  %>% dplyr::select(cumulative_testing, cumulative_testing_per1000)
qc_cum$index <- 1:nrow(qc_cum)  
names(qc_cum)[1:2]<-c("qc1","qc2")

ns_cum<- dplyr::filter(prov, cumulative_deaths > 50 & province=="Nova Scotia")  %>% dplyr::select(cumulative_testing, cumulative_testing_per1000)
ns_cum$index <- 1:nrow(ns_cum)  
names(ns_cum)[1:2]<-c("ns1","ns2")


df_trajectory_can <- can_cum %>% 
  dplyr::left_join(on_cum, by = "index") %>%
  dplyr::left_join(qc_cum, by = "index") %>%
  dplyr::left_join(ab_cum, by = "index") %>%
  dplyr::left_join(bc_cum, by = "index") %>%
  dplyr::left_join(ns_cum, by = "index") 


write.csv(df_trajectory_can, "docs/df_trajectory_can.csv",row.names = F)


