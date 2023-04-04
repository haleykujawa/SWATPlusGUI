# Gil's method for developing climate change scenarios
rm(list=ls())
library(tidyverse)
library(here)

setwd(here::here('UW Climate Data','NORWALK_WWTP'))

clim_files<-list.files()

obs_data<-c()

for (i in clim_files){
  
  obs_add<-read.csv(i)
  obs_data<-rbind(obs_data,obs_add)
  
}

#annual
ClimateSummary_annual<-obs_data %>% 
  mutate(DATE = ymd(DATE)) %>% 
  group_by(year(DATE)) %>%
  mutate(TAVG=(TMIN+TMAX)/2) %>% 
  summarize(PCP_mm=sum(PRCP,na.rm=T),TMP_C=mean(TAVG,na.rm=T)) %>% 
  mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(-TMP_C))

#monthly
ClimateSummary_monthly<-obs_data %>% 
  mutate(DATE = ymd(DATE)) %>% 
  group_by(year=year(DATE),month=month(DATE)) %>%
  mutate(TAVG=(TMIN+TMAX)/2) %>% 
  summarize(PCP_mm=sum(PRCP,na.rm=T),TMP_C=mean(TAVG,na.rm=T)) %>% 
  ungroup() %>% #This allows them to be ranked overall rather than with the group of year and month, should potentially just do rankings for group
  mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(-TMP_C)) %>% 
  mutate(season=NA) %>% 
  mutate(season = replace(season, c(month == 12 |month == 1 | month ==2), 'winter')) %>% 
  mutate(season = replace(season, c(month == 3 |month == 4 | month ==5), 'spring')) %>% 
  mutate(season = replace(season, c(month == 6 |month == 7 | month ==8), 'summer')) %>% 
  mutate(season = replace(season, c(month == 9 |month == 10 | month ==11), 'fall')) 

#seasonal
ClimateSummary_seasonal<-ClimateSummary_monthly %>% 
  group_by(season,year) %>% 
  summarize(PCP_mm=sum(PCP_mm,na.rm=T),TMP_C=mean(TMP_C,na.rm=T))  %>% 
  mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(-TMP_C))