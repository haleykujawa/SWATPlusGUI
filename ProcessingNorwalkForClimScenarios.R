# Gil's method for developing climate change scenarios
rm(list=ls())
library(tidyverse)
library(here)
library(magrittr)

setwd(here::here('UW Climate Data','NORWALK_WWTP'))

clim_files<-c('3282969.csv','3282971.csv')

obs_data<-c()

for (i in clim_files){
  
  obs_add<-read.csv(i)
  obs_data<-rbind(obs_data,obs_add)
  
}
# Precip rank #1 = wettest / flood
# Temp rank #1 = coldest
# sum of the precip + temp rank, high= drought year, low = flood, extreme wet/cold

#annual
ClimateSummary_annual<-obs_data %>% 
  mutate(DATE = ymd(DATE)) %>% 
  group_by(YEAR=year(DATE)) %>%
  mutate(TAVG=(TMIN+TMAX)/2) %>% 
  summarize(PCP_mm=sum(PRCP,na.rm=T),TMP_C=mean(TAVG,na.rm=T)) %>% 
  mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(TMP_C),Overall_rank=(TMP_rank+PCP_rank))


#monthly
ClimateSummary_monthly<-obs_data %>% 
  mutate(DATE = ymd(DATE)) %>% 
  group_by(YEAR=year(DATE),MONTH=month(DATE)) %>%
  mutate(TAVG=(TMIN+TMAX)/2) %>% 
  summarize(PCP_mm=sum(PRCP,na.rm=T),TMP_C=mean(TAVG,na.rm=T)) %>% 
  ungroup() %>% #This allows them to be ranked overall rather than with the group of year and month, should potentially just do rankings for group
  mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(TMP_C)) %>% 
  mutate(season=NA) %>% 
  mutate(season = replace(season, c(MONTH == 12 |MONTH == 1 | MONTH ==2), 'winter')) %>% 
  mutate(season = replace(season, c(MONTH == 3 |MONTH == 4 | MONTH ==5), 'spring')) %>% 
  mutate(season = replace(season, c(MONTH == 6 |MONTH == 7 | MONTH ==8), 'summer')) %>% 
  mutate(season = replace(season, c(MONTH == 9 |MONTH == 10 | MONTH ==11), 'fall'))  

#seasonal
ClimateSummary_seasonal<-ClimateSummary_monthly %>% 
  group_by(season,YEAR) %>% 
  summarize(PCP_mm=sum(PCP_mm,na.rm=T),TMP_C=mean(TMP_C,na.rm=T))  %>% 
  mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(TMP_C))

# OWC Discharge 
# read channel output
# Manually deleted missing data circa 1994 out of file
tmp <- file('Discharge_OWC.txt')
open(tmp, "r") #read
readLines(tmp, n = 31)   #read past headerlines

DisOWC<-readLines(tmp,n=-1)
DisOWC<-data.frame(do.call(rbind, (strsplit(DisOWC,"\t"))))

DisOWC <- DisOWC %>% 
  set_colnames(c('1','2','DATE','DIS_CMS','flag')) %>% # in the magrittr package
  filter(flag=="A"| flag=='A:e') %>% 
  select('DATE','DIS_CMS') %>% 
  mutate(DIS_CMS=as.numeric(DIS_CMS)*0.0283)

Dis_annual <-  DisOWC %>% 
  group_by(YEAR=year(DATE)) %>%
  filter(YEAR <= 1999, YEAR != 1994, YEAR != 1995) %>% # Maybe should remove 1994 and 1995 bc missing a few months of data 
  summarize(DIS_CMS=mean(DIS_CMS,na.rm=T))

Dis_monthly <- DisOWC %>%
  group_by(YEAR=year(DATE),MONTH=month(DATE)) %>%
  filter(YEAR <= 1999, !(YEAR == 1995 & MONTH > 9), !(YEAR == 1994 & MONTH <= 9) ) %>% 
  summarize(DIS_CMS=mean(DIS_CMS,na.rm=T)) %>% 
  ungroup() %>% #This allows them to be ranked overall rather than with the group of year and month, should potentially just do rankings for group
  mutate(season=NA) %>% 
  mutate(season = replace(season, c(MONTH == 12 |MONTH == 1 | MONTH ==2), 'winter')) %>% 
  mutate(season = replace(season, c(MONTH == 3 |MONTH == 4 | MONTH ==5), 'spring')) %>% 
  mutate(season = replace(season, c(MONTH == 6 |MONTH == 7 | MONTH ==8), 'summer')) %>% 
  mutate(season = replace(season, c(MONTH == 9 |MONTH == 10 | MONTH ==11), 'fall')) 
  
Dis_seasonal <- Dis_monthly %>% 
  group_by(season,YEAR) %>% 
  summarize(DIS_CMS=mean(DIS_CMS,na.rm=T))
  
AnnualData<-full_join(ClimateSummary_annual,Dis_annual,by='YEAR')
MonthlyData<-full_join(ClimateSummary_monthly,Dis_monthly,by=c('YEAR','MONTH','season'))
SeasonalData<-full_join(ClimateSummary_seasonal,Dis_seasonal,by=c('season','YEAR'))

#Annual plots
pcpvdis<-AnnualData %>% 
  ggplot(.,aes(x=PCP_mm,y=DIS_CMS))+geom_point()+ stat_smooth(method="lm",formula=y ~ x,se=F)+
  stat_poly_eq(formula=y~x,aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*"))) +
  theme(panel.grid.major =   element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),panel.border=element_rect(fill='transparent'))

tmpvdis<-AnnualData %>% 
  ggplot(.,aes(x=TMP_C,y=DIS_CMS))+geom_point()+ stat_smooth(method="lm",formula=y ~ x,se=F)+
  stat_poly_eq(formula=y~x,aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*"))) +
  theme(panel.grid.major =   element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),panel.border=element_rect(fill='transparent'))

AnnualComp<-ggarrange(pcpvdis,tmpvdis)
ggsave("ClimvsDis_annual.png",AnnualComp,height=200,width=400,units='mm')

AnnualData<-reshape2::melt(AnnualData,id='YEAR')

AnnualData %>% 
  filter(variable == 'DIS_CMS' | variable == 'PCP_mm' | variable == 'TMP_C') %>% 
ggplot(.,aes(x=YEAR,y=value))+geom_line()+facet_wrap(vars(variable),nrow=3,ncol=1,scales="free_y")+
  theme(panel.grid.major =   element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),panel.border=element_rect(fill='transparent'))

# Seasonal plots
pcpvdis<-SeasonalData %>% 
  ggplot(.,aes(x=PCP_mm,y=DIS_CMS))+geom_point()+ facet_wrap(vars(season))+stat_smooth(method="lm",formula=y ~ x,se=F)+
  stat_poly_eq(formula=y~x,aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*"))) +
  theme(panel.grid.major =   element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),panel.border=element_rect(fill='transparent'))

tmpvdis<-SeasonalData %>% 
  ggplot(.,aes(x=TMP_C,y=DIS_CMS))+geom_point()+facet_wrap(vars(season))+ stat_smooth(method="lm",formula=y ~ x,se=F)+
  stat_poly_eq(formula=y~x,aes(label = paste(after_stat(eq.label),after_stat(rr.label), sep = "*\", \"*"))) +
  theme(panel.grid.major =   element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),panel.border=element_rect(fill='transparent'))

AnnualComp<-ggarrange(pcpvdis,tmpvdis)
ggsave("ClimvsDis_seasonal.png",AnnualComp,height=200,width=400,units='mm')  
  