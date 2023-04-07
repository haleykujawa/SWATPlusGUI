# Gil's method for developing climate change scenarios

# 4/5 May need to add in method for filtering out discharge if too many missing dates, 
# manually excluded a period between 1994 and 1995 where there was a year of missing data
# Also didn't clean norwalk data for flags, may need to go back and do that

# Need to find why using na.rm / na.omit = T returns a value consistently larger than what the value should be? (4 or 1 depending on which time period)
# Just checked this again and it's a non-issue with the water year summaries... 4/7
# Cross checked value with excel, matches the value without using na.rm=T for SUM
# If doing the mean, setting na.rm=F returns NA anytime there is a value of NA..

rm(list=ls())
library(tidyverse)
library(here)
library(magrittr)
library(ggpubr)
library(ggpmisc)

setwd(here::here('UW Climate Data','NORWALK_WWTP'))

clim_files<-c('3282969.csv','3282971.csv','3295661.csv','3295676.csv')
# 2020-2023 file, although the number of columns is different, would need to bind in a different way
# '3295684.csv'

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
  mutate(DATE = ymd(DATE), MONTH=month(ymd(DATE))) %>% 
  mutate(WY=year(DATE)) %>% 
  mutate(WY=ifelse(c(MONTH == 10 |MONTH == 11 | MONTH ==12), WY+1,WY)) %>% # do function only on selected rows
  filter(WY > min(WY) & WY < max(WY)) %>% 
  group_by(WY) %>%
  mutate(TMIN = replace(TMIN,is.na(TMAX),NA)) %>% # Don't calculate average daily temp if min or max is missing
  mutate(TMAX = replace(TMAX,is.na(TMIN),NA)) %>%  
  mutate(TAVG=(TMIN+TMAX)/2) %>% 
  summarize(PCP_mm=sum(PRCP,na.rm=T),TMP_C=mean(TAVG,na.rm=T),
            n_pcp_missing = sum(is.na(PRCP)),n_tmp_missing = sum(is.na(TAVG))) %>% 
  filter(n_pcp_missing < 30 & n_tmp_missing < 30) %>%  #Exclude years with more than a month of missing data %>% 
  mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(TMP_C),Overall_rank=(TMP_rank+PCP_rank)) %>% 
  mutate(PCP_cat= NA, TMP_cat = NA) %>% #catergory for rankings
  mutate(PCP_cat=replace(PCP_cat,PCP_mm >= quantile(PCP_mm,c(0.9)), '90th percentile')) %>% 
  mutate(PCP_cat=replace(PCP_cat,PCP_mm <= quantile(PCP_mm,c(0.1)), '10th percentile')) %>% 
  # mutate(PCP_cat=replace(PCP_cat,PCP_mm <= quantile(PCP_mm,c(0.5)), '50th percentile'))
  mutate(TMP_cat=replace(TMP_cat,TMP_C >= quantile(TMP_C,c(0.9)), '90th percentile')) %>% 
  mutate(TMP_cat=replace(TMP_cat,TMP_C <= quantile(TMP_C,c(0.1)), '10th percentile')) 
  
  




#monthly
ClimateSummary_monthly<-obs_data %>% 
  mutate(DATE = ymd(DATE)) %>% 
  group_by(YEAR=year(DATE),MONTH=month(DATE)) %>%
  mutate(TAVG=(TMIN+TMAX)/2) %>% 
  summarize(PCP_mm=sum(PRCP,na.rm=F),TMP_C=mean(TAVG,na.rm=T)) %>% 
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
  summarize(PCP_mm=sum(PCP_mm,na.rm=F),TMP_C=mean(TMP_C,na.rm=T))  %>% 
  mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(TMP_C))

# OWC Discharge 
# read channel output
# Manually deleted missing data circa 1994-9-30 to 1995-9-29 out of file
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
  mutate(DATE=ymd(DATE)) %>% 
  complete(DATE=seq.Date(as.Date(min(DATE)),as.Date(max(DATE)),by='day')) %>% 
  mutate(WY=year(DATE)) %>% 
  mutate(WY=ifelse(c(month(DATE) == 10 |month(DATE) == 11 | month(DATE) ==12), WY+1,WY)) %>% # do function only on selected rows
  filter(WY > min(WY) & WY < max(WY)) %>% 
  group_by(WY) %>%
  filter(WY >= 1981, WY <= 2019) %>% # Maybe should remove 1994 and 1995 bc missing a few months of data 
  summarize(DIS_MISSING=sum(is.na(DIS_CMS)),DIS_CMS=mean(DIS_CMS,na.rm=T)) %>% 
  filter(DIS_MISSING < 30) %>% 
  mutate(DIS_cat=NA) %>% #catergory for rankings
  mutate(DIS_cat=replace(DIS_cat,DIS_CMS >= quantile(DIS_CMS,c(0.9)), '90th percentile')) %>% 
  mutate(DIS_cat=replace(DIS_cat,DIS_CMS <= quantile(DIS_CMS,c(0.1)), '10th percentile')) 
# mutate(PCP_cat=replace(PCP_cat,PCP_mm <= quantile(PCP_mm,c(0.5)), '50th percentile'))
  
#should change these to water year
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
  
AnnualData<-full_join(ClimateSummary_annual,Dis_annual,by='WY')
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

### Bar plots of annual averages ####
tmp_extr<- quantile(AnnualData$TMP_C,c(0.05,0.5,0.95),na.rm=T)
pcp_extr<-quantile(AnnualData$PCP_mm,c(0.05,0.5,0.95),na.rm=T)
dis_extr<-quantile(AnnualData$DIS_CMS,c(0.05,0.5,0.95),na.rm=T)

TMP_annual<-ggplot(AnnualData, aes(x=WY,y=TMP_C,fill=TMP_cat))+geom_bar(stat='identity')+geom_hline(yintercept = tmp_extr[[3]])+
  geom_hline(yintercept = tmp_extr[[1]],linetype='dashed')+labs(x="")+scale_x_continuous(breaks=seq(1980,2020,by=1),limits =c(1980,2020) )+
  theme(panel.grid.major =   element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
        panel.border=element_rect(fill='transparent'),axis.text.x=element_text(angle=90,vjust = 0.5, hjust=1),legend.title=element_blank())

PCP_annual<-ggplot(AnnualData,aes(x=WY,y=PCP_mm,fill=PCP_cat))+geom_bar(stat='identity')+geom_hline(yintercept = pcp_extr[[3]])+
  geom_hline(yintercept = pcp_extr[[1]],linetype='dashed')+labs(x="")+scale_x_continuous(breaks=seq(1980,2020,by=1),limits =c(1980,2020) )+
  theme(panel.grid.major =   element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
        panel.border=element_rect(fill='transparent'),axis.text.x=element_text(angle=90,vjust = 0.5, hjust=1),legend.title=element_blank())

DIS_annual<-ggplot(AnnualData, aes(x=WY,y=DIS_CMS,fill=DIS_cat))+geom_bar(stat='identity')+geom_hline(yintercept = dis_extr[[3]])+
  geom_hline(yintercept = dis_extr[[1]],linetype='dashed')+labs(x="")+scale_x_continuous(breaks=seq(1980,2020,by=1),limits =c(1980,2020) )+
  theme(panel.grid.major =   element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
        panel.border=element_rect(fill='transparent'),axis.text.x=element_text(angle=90,vjust = 0.5, hjust=1),legend.title=element_blank())

ggarrange(TMP_annual,PCP_annual,DIS_annual,nrow=3,ncol=1,align='hv',common.legend = T)

ggsave('WaterYearSummary.png',last_plot(),height=150,width=200,units='mm')

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
  