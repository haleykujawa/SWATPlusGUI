# method for developing climate change scenarios based on the historical record



# To dos
# Note what Norwalk and OWC data data flags were used in filtering data
# Replace missing 2008 temp data with OWC data -- done 4/18/23
# Move winter to be cohesive-- currently summarizing jan/feb, dec of same year, need dec previous year + jan/feb current year - done, see lines 193

# Did small spot checks to see that data was being calculated correctly
# dailyClim_final --> tmin never greater than tmax
# historical data (dailyClim, seasonal summaries) matches what's from obs
# Proper change in years (e.g., shift 1990 --> 2002 and add delta P and T, proper addition of delta if a historical year)

# if user selects number of years that are in the baseline, no changes will be made because if the number of years
# for any climate category is selected it will grab those years and remove them from the historical to be replaced
# it will also only replace years if the number of years selected is greater than what's in the baseline.
# Hence, selecting the exact years that are in the baseline will not allow the function to be applied

# Calculating daily climate needs to be removed-- can replace the recalculated WY with the WY data frame
# that is used to replace years 4/25/23

# Full seasons that are within the 1990-2019 WY are included. This excludes fall 2019

ClimateChange<-function(nyrs_LOWPCP_HIGHTMP,nyrs_HIGHPCP_AVGTMP,nyrs_AVGPCP_HIGHTMP,deltaC, deltaP){
  
 if( is.na(deltaP)){
   
   deltaP<-0
 }
  
  if( is.na(deltaC)){
    
    deltaC<-0
  }

  set.seed(1)
  
  # local_dir<-gsub("\\\\", "/",local_dir)
  local_dir<-here()
  
    
  ### functions ######
  spaceOutput<-function(data,nspaces){
    
    newData<-paste0(str_dup(" ",(nspaces-nchar(data))),data)
    return(newData)
    
  }
  
  spaceOutput_spacesecond<-function(data,nspaces){
    
    newData<-paste0(data,str_dup(" ",(nspaces-nchar(data))))
    return(newData)
    
  }


# rm(list=ls())
# library(tidyverse)
# library(here)
# library(magrittr)
# library(ggpubr)
# library(ggpmisc)

setwd(here::here('UW Climate Data','NORWALK_WWTP'))


### inputs for climate generator #########


# Changes to the annual
# deltaC C
# deltaP %

deltaP <-deltaP/100 # %

# clim_files<-c('3282969.csv','3282971.csv','3295661.csv','3295676.csv')
# # 2020-2023 file, although the number of columns is different, would need to bind in a different way
# # '3295684.csv'
# 
# obs_data<-c()
# 
# for (i in clim_files){
#   
#   obs_add<-read.csv(i)
#   obs_data<-rbind(obs_data,obs_add)
#   
# }
# # Precip rank #1 = wettest / flood
# # Temp rank #1 = coldest
# # sum of the precip + temp rank, high= drought year, low = flood, extreme wet/cold
# 
# ####### daily climate data #######
# 
# dailyClim<-obs_data %>%
#   mutate(DATE=ymd(DATE)) %>% 
#   complete(DATE=seq.Date(as.Date(min(DATE)),as.Date(max(DATE)),by='day')) %>% 
#   select(DATE,PRCP,TMIN,TMAX)
# 
# 
# ####### OWC GAP FILL #####
# # Read in data from OWC to fill any significant gaps in temperature, e.g., 2008
# 
# #OWCOMET_edit is removing the top few lines off the excel file
# #also edited the header names
# tmp <- file(here::here('UW Climate Data','NORWALK_WWTP','OWCOWMET.csv'))
# open(tmp, "r") #read
# 
# #read past headerline and save to rewrite the file
# readLines(tmp, n = 2) 
# 
# 
# headers<-readLines(tmp, n = 1) 
# headers<-strsplit(headers,split=",")
# headers<-data.frame(do.call(rbind, headers)) #unlist
# headers<-as.character(headers[1,])
# headers<-gsub("[^[:alnum:]]", "", headers)
# 
# DF<-readLines(tmp, n = -1) 
# #remove legend at bottom of file
# DF<-DF[-c(grep("Legend",DF):length(DF))]
# 
# 
# DF<-strsplit(DF,split=",")
# DF<-data.frame(do.call(rbind, DF)) #unlist
# 
# colnames(DF)<-headers
# 
# 
# # summarize minimum and maximum daily temperatures
# 
# OWC_temps<-DF %>% 
#   mutate(ATemp=as.numeric(ATemp),DATE=as.Date(DateTimeStamp,format="%m/%d/%Y %H:%M")) %>% 
#   complete(DATE=seq.Date(as.Date(min(DATE)),as.Date(max(DATE)),by='day')) %>% 
#   group_by(DATE) %>% 
#   summarize(TMP_MIN_OWC=min(ATemp,na.rm=T), TMP_MAX_OWC=max(ATemp,na.rm=T)) %>% 
#   mutate(TMP_MIN_OWC=ifelse(is.infinite(TMP_MIN_OWC),NA,TMP_MIN_OWC)) %>% 
#   mutate(TMP_MAX_OWC=ifelse(is.infinite(TMP_MAX_OWC),NA,TMP_MAX_OWC))
# 
# # gap fill dailyClim with OWC_data, remove OWC data
# dailyClim<-left_join(dailyClim,OWC_temps,by=c('DATE'))
# 
# dailyClim <- dailyClim %>% 
#   mutate(TMIN=ifelse(is.na(TMIN),TMP_MIN_OWC,TMIN)) %>% 
#   mutate(TMAX=ifelse(is.na(TMAX),TMP_MAX_OWC,TMAX)) %>% 
#   select('DATE','PRCP','TMIN','TMAX')
# 
# # Write daily clim here for GUI inputs
# write.csv(dailyClim,'dailyClim_hist.csv', row.names=F)
  
dailyClim<-read.csv('dailyClim_hist.csv')  

####### annual ############
# ClimateSummary_annual<-dailyClim %>% #change from obs_data to daily_clim
#   mutate(DATE = ymd(DATE), MONTH=month(ymd(DATE))) %>% 
#   mutate(WY=year(DATE)) %>% 
#   mutate(WY=ifelse(c(MONTH == 10 |MONTH == 11 | MONTH ==12), WY+1,WY)) %>% # do function only on selected rows
#   filter(WY > min(WY) & WY < max(WY)) %>% 
#   mutate(TMIN = replace(TMIN,is.na(TMAX),NA)) %>% # Don't calculate average daily temp if min or max is missing
#   mutate(TMAX = replace(TMAX,is.na(TMIN),NA)) %>%  
#   group_by(WY) %>%
#   mutate(TAVG=(TMIN+TMAX)/2) %>% 
#   summarize(PCP_mm=sum(PRCP,na.rm=T),TMP_C=mean(TAVG,na.rm=T),
#             n_pcp_missing = sum(is.na(PRCP)),n_tmp_missing = sum(is.na(TAVG))) %>% 
#   filter(n_pcp_missing < 30 & n_tmp_missing < 30, WY >= 1990) %>%  #Exclude years with more than a month of missing data %>% 
#   mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(-TMP_C),Overall_rank=(TMP_rank+PCP_rank)) %>% 
#   mutate(PCP_cat= NA, TMP_cat = NA) %>% #category for rankings
#   mutate(PCP_cat=replace(PCP_cat,PCP_mm >= quantile(PCP_mm,c(0.9)), '90th percentile')) %>% 
#   mutate(PCP_cat=replace(PCP_cat,PCP_mm <= quantile(PCP_mm,c(0.1)), '10th percentile')) %>% 
#   mutate(PCP_cat=replace(PCP_cat,PCP_mm >= quantile(PCP_mm,c(0.5)) & PCP_mm < quantile(PCP_mm,c(0.9)), '50th percentile')) %>% 
#   mutate(TMP_cat=replace(TMP_cat,TMP_C >= quantile(TMP_C,c(0.9)), '90th percentile')) %>% 
#   mutate(TMP_cat=replace(TMP_cat,TMP_C >= quantile(TMP_C,c(0.5)) & TMP_C < quantile(TMP_C,c(0.9)), '50th percentile'))  # remove cold years bc we only want years warmer than avg
# 
#   
# # Write seasonal historical summary
# write.csv(ClimateSummary_annual,'AnnualSummary_hist.csv', row.names=F) 

ClimateSummary_annual<-read.csv('AnnualSummary_hist.csv')

######## seasonal ################

# # Do seasonal summaries for each year
# ClimateSummary_seasonal<-dailyClim %>% 
#   mutate(DATE = ymd(DATE)) %>% 
#   mutate(WY=year(DATE),MONTH=month(DATE),YEAR=year(DATE),SZNYR=year(DATE)) %>% 
#   mutate(WY=ifelse(c(MONTH == 10 |MONTH == 11 | MONTH ==12), WY+1,WY)) %>% # do function only on selected rows
#   mutate(season=NA) %>% 
#   mutate(season = replace(season, c(MONTH == 12 |MONTH == 1 | MONTH ==2), 'winter')) %>% 
#   mutate(season = replace(season, c(MONTH == 3 |MONTH == 4 | MONTH ==5), 'spring')) %>% 
#   mutate(season = replace(season, c(MONTH == 6 |MONTH == 7 | MONTH ==8), 'summer')) %>% 
#   mutate(season = replace(season, c(MONTH == 9 |MONTH == 10 | MONTH ==11), 'fall')) %>% 
#   mutate(SZNYR=ifelse(MONTH == 12, SZNYR+1,SZNYR)) %>% # do function only on selected rows
#   mutate(TMIN = replace(TMIN,is.na(TMAX),NA)) %>% # Don't calculate average daily temp if min or max is missing
#   mutate(TMAX = replace(TMAX,is.na(TMIN),NA)) %>%  
#   group_by(SZNYR,season) %>%
#   mutate(TAVG=(TMIN+TMAX)/2) %>%  # make value NA if max or min is missing
#   summarize(PCP_mm=sum(PRCP,na.rm=T),TMP_C=mean(TAVG,na.rm=T),
#             n_pcp_missing = sum(is.na(PRCP)),n_tmp_missing = sum(is.na(TAVG))) %>% 
#   filter(n_pcp_missing <= 30 & n_tmp_missing <= 30,
#          SZNYR >= 1990 & SZNYR <= 2019 , !(SZNYR == 1990 & season =='winter') , !(SZNYR ==2019 & season == 'winter') ) %>%  #Exclude months with more than 10 days of missing data, also have to exclude beginning and ending year winters bc using consecutive winters
#   ungroup() %>% #This allows them to be ranked overall rather than with the group of year and month, should potentially just do rankings for group
#   group_by(season) %>% 
#   mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(TMP_C)) %>% 
#   mutate(PCP_cat= NA, TMP_cat = NA) %>% #catergory for rankings
#   mutate(PCP_cat=replace(PCP_cat,PCP_mm >= quantile(PCP_mm,c(0.9)), '90th percentile')) %>% 
#   mutate(PCP_cat=replace(PCP_cat,PCP_mm <= quantile(PCP_mm,c(0.1)), '10th percentile')) %>% 
#   mutate(PCP_cat=replace(PCP_cat,PCP_mm >= quantile(PCP_mm,c(0.5)) & PCP_mm < quantile(PCP_mm,c(0.9)), '50th percentile')) %>%
#   mutate(TMP_cat=replace(TMP_cat,TMP_C >= quantile(TMP_C,c(0.9)), '90th percentile')) %>% 
#   mutate(TMP_cat=replace(TMP_cat,TMP_C >= quantile(TMP_C,c(0.5)) & TMP_C < quantile(TMP_C,c(0.9)), '50th percentile')) 
# 
# # Write seasonal historical summary
# write.csv(ClimateSummary_seasonal,'SeasonalSummary_hist.csv', row.names=F)

ClimateSummary_seasonal<-read.csv('SeasonalSummary_hist.csv')

##### Generate climate change from the historical dataset #####

# Climate generator
# --> hot + dry summer year
# --> hot + wet winter year
# --> wet but not particularly hot (cold-ish)

WY <- data.frame(ClimateSummary_annual[,c(1:3)])

## if the number of years selected is 0, these become historical years that can be replaced





WY_LOWPCP_HIGHTMP<-c()
# Hot and dry temp
# if (nyrs_LOWPCP_HIGHTMP > 0 ){ # 11/30/23 remove this function bc if it is 0 it will just sample 0 years and nothing should change
# WY_LOWPCP_HIGHTMP<-AnnualData %>%
#   filter(TMP_C >= quantile(TMP_C, 0.75,na.rm=T) & TMP_C > mean(TMP_C,na.rm=T) & PCP_mm <= quantile(PCP_mm, 0.4,na.rm=T)) %>%
#   select('WY') %>%
#   as.vector() %>%
#   unlist() %>%
#   unname()
  
# LOWPCP_HIGHTMP_origNyrs <- length(WY_LOWPCP_HIGHTMP) 

# replace with known data so don't have to recalculate every time user changes the value
  # nyrs_LOWPCP_HIGHTMP 1991 1999 2010 2012 2016
WY_LOWPCP_HIGHTMP<-c(1991, 1999, 2010, 2012, 2016)
LOWPCP_HIGHTMP_origNyrs<-5


# If number of years wanted less than those in the dataset, sub sample what years to use so the others can be replaced as historical years
if (nyrs_LOWPCP_HIGHTMP < length(WY_LOWPCP_HIGHTMP)) {
  
  WY_LOWPCP_HIGHTMP<-sample(WY_LOWPCP_HIGHTMP, nyrs_LOWPCP_HIGHTMP, replace=F)
  
}

 # }

WY_HIGHPCP_AVGTMP<-c()
# Wet but not particularly hot
# if (nyrs_HIGHPCP_AVGTMP > 0) { 
# WY_HIGHPCP_AVGTMP<-AnnualData %>%
#   filter(TMP_C > mean(TMP_C,na.rm=T) & PCP_mm >= quantile(PCP_mm, 0.75,na.rm=T)) %>%
#   select('WY') %>%
#   as.vector() %>%
#   unlist() %>%
#   unname() 
# 
# HIGHPCP_AVGTMP_origNyrs <- length(WY_HIGHPCP_AVGTMP) 

  # replace with known data so don't have to recalculate every time user changes the value  
  # nyrs_HIGHPCP_AVGTMP 2000 2007 2008 2013 2019
  WY_HIGHPCP_AVGTMP<-c(2000, 2007, 2008, 2013, 2019)
  HIGHPCP_AVGTMP_origNyrs<-5

# If number of years wanted less than those in the dataset, sub sample what years to use so the others can be replaced as historical years
if (nyrs_HIGHPCP_AVGTMP < length(WY_HIGHPCP_AVGTMP)) {
 
  WY_HIGHPCP_AVGTMP<-sample(WY_HIGHPCP_AVGTMP, nyrs_HIGHPCP_AVGTMP, replace=F)
  
}

 # }

WY_AVGPCP_HIGHTMP<-c()
# Wet, warmer
# if (nyrs_AVGPCP_HIGHTMP > 0){
# WY_AVGPCP_HIGHTMP<-AnnualData %>%
#   filter(TMP_C >= quantile(TMP_C, 0.70,na.rm=T) & TMP_C > mean(TMP_C,na.rm=T) & PCP_mm >= mean(PCP_mm,na.rm=T)) %>%
#   select('WY') %>%
#   as.vector() %>%
#   unlist() %>%
#   unname()
# 
# AVGPCP_HIGHTMP_origNyrs <- length(WY_AVGPCP_HIGHTMP) 
  
  # replace with known data so don't have to recalculate every time user changes the value 
  # nyrs_AVGPCP_HIGHTMP 1998 2002 2018 
  WY_AVGPCP_HIGHTMP<-c(1998,2002,2018)
  AVGPCP_HIGHTMP_origNyrs<-3
  

# If number of years wanted less than those in the data set, sub sample what years to use so the others can be replaced as historical years
if (nyrs_AVGPCP_HIGHTMP < length(WY_AVGPCP_HIGHTMP)) {
  
  WY_AVGPCP_HIGHTMP<-sample(WY_AVGPCP_HIGHTMP, nyrs_AVGPCP_HIGHTMP, replace=F)
  
}


 # }

# Other option is to replace any years that aren't in the future scenario, replacing the coldest years first 
WY_hist<-ClimateSummary_annual %>%
  filter(!WY %in% c(WY_LOWPCP_HIGHTMP,WY_HIGHPCP_AVGTMP,WY_AVGPCP_HIGHTMP)) %>%
  select('WY','TMP_rank')

  # WY = WY / pcp (sum) / tmp (avg) 
  # WY_fut -- vector of water years to be a replacement
  # WY_hist -- vector of water years to be replaced
  
  avgP_hist<-mean(WY[,2],na.rm=T) 
  avgT_hist<-mean(WY[,3],na.rm=T)

  WY$rep_year<-NA

    # if years are greater than the original number, replace n number of dry years beyond what's already in the data set
    # AVGPCP_HIGHTMP
    if (nyrs_AVGPCP_HIGHTMP > LOWPCP_HIGHTMP_origNyrs){
      
      n_yr_replace <- nyrs_AVGPCP_HIGHTMP - length(WY_AVGPCP_HIGHTMP)
      
      WY_select <- WY_hist %>%  # select colder years first
        arrange(desc(TMP_rank)) %>% 
        slice(1:(n_yr_replace)) %>% 
        select('WY') %>% 
        as.vector() %>%
        unlist() %>%
        unname()
        
      
      sample_fut <- sample(WY_AVGPCP_HIGHTMP, n_yr_replace,replace=T) #select future climate year
      sample_hist <- sample(WY_select, n_yr_replace) #select average historical year to replace
      
      i<-1
      
      for (yr_fut in sample_fut){
        
      yr_hist <-sample_hist[i]   
      
      WY[WY[,1] %in% yr_hist, c(2:4)]<- cbind(WY[WY[,1] %in% yr_fut,c(2,3)],yr_fut)
      
      i<-i+1
      
      #remove historical year selected from sample vector 
      WY_hist <- WY_hist[!(WY_hist %in% sample_hist)]
      
      }
    }
  
  # HIGHPCP_AVGTMP
  if (nyrs_HIGHPCP_AVGTMP > HIGHPCP_AVGTMP_origNyrs){
    
    n_yr_replace <- nyrs_HIGHPCP_AVGTMP - length(WY_HIGHPCP_AVGTMP)
    
    WY_select <- WY_hist %>%  # select colder years first
      arrange(desc(TMP_rank)) %>% 
      slice(1:n_yr_replace) %>% 
      select('WY')%>% 
      as.vector() %>%
      unlist() %>%
      unname()
    
    sample_fut <- sample(WY_HIGHPCP_AVGTMP,  n_yr_replace,replace=T) #select future climate year
    sample_hist <- sample(WY_select,  n_yr_replace) #select average historical year to replace
    
    i<-1
    
    for (yr_fut in sample_fut){
      
      yr_hist <-sample_hist[i]   
      
      WY[WY[,1] %in% yr_hist, c(2:4)]<- cbind(WY[WY[,1] %in% yr_fut,c(2,3)],yr_fut)
      
      i<-i+1
    
    }
    
    #remove historical year selected from sample vector 
    WY_hist <- WY_hist[!(WY_hist %in% sample_hist)]
    
  }
  
  # LOWPCP_HIGHTMP
  if (nyrs_LOWPCP_HIGHTMP > LOWPCP_HIGHTMP_origNyrs){
    
    n_yr_replace <- nyrs_LOWPCP_HIGHTMP - length(WY_LOWPCP_HIGHTMP)
    
    WY_select <- WY_hist %>%  # select colder years first
      arrange(desc(TMP_rank)) %>% 
      slice(1:n_yr_replace) %>% 
      select('WY')%>% 
      as.vector() %>%
      unlist() %>%
      unname()
    
    sample_fut <- sample(WY_LOWPCP_HIGHTMP, n_yr_replace,replace=T) #select future climate year
    sample_hist <- sample(WY_select, n_yr_replace) #select average historical year to replace
    
    i<-1
    
    for (yr_fut in sample_fut){
      
      yr_hist <-sample_hist[i]   
      
      WY[WY[,1] %in% yr_hist, c(2:4)]<- cbind(WY[WY[,1] %in% yr_fut,c(2,3)],yr_fut)
      
      i<-i+1
      
      #remove historical year selected from sample vector 
      WY_hist <- WY_hist[!(WY_hist %in% sample_hist)]
      
    }
  }
    
    # Calculate futT and futP
    avgP<-mean(WY[,2],na.rm=T)
    avgT<-mean(WY[,3],na.rm=T)
    
    
  # i=i+1  
  # if (length(WY_hist)==0){
  #   
  #   break()
  #   
  # }
    
  # }
  
  (mean(WY[,2],na.rm=T)-avgP_hist)*100/avgP_hist
  (mean(WY[,3],na.rm=T)-avgT_hist)

# Build daily data with new years in WY data frame
  
dailyClim_final<-c()
WY<-WY[order(WY$WY),]
temp_add<-deltaC/length(WY$WY)
pcp_add<-deltaP/length(WY$WY)

# add WY index for filtering data
dailyClim <- dailyClim %>% 
  mutate(WY=year(DATE)) %>% 
  mutate(WY=ifelse(c(month(DATE) == 10 |month(DATE) == 11 | month(DATE) ==12), WY+1,WY))

#### Copy first 3 years as warm-up years to daily data set #####



  # keep original year
  dailyClim_add<- dailyClim %>% 
    filter(WY %in% c(1990,1991,1992)) %>% 
    mutate(warmup='yes',DATE=ymd(DATE)-years(3)) %>% 
    filter(!is.na(DATE)) %>%  # issue where date doesn't exists bc year replaced with is a leap year
    mutate(DATE=as.character(DATE)) %>% 
    select(DATE,PRCP,TMIN,TMAX,warmup)
  
  dailyClim_final<-rbind(dailyClim_final,dailyClim_add)




  
  for (i in c(1:length(WY$WY))){
    
    
    if (!is.na(WY$rep_year[i])){
      
      wateryear_replace<-WY$rep_year[i]
      wateryear_orig<-WY$WY[i]
      
      # replace year with other data
      
      # grab new data
      dailyClim_add<- dailyClim %>% 
        filter(WY == wateryear_replace) %>% 
        mutate(DAY_MONTH = format(as.Date(DATE),'%m-%d'))
      
      #grab original dates
      dailyClim_dates<- dailyClim %>%
        filter(WY == wateryear_orig) %>%
        select(DATE) %>%
        mutate(DAY_MONTH = format(as.Date(DATE),'%m-%d')) %>%
        mutate(YEAR= year(DATE)) # year as original year
      
      
      dailyClim_add <- left_join(dailyClim_dates,dailyClim_add,by='DAY_MONTH') %>% 
        mutate(DATE = paste0(YEAR,"-",DAY_MONTH),warmup='no') %>% 
        select(DATE,PRCP,TMIN,TMAX,warmup)
      
      # add linear change to delta C and delta P based on year
      dailyClim_add$TMIN<- dailyClim_add$TMIN + i*temp_add
      dailyClim_add$TMAX<- dailyClim_add$TMAX + i*temp_add
      
      dailyClim_add$PRCP<- dailyClim_add$PRCP + (i*pcp_add)*dailyClim_add$PRCP
      
      dailyClim_final<-rbind(dailyClim_final,dailyClim_add)

    } else {
      
      wateryear<-WY$WY[i]
      
      # keep original year
      dailyClim_add<- dailyClim %>% 
        filter(WY == wateryear) %>% 
        mutate(warmup='no') %>% 
        select(DATE,PRCP,TMIN,TMAX,warmup)
      
      # add linear change to delta C and delta P based on year
      dailyClim_add$TMIN<- dailyClim_add$TMIN + i*temp_add
      dailyClim_add$TMAX<- dailyClim_add$TMAX + i*temp_add
      
      dailyClim_add$PRCP<- dailyClim_add$PRCP + (i*pcp_add)*dailyClim_add$PRCP
      
      dailyClim_final<-rbind(dailyClim_final,dailyClim_add)
      
      
    }
    
    
  }

### Write SWAT pcp and tmp files to ClimateChange scenario folder #####
  
#### Compare old and new seasonal and annual data ####
#outliers are partial water years (1990, 2020), need to remove

### annual ###
ClimateSummary_annual$data<-'hist'

ClimateSummary_annual_fut<-dailyClim_final %>% 
  filter(warmup=='no') %>% 
  mutate(DATE = ymd(DATE), MONTH=month(ymd(DATE))) %>% 
  mutate(WY=year(DATE)) %>% 
  mutate(WY=ifelse(c(MONTH == 10 |MONTH == 11 | MONTH ==12), WY+1,WY)) %>% # do function only on selected rows
  # filter(WY > min(WY) & WY < max(WY)) %>% 
  mutate(TMIN = replace(TMIN,is.na(TMAX),NA)) %>% # Don't calculate average daily temp if min or max is missing
  mutate(TMAX = replace(TMAX,is.na(TMIN),NA)) %>%  
  group_by(WY) %>%
  mutate(TAVG=(TMIN+TMAX)/2) %>% 
  summarize(PCP_mm=sum(PRCP,na.rm=T),TMP_C=mean(TAVG,na.rm=T),
            n_pcp_missing = sum(is.na(PRCP)),n_tmp_missing = sum(is.na(TAVG))) %>% 
  filter(n_pcp_missing < 30 & n_tmp_missing < 30, WY >= 1990 & WY <= 2019) %>%  #Exclude years with more than a month of missing data %>% 
  mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(-TMP_C),Overall_rank=(TMP_rank+PCP_rank)) %>% 
  mutate(PCP_cat= NA, TMP_cat = NA) %>% #category for rankings
  mutate(data= 'fut') 
  # mutate(PCP_cat=replace(PCP_cat,PCP_mm >= quantile(PCP_mm,c(0.9)), '90th percentile')) %>% 
  # mutate(PCP_cat=replace(PCP_cat,PCP_mm <= quantile(PCP_mm,c(0.1)), '10th percentile')) %>% 
  # mutate(PCP_cat=replace(PCP_cat,PCP_mm >= quantile(PCP_mm,c(0.5)) & PCP_mm < quantile(PCP_mm,c(0.9)), '50th percentile')) %>% 
  # mutate(TMP_cat=replace(TMP_cat,TMP_C >= quantile(TMP_C,c(0.9)), '90th percentile')) %>% 
  # mutate(TMP_cat=replace(TMP_cat,TMP_C >= quantile(TMP_C,c(0.5)) & TMP_C < quantile(TMP_C,c(0.9)), '50th percentile'))  # remove cold years bc we only want years warmer than avg
  
FinalAnnualSummary<-rbind(ClimateSummary_annual,ClimateSummary_annual_fut)
# FinalAnnualSummary$data<-factor(FinalAnnualSummary$data, ordered=T,levels=c('hist','fut'))

FUT_CLIM_WY<-FinalAnnualSummary %>% 
  group_by(data) %>% 
  summarize(TMP_C=mean(TMP_C,na.rm=T),PCP_mm=mean(PCP_mm,na.rm=T)) 

FUT_CLIM_ADD<-data.frame(matrix(nrow=1,ncol=0))
FUT_CLIM_ADD$data[1]<-'Change (C, %)'
FUT_CLIM_ADD$TMP_C[1]<- FUT_CLIM_WY$TMP_C[FUT_CLIM_WY$data=='fut'] - FUT_CLIM_WY$TMP_C[FUT_CLIM_WY$data=='hist']
FUT_CLIM_ADD$PCP_mm[1]<-( FUT_CLIM_WY$PCP_mm[FUT_CLIM_WY$data=='fut'] - FUT_CLIM_WY$PCP_mm[FUT_CLIM_WY$data=='hist'] ) *100 / FUT_CLIM_WY$PCP_mm[FUT_CLIM_WY$data=='hist']

FUT_CLIM_WY<-rbind(FUT_CLIM_ADD,FUT_CLIM_WY)
rm(FUT_CLIM_ADD)

colnames(FUT_CLIM_WY)<-c('Data','Temperature', 'Precipitation')
FUT_CLIM_WY[,1]<-c('Change between historical and user-generated climate scenario','User-generated climate scenario','Historical (1990-2019)')

FUT_CLIM_WY<-FUT_CLIM_WY %>% 
  arrange(match(c('Historical (1990-2019)','User-generated climate scenario','Change between historical and user-generated climate scenario'), Data)) %>% 
  mutate(Temperature= round(Temperature,2),Precipitation= round(Precipitation,1)) %>% 
  mutate(Temperature= paste(Temperature,'C'),Precipitation= paste(Precipitation,c('mm','mm','%')))

# This goes into reactive plot
# PCP_ANNUAL_PLOT<-ggplot(FinalAnnualSummary,aes(y=PCP_mm,x=data))+geom_boxplot()
# TMP_ANNUAL_PLOT<-ggplot(FinalAnnualSummary,aes(y=TMP_C,x=data))+geom_boxplot()
# 
# # Table
# final_table<-data.frame(matrix(nrow=1,ncol=0))
# 
# fut_pcp<-FinalAnnualSummary %>%
#   filter(data=='fut') %>%
#   summarize(PCP_mm = sum(PCP_mm,na.rm=T)) %>% 
#   getElement('PCP_mm')
# 
# hist_pcp<-FinalAnnualSummary %>%
#   filter(data=='hist') %>%
#   summarize(PCP_mm = sum(PCP_mm,na.rm=T)) %>% 
#   getElement('PCP_mm')
# 
# # future - hist / hist 
# final_table$`Change in precipitation (%)`<- round((fut_pcp-hist_pcp)*100/hist_pcp,2)
# 
# fut_tmp<-FinalAnnualSummary %>%
#   filter(data=='fut') %>%
#   summarize(TMP_C = mean(TMP_C,na.rm=T)) %>% 
#   getElement('TMP_C')
# 
# hist_tmp<-FinalAnnualSummary %>%
#   filter(data=='hist') %>%
#   summarize(TMP_C = mean(TMP_C,na.rm=T)) %>% 
#   getElement('TMP_C')
# 
# final_table$`Change in temperature (C)` <- round((fut_tmp-hist_tmp),2)
# 
# ANNUAL_TABLE <- ggplot() +                             
#   theme_void() +
#   annotate(geom = "table",
#            x = 1,
#            y = 1,
#            label = list(final_table))
# 
# nyr_change<-data.frame(matrix(nrow=3,ncol=0))
# nyr_change$description<-c('high tmp, low pcp (drought)','high tmp, avg pcp (warm)', 'high pcp, avg tmp (wet)')
# nyr_change$HIST_YRS<-c(length(WY_LOWPCP_HIGHTMP),length(WY_AVGPCP_HIGHTMP),length(WY_HIGHPCP_AVGTMP))
# nyr_change$FUT_YRS<-c(nyrs_LOWPCP_HIGHTMP+ length(WY_LOWPCP_HIGHTMP),nyrs_AVGPCP_HIGHTMP+length(WY_AVGPCP_HIGHTMP),nyrs_HIGHPCP_AVGTMP+length(WY_HIGHPCP_AVGTMP))
# nyr_change$CHANGE<-nyr_change$FUT_YRS-nyr_change$HIST_YRS
# 
# NYR_TABLE <- ggplot() +                             
#   theme_void() +
#   annotate(geom = "table",
#            x = 1,
#            y = 1,
#            label = list(nyr_change))
# 
# ggarrange(PCP_ANNUAL_PLOT,TMP_ANNUAL_PLOT,ANNUAL_TABLE,NYR_TABLE,nrow=4,ncol=1)
# ggsave("Annual_changes.png",last_plot(),height=200,width=100,units='mm')

### seasonal ###
ClimateSummary_seasonal$data<-'hist'

ClimateSummary_seasonal_fut<-dailyClim_final %>% 
  filter(warmup=='no') %>% 
  mutate(DATE = ymd(DATE)) %>% 
  mutate(WY=year(DATE),MONTH=month(DATE),YEAR=year(DATE),SZNYR=year(DATE)) %>% 
  mutate(WY=ifelse(c(MONTH == 10 |MONTH == 11 | MONTH ==12), WY+1,WY)) %>% # do function only on selected rows
  mutate(SZNYR=ifelse(MONTH == 12, SZNYR+1,SZNYR)) %>% # do function only on selected rows
  mutate(season=NA) %>% 
  mutate(season = replace(season, c(MONTH == 12 |MONTH == 1 | MONTH ==2), 'winter')) %>% 
  mutate(season = replace(season, c(MONTH == 3 |MONTH == 4 | MONTH ==5), 'spring')) %>% 
  mutate(season = replace(season, c(MONTH == 6 |MONTH == 7 | MONTH ==8), 'summer')) %>% 
  mutate(season = replace(season, c(MONTH == 9 |MONTH == 10 | MONTH ==11), 'fall')) %>% 
  mutate(TMIN = replace(TMIN,is.na(TMAX),NA)) %>% # Don't calculate average daily temp if min or max is missing
  mutate(TMAX = replace(TMAX,is.na(TMIN),NA)) %>%  
  group_by(SZNYR,season) %>%
  mutate(TAVG=(TMIN+TMAX)/2) %>%  # make value NA if max or min is missing
  summarize(PCP_mm=sum(PRCP,na.rm=T),TMP_C=mean(TAVG,na.rm=T),
            n_pcp_missing = sum(is.na(PRCP)),n_tmp_missing = sum(is.na(TAVG))) %>% 
  filter(n_pcp_missing <= 30 & n_tmp_missing <= 30,
         SZNYR >= 1990 & SZNYR <= 2019 , !(SZNYR ==2019 & season == 'fall')) %>% # have to exclude these bc water years don't follow the seasons
  ungroup() %>% #This allows them to be ranked overall rather than with the group of year and month, should potentially just do rankings for group
  group_by(season) %>% 
  mutate(PCP_rank=rank(-PCP_mm),TMP_rank=rank(TMP_C)) %>% 
  mutate(PCP_cat= NA, TMP_cat = NA) %>%   #catergory for rankings
  mutate(data='fut')
  # mutate(PCP_cat=replace(PCP_cat,PCP_mm >= quantile(PCP_mm,c(0.9)), '90th percentile')) %>% 
  # mutate(PCP_cat=replace(PCP_cat,PCP_mm <= quantile(PCP_mm,c(0.1)), '10th percentile')) %>% 
  # mutate(PCP_cat=replace(PCP_cat,PCP_mm >= quantile(PCP_mm,c(0.5)) & PCP_mm < quantile(PCP_mm,c(0.9)), '50th percentile')) %>%
  # mutate(TMP_cat=replace(TMP_cat,TMP_C >= quantile(TMP_C,c(0.9)), '90th percentile')) %>% 
  # mutate(TMP_cat=replace(TMP_cat,TMP_C >= quantile(TMP_C,c(0.5)) & TMP_C < quantile(TMP_C,c(0.9)), '50th percentile')) 

FinalSeasonalSummary<-rbind(ClimateSummary_seasonal,ClimateSummary_seasonal_fut)
FinalSeasonalSummary$data<-factor(FinalSeasonalSummary$data, ordered=T,levels=c('hist','fut'))

# This goes into reactive plot
# PCP_SEASON_PLOT<-ggplot(FinalSeasonalSummary,aes(y=PCP_mm,x=data))+geom_boxplot()+facet_wrap(vars(season),scales='free_y')
# TMP_SEASON_PLOT<-ggplot(FinalSeasonalSummary,aes(y=TMP_C,x=data))+geom_boxplot()+facet_wrap(vars(season),scales='free_y')
# 
# 
#   fut_seasonal<-FinalSeasonalSummary %>%
#     filter(data=='fut') %>%
#     group_by(season) %>% 
#     summarize(PCP_mm_future=round(mean(PCP_mm,na.rm=T),2),TMP_C_future=round(mean(TMP_C),2)) 
#   
#   hist_seasonal<-FinalSeasonalSummary %>%
#     filter(data=='hist') %>%
#     group_by(season) %>% 
#     summarize(PCP_mm_historical=round(mean(PCP_mm,na.rm=T),2),TMP_C_historical=round(mean(TMP_C),2)) 
#   
#   seasonal_summary<-left_join(hist_seasonal,fut_seasonal,by=c('season'))
#   
#   seasonal_summary$Change_in_pcp_percent<-round ((seasonal_summary$PCP_mm_future - seasonal_summary$PCP_mm_historical) * 100 / seasonal_summary$PCP_mm_historical , 1)
#   seasonal_summary$Change_in_tmp_C<-round ((seasonal_summary$TMP_C_future - seasonal_summary$TMP_C_historical), 2)
#   
#   SEASONAL_TABLE <- ggplot() +                             
#     theme_void() +
#     annotate(geom = "table",
#              x = 1,
#              y = 1,
#              label = list(seasonal_summary))
#   
#   ggarrange(PCP_SEASON_PLOT,TMP_SEASON_PLOT,SEASONAL_TABLE,nrow=3,ncol=1)
#   ggsave("Seasonal_changes.png",last_plot(),height=200,width=225,units='mm')

##### Write SWAT+ pcp and tmp files ##############

# Summarize daily min and max tmp, set up date outputs and spacing needed for SWAT climate files
# don't filter any data for writing the climate files because need excess data as a warm up period
dailyClimData <- dailyClim_final %>% 
  # group_by(DATE) %>%
  # filter(year(time) != 2000) %>% # remove the one output for yr 2000
  # mutate(pcp_mm = pcp*60*60) %>% # convert from kg /m-2 s-1 to mm -- not needed since processed with 'ReadUWData'
  # summarize(tmp_minC=min(owc_airtemp),tmp_maxC=max(owc_airtemp),dailypcp_mm=sum(owc_pcp),tmp_avgC=((min(owc_airtemp)+max(owc_airtemp))/2)) %>% # Change column names to match ReadUWClimate output
  mutate(doy=yday(DATE)) %>%
  mutate(year=year(DATE)) %>%
  mutate(PRCP = replace(PRCP, is.na(PRCP),-99)) %>% 
  mutate(TMIN = replace(TMIN, is.na(TMIN),-99)) %>%
  mutate(TMAX = replace(TMAX, is.na(TMAX),-99)) %>%
  mutate(PRCP = format(round(PRCP,5))) %>% # I think these three lines could be better but leaving for now
  mutate(TMAX= format(round(TMAX,5))) %>%
  mutate(TMIN=format(round(TMIN,5))) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(TMAX=spaceOutput(TMAX,11)) %>%
  mutate(TMIN=spaceOutput(TMIN,12)) %>%
  mutate(PRCP=spaceOutput(PRCP,11)) %>%
  mutate(doy=spaceOutput(doy,5))

rm(dailyClim_final)





### Write SWAT+ climate files ####
TxtInOut<-file.path(paste0(local_dir,"/Scenarios",'/userClimScen'))

setwd(TxtInOut)


nbyr<-max(as.numeric(dailyClimData$year))-min(as.numeric(dailyClimData$year))+1

### Write tmp file  ####
# head of tmp file 
tmp_header<- c('owcmet_tmp.tmp: Temperature data - file written by SWAT+ editor 2022-01-21 12:20:49.114642\nnbyr     tstep       lat       lon      elev')

tmp_header1<-paste0(spaceOutput(as.character(nbyr),4), c('         0    41.378   -82.508   184.000'))

DF<-paste0(dailyClimData$year,dailyClimData$doy, dailyClimData$TMAX,dailyClimData$TMIN,'  ') # add 2 empty spaces to see if this is causing the issue w temp file

climFile<-file.path('owcmet_tmp.tmp')
if (file.exists(climFile)){
  
  # wipe file clean 
  close( file( climFile, open="w" ) ) 
  
}else{
  
  # create file
  file.create(climFile) 
  
}



sink(climFile, type=c("output"), append = T)
write(tmp_header,climFile,sep = "\n",append=T)
write(tmp_header1,climFile,sep = "\n",append=T)
write(DF,climFile,sep = "\n",append=T)
sink()

#### Write pcp file ####
# head of pcp file 
tmp_header<- c('owcmet_pcp.pcp: Precipitation data - file written by SWAT+ editor 2022-01-21 12:20:49.028861\nnbyr     tstep       lat       lon      elev')

DF<-paste0(dailyClimData$year,dailyClimData$doy, dailyClimData$PRCP)

climFile<-file.path('owcmet_pcp.pcp')
if (file.exists(climFile)){
  
  # wipe file clean 
  close( file( climFile, open="w" ) ) 
  
}else{
  
  # create file
  file.create(climFile) 
  
}


climFile<-file.path('owcmet_pcp.pcp')
sink(climFile, type=c("output"), append = T)
write(tmp_header,climFile,sep = "\n",append=T)
write(tmp_header1,climFile,sep = "\n",append=T)
write(DF,climFile,sep = "\n",append=T)
sink()






return(list(FUT_CLIM_WY))  

# Write csv of daily data that then is used for writing the daily files when 'run swat' is pushed

}