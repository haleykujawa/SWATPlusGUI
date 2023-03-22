###### Read output from Jupyter notebook reading the climate data and write as SWAT+ pcp file #########
rm(list=ls())

library("here")
library("tidyverse")

### functions ######
spaceOutput<-function(data,nspaces){
  
  newData<-paste0(str_dup(" ",(nspaces-nchar(data))),data)
  return(newData)
  
}

spaceOutput_spacesecond<-function(data,nspaces){
  
  newData<-paste0(data,str_dup(" ",(nspaces-nchar(data))))
  return(newData)
  
}

#### read in climate data #####

#Temp is in C
#pcp in kg m-2 s-1

ClimateSummary<-data.frame(matrix(nrow=12,ncol=4))
colnames(ClimateSummary)<-c("dailypcp_mm", "tmp_avgC",    "model",       "time_period")

climatemodels<-c('ACCESS','CNRM')

for (clim in climatemodels){ # for every time model
  
  for (timeperiod in c('hist','future')){ # for each time period
    
    if (timeperiod == 'hist'){
      
      yr_start=1980
      yr_end=1999
      
    }else{
      
      yr_start=2040
      yr_end=2059
      
    }
   
  setwd(here("UW Climate Data",clim, timeperiod))

clim_file<-file.path(paste0(clim,'_',timeperiod,'.csv'))
climdata<-read.csv(clim_file)

# Use conversion of 1 kg/m -2 s-1 --> mm / s -1 using std volume of water = 1 g / cm-3
# This is based off water temperature and we only have air temp
# climdata$water_dens<-water.density()

# Convert date

# This isn't working because I accidentally processed the 2070-2099 data for ACCESS
# climdata$lag<-c(NA,climdata$time[-nrow(climdata)])
# 
# climdata <- climdata %>% mutate(time= ymd_hms(time)) %>%
#  mutate(lag= ymd_hms(lag)) %>%
#   mutate(elapse_time = time - lag)

# # check that all data is output hourly
# if (unique(climdata$elapse_time[-1]) != 1){
#   stop('time elapsed not consistently 1 hr')
#   
# }

# Summary table for comparing historical and future outputs
ClimateSummary_add <- climdata %>% 
  mutate(day_col = date(time)) %>%
  group_by(day_col) %>%
  filter(year(time) >= yr_start & year(time) <= yr_end) %>% # remove the one output for yr 2000
  mutate(pcp_mm = pcp*60*60) %>% # convert from kg /m-2 s-1 to mm
  summarize(tmp_minC=min(temp),tmp_maxC=max(temp),dailypcp_mm=sum(pcp_mm),tmp_avgC=mean(temp))

ClimateSummary_add <- ClimateSummary_add %>%
  summarize(dailypcp_mm=mean(dailypcp_mm),tmp_avgC=mean(tmp_avgC)) %>%
  mutate(model = clim, time_period= timeperiod)

ClimateSummary<-rbind(ClimateSummary,ClimateSummary_add)
  
  


# Summarize daily min and max tmp, set up date outputs needed for climate files
# don't filter any data for writing the climate files because need excess data as a warm up period
dailyClimData <- climdata %>% 
  mutate(day_col = date(time)) %>%
  group_by(day_col) %>%
  # filter(year(time) != 2000) %>% # remove the one output for yr 2000
  mutate(pcp_mm = pcp*60*60) %>% # convert from kg /m-2 s-1 to mm
  summarize(tmp_minC=min(temp),tmp_maxC=max(temp),dailypcp_mm=sum(pcp_mm),tmp_avgC=mean(temp)) %>%
  mutate(doy=yday(day_col)) %>%
  mutate(year=year(day_col)) %>%
  mutate(dailypcp_mm= format(round(dailypcp_mm,5))) %>% # I think these three lines could be better but leaving for now
  mutate(tmp_maxC= format(round(tmp_maxC,5))) %>%
  mutate(tmp_minC=format(round(tmp_minC,5))) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(tmp_maxC=spaceOutput(tmp_maxC,11)) %>%
  mutate(tmp_minC=spaceOutput(tmp_minC,12)) %>%
  mutate(dailypcp_mm=spaceOutput(dailypcp_mm,11)) %>%
  mutate(doy=spaceOutput(doy,5))



# Write SWAT+ climate files

nbyr<-max(as.numeric(dailyClimData$year))-min(as.numeric(dailyClimData$year))

# head of tmp file 
tmp_header<- c('owcmet_tmp.tmp: Temperature data - file written by SWAT+ editor 2022-01-21 12:20:49.114642\nnbyr     tstep       lat       lon      elev')

tmp_header1<-paste0(spaceOutput(as.character(nbyr),4), c('         0    41.378   -82.508   184.000'))

DF<-paste0(dailyClimData$year,dailyClimData$doy, dailyClimData$tmp_maxC,dailyClimData$tmp_minC)


file.create('owcmet_tmp.tmp')
climFile<-file.path('owcmet_tmp.tmp')
sink(climFile, type=c("output"), append = T)
write(tmp_header,climFile,sep = "\n",append=T)
write(tmp_header1,climFile,sep = "\n",append=T)
write(DF,climFile,sep = "\n",append=T)
sink()

# head of pcp file 
tmp_header<- c('owcmet_pcp.pcp: Precipitation data - file written by SWAT+ editor 2022-01-21 12:20:49.028861\nnbyr     tstep       lat       lon      elev')

DF<-paste0(dailyClimData$year,dailyClimData$doy, dailyClimData$dailypcp_mm)


file.create('owcmet_pcp.pcp')
climFile<-file.path('owcmet_pcp.pcp')
sink(climFile, type=c("output"), append = T)
write(tmp_header,climFile,sep = "\n",append=T)
write(tmp_header1,climFile,sep = "\n",append=T)
write(DF,climFile,sep = "\n",append=T)
sink()


}

}

