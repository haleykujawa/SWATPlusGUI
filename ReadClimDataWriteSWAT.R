###### Read output from Jupyter notebook reading the climate data and write as SWAT+ pcp file #########
# 3/26 - need to change average temp too min+max/2
rm(list=ls())

library("here")
library("tidyverse")
library("gridExtra")
library("ggpmisc")

### functions ######
spaceOutput<-function(data,nspaces){
  
  newData<-paste0(str_dup(" ",(nspaces-nchar(data))),data)
  return(newData)
  
}

spaceOutput_spacesecond<-function(data,nspaces){
  
  newData<-paste0(data,str_dup(" ",(nspaces-nchar(data))))
  return(newData)
  
}

#### Read in processed climate data #####

#Temp is in C
#pcp in kg m-2 s-1

ClimateSummary<-data.frame(matrix(ncol=4,nrow=0))
colnames(ClimateSummary)<-c("dailypcp_mm", "tmp_avgC",    "model",       "time_period")

climatemodels<-c('ACCESS','CNRM','IPSL') # exclude GFDL, see if all runs complete 3/23 8:43
obs_hist<-c('NORWALK_WWTP')

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

# relabel all 0:00 as the day before
ind<-grep("00",substr(climdata$time,12,13))
climdata$time[ind]<-as.character(ymd_hms(climdata$time[ind])-1)
  

# Summary table for comparing historical and future outputs
ClimateSummary_add <- climdata %>% 
  mutate(day_col = date(time)) %>%
  group_by(day_col) %>%
  filter(year(time) >= yr_start & year(time) <= yr_end) %>% # remove the one output for yr 2000
  mutate(pcp_mm = pcp*60*60) %>% # convert from kg /m-2 s-1 to mm
  summarize(tmp_minC=min(temp),tmp_maxC=max(temp),dailypcp_mm=sum(pcp_mm),tmp_avgC=mean(temp))

# annual average
ClimateSummary_add <- ClimateSummary_add %>%
  group_by(year(day_col)) %>%
  summarize(annualpcp_mm=sum(dailypcp_mm),tmp_avgC=mean(tmp_avgC)) %>%
  mutate(model = clim, time_period= timeperiod)

ClimateSummary<-rbind(ClimateSummary,ClimateSummary_add)
  
 


# Summarize daily min and max tmp, set up date outputs and spacing needed for SWAT climate files
# don't filter any data for writing the climate files because need excess data as a warm up period
dailyClimData <- climdata %>% 
  mutate(day_col = date(time)) %>%
  group_by(day_col) %>%
  # filter(year(time) != 2000) %>% # remove the one output for yr 2000
  mutate(pcp_mm = pcp*60*60) %>% # convert from kg /m-2 s-1 to mm
  summarize(tmp_minC=min(temp),tmp_maxC=max(temp),dailypcp_mm=sum(pcp_mm),tmp_avgC=((min(temp)+max(temp))/2)) %>%
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



### Write SWAT+ climate files ####
TxtInOut<-file.path(here("UW Climate Data",clim, timeperiod,'TxtInOut'))

if (dir.exists(TxtInOut)){
  
  setwd(TxtInOut)
  unlink(dir(TxtInOut)) # erase all files
  
}else{
  
  dir.create(TxtInOut)
  setwd(TxtInOut)
  
}

nbyr<-max(as.numeric(dailyClimData$year))-min(as.numeric(dailyClimData$year))+1

### Write tmp file to current folder UW Climate Data ####
# head of tmp file 
tmp_header<- c('owcmet_tmp.tmp: Temperature data - file written by SWAT+ editor 2022-01-21 12:20:49.114642\nnbyr     tstep       lat       lon      elev')

tmp_header1<-paste0(spaceOutput(as.character(nbyr),4), c('         0    41.378   -82.508   184.000'))

DF<-paste0(dailyClimData$year,dailyClimData$doy, dailyClimData$tmp_maxC,dailyClimData$tmp_minC,'  ') # add 2 empty spaces to see if this is causing the issue w temp file

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

### Copy future tmp file to scenarios folder ####
if (timeperiod =='future'){
  
  climFile<-file.path(here('Scenarios',clim,'owcmet_tmp.tmp'))
  
  close( file( climFile, open="w" )) # wipe current file
  
  sink(climFile, type=c("output"), append = T) # write new output to file
  write(tmp_header,climFile,sep = "\n",append=T)
  write(tmp_header1,climFile,sep = "\n",append=T)
  write(DF,climFile,sep = "\n",append=T)
  sink()
  
  
}

### Write pcp file to current folder UW Climate Data ####

# head of pcp file 
tmp_header<- c('owcmet_pcp.pcp: Precipitation data - file written by SWAT+ editor 2022-01-21 12:20:49.028861\nnbyr     tstep       lat       lon      elev')

DF<-paste0(dailyClimData$year,dailyClimData$doy, dailyClimData$dailypcp_mm)

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

### Copy future pcp file to scenarios folder ####
if (timeperiod =='future'){
  
  climFile<-file.path(here('Scenarios',clim,'owcmet_pcp.pcp'))
  
  close( file( climFile, open="w" )) # wipe current file
  
  sink(climFile, type=c("output"), append = T) # write new output to file
  write(tmp_header,climFile,sep = "\n",append=T)
  write(tmp_header1,climFile,sep = "\n",append=T)
  write(DF,climFile,sep = "\n",append=T)
  sink()
  
  
}


}

}

setwd(here("UW Climate Data"))
write.csv(ClimateSummary,"AnnualClimateSummary.csv",row.names=F)

### Obs data Norwalk WWTP ######

obs_data<-c()
  
  setwd(here("UW Climate Data", obs_hist))
  
  clim_files<-list.files()
  
  for (i in clim_files){
    
    obs_add<-read.csv(i)
    obs_data<-rbind(obs_data,obs_add)
    
  }
  
  
  ClimateSummary_add<-obs_data %>% 
    mutate(DATE = ymd(DATE)) %>% 
    group_by(year(DATE)) %>%
    mutate(TAVG=(TMIN+TMAX)/2) %>% 
    summarize(sum(PRCP,na.rm=T),mean(TAVG,na.rm=T)) %>% 
    mutate(model = 'Observed hist',timeperiod='hist')

  colnames(ClimateSummary_add)<-colnames(ClimateSummary)
  
  ClimateSummary<-rbind(ClimateSummary,ClimateSummary_add)
  

    



### Make plots of all climate data ####
setwd(here('UW Climate Data'))
ClimateSummary<-ClimateSummary %>% 
  mutate(model =factor(model)) %>%
  mutate(time_period=factor(time_period,levels=c('hist','future'),ordered=T))

pcp_plot<-ggplot(ClimateSummary, aes(x=model,y=annualpcp_mm,fill=time_period))+geom_boxplot()+
  # geom_hline(yintercept=1090,linetype='dashed')+geom_text(aes(x='CNRM',y=1200,label='Historical average annual (2013-2020)'))+
  labs(x="",y="Annual precipitation (mm)")+
  scale_fill_discrete(labels=c("hist"="historical (1980-1999)","future"="future (2040-2059)"))+
  theme_bw()

tmp_plot<-ggplot(ClimateSummary, aes(x=model,y=tmp_avgC,fill=time_period))+
  geom_boxplot()+
  # geom_hline(yintercept=10.9,linetype='dashed')+geom_text(aes(x='CNRM',y=11.5,label='Historical (2013-2020)'))+
  labs(x="",y="Annual average yearly temp (C)")+
  scale_fill_discrete(labels=c("hist"="historical (1980-1999)","future"="future (2040-2059)"))+
  theme_bw()

climate_data_table<-ClimateSummary %>%
  group_by(model,time_period) %>%
  summarize(annual_pcp_avg=mean(annualpcp_mm),annual_tmp_avg=mean(tmp_avgC))

# climate change table
final_table<-data.frame(matrix(ncol=3,nrow=length(climatemodels))) 
colnames(final_table)<-c('Climate model', 'Change in precipitation (%)', 'Change in temperature (C)')
final_table$`Climate model`<-climatemodels

for (clim in climatemodels) {
 
   
fut_pcp<-climate_data_table %>%
  filter(model==clim,time_period=='future') %>%
  getElement('annual_pcp_avg')

hist_pcp<-climate_data_table %>%
  filter(model==clim,time_period=='hist') %>%
  getElement('annual_pcp_avg')
  
# future - hist / hist 
final_table$`Change in precipitation (%)`[final_table$`Climate model` == clim] <- round((fut_pcp-hist_pcp)*100/hist_pcp,2)

fut_tmp<-climate_data_table %>%
  filter(model==clim,time_period=='future') %>%
  getElement('annual_tmp_avg')

hist_tmp<-climate_data_table %>%
  filter(model==clim,time_period=='hist') %>%
  getElement('annual_tmp_avg')

final_table$`Change in temperature (C)`[final_table$`Climate model` == clim] <- round((fut_tmp-hist_tmp),2)
  
  
}

# # Check summary
# mean(ClimateSummary$annualpcp_mm[ClimateSummary$model=='ACCESS' & ClimateSummary$time_period=='hist'])
# mean(ClimateSummary$annualpcp_mm[ClimateSummary$model=='CNRM' & ClimateSummary$time_period=='future'])
# mean(ClimateSummary$tmp_avgC[ClimateSummary$model=='CNRM' & ClimateSummary$time_period=='future'])

# Table for adding overall changes in climate 
ggp_table <- ggplot() +                             
theme_void() +
annotate(geom = "table",
x = 1,
y = 1,
label = list(final_table))

# output_data<-plot1+ggp_table

finalPlot<-grid.arrange(pcp_plot,tmp_plot,ggp_table)
ggsave("ClimateSummary.png",finalPlot,height=200,width=150,units="mm")

#### Run SWAT for all historical runs and get averages ######

### Copy over baseline files ####
# Copy over baseline files except for pcp, tmp, print.prt, and time.sim
# I manually changed and added the print.prt and time.sim to the UW Climate folder


BaselineFiles<-list.files(here("Baseline"))
BaselineFiles <-BaselineFiles[!grepl(paste0(c("time.sim","print.prt","owcmet_pcp.pcp","owcmet_tmp.tmp",
                                              "owcmet_hmd.hmd","hmd.cli","owcmet_wind.wnd","wnd.cli","weather-sta.cli"),collapse="|"),BaselineFiles)] # don't copy climate data we don't have

time_files<-c("time.sim","print.prt","weather-sta.cli")

baseline_data<-data.frame(matrix(nrow=5,ncol=2))
colnames(baseline_data)<-c("variable","baseline")
baseline_data$variable<-c('discharge_cms', 'solp_kg','sedp_kg','sediment_kg','totp_kg')

headers<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"areaha",	"precipha.m",	"evapha.m",	
           "seepha.m",	"flo_storm.3.s",	"sed_stormtons",	"orgn_storkgN",	"sedp_storkgP",	"no3_storkgN",	"solp_storkgP",
           "chla_storkg",	"nh3_storkgN",	"no2_storkgN",	"cbod_storkg",	"dox_storkg",	"san_stortons",	"sil_stortons",	"cla_stortons",	"sag_stortons",
           "lag_stortons",	"grv_stortons",	"null1", "setl_stor",	"setlp_stor",	"flo_inm.3.s",	"sed_inmtons",	"orgn_inkgN",	"sedp_inkgP",	"no3_inkgN",
           "solp_inkgP",	"chla_inkg",	"nh3_inkgN",	"no2_inkgN",	"cbod_inkg",	"dox_inkg",	"san_intons",	"sil_intons",	"cla_intons",
           "sag_intons",	"lag_intons",	"grv_intons",	"null",	 "setl_in",	"setlp_in","flo_outm.3.s",	"sed_outmtons",	"orgn_outkgN",	"sedp_outkgP",	"no3_outkgN",
           "solp_outkgP",	"chla_outkg",	"nh3_outkgN",	"no2_outkgN",	"cbod_outkg",	"dox_outkg",	"san_outtons",	"sil_outtons",	"cla_outtons",
           "sag_outtons",	"lag_outtons",	"grv_outtons",	"null2", "setl_out",	"setlp_out", "water_tempdegC")#"null3","null4","null5","null6","null7")


# if files are already copied SWAT will crash for some reason, need to add in deleting files before copying them over

for (clim in climatemodels){
  
  print(paste0(clim, ' hist run started'))
  
# Want to change this to be run in a TxtInOut rather than just in the folder
  baseline_data$baseline<-NA # reset data
  scenario_dir<-here("UW Climate Data",clim,"hist","TxtInOut") 
  
  

file.copy(from = file.path(here('Baseline'),BaselineFiles),   # Copy files
          to = file.path(scenario_dir,BaselineFiles))

 file.copy(from = file.path(here("UW Climate Data"),time_files),   # Copy files
          to = file.path(scenario_dir,time_files))
 
 # run SWAT
 # historical climate runs are done within the UW climate folder
 # future runs with mgt scenarios are done within the Scenarios folder
 setwd(scenario_dir)
 system('SWATPlus_60.5.5.exe',ignore.stdout = F,ignore.stderr = F) #run executable
 
 # read channel output
 tmp <- file('channel_sd_yr.txt')
 open(tmp, "r") #read
 

 readLines(tmp, n = 3)   #read past headerlines
 
 DF<-readLines(tmp,n=-1)
 
 close(tmp)
 DF<-strsplit(DF,split=" ") #split based on spacing
 DF<-lapply(DF, function(z){ z[z != ""]}) # remove empty spaces
 DF<-data.frame(do.call(rbind, DF)) #unlist
 colnames(DF)<-headers
 

 DF<-DF%>%
   filter(gis_id=="46")  # Berlin Rd 
 
 DF[,c(1:6,8:(ncol(DF)-1))]<-DF[,c(1:6,8:(ncol(DF)-1))]%>%
   unlist()%>%
   as.numeric()
 
 baseline_data$baseline[baseline_data$variable=="discharge_cms"]<-mean(DF$flo_outm.3.s,na.rm=T)
 baseline_data$baseline[baseline_data$variable=="solp_kg"]<-sum(DF$solp_outkgP,na.rm=T)
 baseline_data$baseline[baseline_data$variable=="sedp_kg"]<-sum(DF$sedp_outkgP,na.rm=T)
 baseline_data$baseline[baseline_data$variable=="sediment_kg"]<-sum(DF$sed_outmtons,na.rm=T)
 baseline_data$baseline[baseline_data$variable=="totp_kg"]<-sum(DF$solp_outkgP + DF$sedp_outkgP,na.rm=T)

 setwd(here("Scenarios",clim))
 write.csv(baseline_data,"baseline_data_avg.csv",row.names=F)
 
 print(paste0(clim, ' hist run done'))

}

