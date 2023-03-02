##reading stream output
## Same as read channel daily 7, but only reads raw output instead of reading processed output, just in case I make changes and run again
## Note monthly and annual summaries are only a SUM, for variables like discharge does not make sense to use these outputs..
## This script pulls Heidelberg obs data instead of SWMP data from the NERR database
## Depending on the version of SWAT+, you need to ensure the headers are correct
## Different from read channel daily 6 bc of directories it reads/saves to


ReadChannel_daily2<-function(nyrs,baseline_data,scenario_dir){



xlib = c("readtext","dplyr","splitstackshape","stringr","rlang","reshape2","tidyr","tictoc","here","rstudioapi","rapportools","lfstat","lubridate","ggplot2")
lapply(xlib, require, character.only=T) ; rm(xlib)



########################## Berlin Rd channel no ########################################
BR<-" 46"
WM<-"  1"

grab_hru<-paste0(c(BR,WM),collapse="|")


#########################################################################################

headers<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"areaha",	"precipha.m",	"evapha.m",	
"seepha.m",	"flo_storm.3.s",	"sed_stormtons",	"orgn_storkgN",	"sedp_storkgP",	"no3_storkgN",	"solp_storkgP",
"chla_storkg",	"nh3_storkgN",	"no2_storkgN",	"cbod_storkg",	"dox_storkg",	"san_stortons",	"sil_stortons",	"cla_stortons",	"sag_stortons",
"lag_stortons",	"grv_stortons",	"null1",	"flo_inm.3.s",	"sed_inmtons",	"orgn_inkgN",	"sedp_inkgP",	"no3_inkgN",
"solp_inkgP",	"chla_inkg",	"nh3_inkgN",	"no2_inkgN",	"cbod_inkg",	"dox_inkg",	"san_intons",	"sil_intons",	"cla_intons",
"sag_intons",	"lag_intons",	"grv_intons",	"null",	"flo_outm.3.s",	"sed_outmtons",	"orgn_outkgN",	"sedp_outkgP",	"no3_outkgN",
"solp_outkgP",	"chla_outkg",	"nh3_outkgN",	"no2_outkgN",	"cbod_outkg",	"dox_outkg",	"san_outtons",	"sil_outtons",	"cla_outtons",
"sag_outtons",	"lag_outtons",	"grv_outtons",	"null2","water_tempdegC")#"null3","null4","null5","null6","null7")



########################################################################################
######################### read in observed data  #######################################
########################################################################################
#currently just pull 'Date' column, could use grepl so wold find column that says 'date'

#This assumes the layering of text in out is 
#scenario directory
#Scenario (contains folders leading to scenarios and observed)
#text in out

#setwd('../../..')

#Obs<-read.csv("./Observed/HeidelbergData/obsdata_HDBRG.csv") #observed data
#var_lookup<-read.csv("./Observed/HeidelbergData/var_lookup.csv") 

#In ReadHeidelbergData3, I remove flow data from Heidelberg and add USGS flow data

#Obs$date<-as.Date(Obs$date,format="%Y-%m-%d")

################ find number of days in simultion to read all lines in file ####


#nday<-sum(366*leap_year(nyrs))+sum(365*!leap_year(nyrs))
nyrs<-length(nyrs)
nhru<-375 #375 channels

nrowHRU<- nyrs*nhru 


################################################################################
################## Read in channel output ######################################
################################################################################
#setwd(ProcessedOutput_dir)

##### Check if processed channel data already exists

  
  print("reading from channel output")

setwd(scenario_dir)
tmp <- file('channel_sd_yr.txt')
open(tmp, "r") #read

#read past headerlines
readLines(tmp, n = 3) 



###### read in simulated data columns #########

#this takes ~23-32 (?) minutes
tic("reading daily data")

BR_data<-c()
WM_data<-c()

for (i in c(1:nrowHRU)){
  
  add_data<-readLines(tmp, n=1)
  
  if(grepl(substr(add_data,38,40),BR)){
    
    BR_data<-rbind(BR_data,add_data)
    
  }
  
  
  if(grepl(substr(add_data,38,40),WM)){
    
    WM_data<-rbind(WM_data,add_data)
    
  }
  

  
}


close(tmp)
BR_data<-strsplit(BR_data,split=" ")
BR_data<-lapply(BR_data, function(z){ z[z != ""]}) 
BR_data<-data.frame(do.call(rbind, BR_data)) #unlist
colnames(BR_data)<-headers

WM_data<-strsplit(WM_data,split=" ")
WM_data<-lapply(WM_data, function(z){ z[z != ""]}) 
WM_data<-data.frame(do.call(rbind, WM_data)) #unlist
colnames(WM_data)<-headers


#DF$date<-as.Date(paste(DF$mon,DF$day,DF$yr,sep="/"), format="%m/%d/%Y")              # add date column
BR_data[,c(1:6,8:(ncol(BR_data)-1))]<-as.numeric(unlist(BR_data[,c(1:6,8:(ncol(BR_data)-1))]))    
WM_data[,c(1:6,8:(ncol(WM_data)-1))]<-as.numeric(unlist(WM_data[,c(1:6,8:(ncol(WM_data)-1))]))   # convert to numerics

toc()



################ Summarize outputs and compare to baseline ############################################################
baseline_data$scenario[baseline_data$variable=="discharge_cms"]<-mean(BR_data$flo_outm.3.s,na.rm=T)
baseline_data$scenario[baseline_data$variable=="solp_kg"]<-sum(BR_data$solp_outkgP,na.rm=T)
baseline_data$scenario[baseline_data$variable=="sedp_kg"]<-sum(BR_data$sedp_outkgP,na.rm=T)
baseline_data$scenario[baseline_data$variable=="sediment_kg"]<-sum(BR_data$sed_outmtons,na.rm=T)
baseline_data$scenario[baseline_data$variable=="totp_kg"]<-sum(BR_data$solp_outkgP + BR_data$sedp_outkgP,na.rm=T)

#scen_flow_5<-quantile(DF$flo_outm.3.s, c(.25)) 
#scen_flow_95<-quantile(DF$flo_outm.3.s, c(.95)) 

baseline_data$change<-NA
baseline_data$change<-(baseline_data$scenario-baseline_data$baseline) *100 / baseline_data$baseline

average_change_plot<-ggplot(baseline_data,aes(x=variable,y=change))+geom_bar(stat = 'identity')+ylab("Change from baseline (%)")+xlab("")+theme_bw()

return(average_change_plot)

#setwd(ProcessedOutput_dir)
#write.csv(DF,"ProcessedChannel.csv",row.names=F)


################### add simulated columns of concentrations, TP loads ################################################

#have to use grepl to grab bc depending on if file is read from 
#file or read from a table the flo_out variable is labeled differently

#currently there's days with 0 flow, replacing Inf with NA

#DF$solpconc_outmgl <- (DF$solp_outkgP/DF[,grepl("flo_out",colnames(DF))])*(10^6/(86400*1000))
#DF$solpconc_outmgl[DF$solpconc_outmgl == Inf] <- NA

#DF$no3conc_outmgl <- (DF$no3_outkgN/DF[,grepl("flo_out",colnames(DF))])*(10^6/(86400*1000))
#DF$no3conc_outmgl[DF$no3conc_outmgl == Inf] <- NA

#DF$totp_outkgP <- DF$solp_outkgP + DF$sedp_outkgP 

#DF$totn_outkgN <- DF$orgn_outkgN + DF$no2_outkgN + DF$no3_outkgN + DF$nh3_outkgN

#DF$no23_outkgN <- DF$no2_outkgN + DF$no3_outkgN 


#DF$totpconc_outmgl <- (DF$totp_outkgP/DF[,grepl("flo_out",colnames(DF))])*(10^6/(86400*1000))
#DF$totpconc_outmgl[DF$totpconc_outmgl == Inf] <- NA

#DF$totnconc_outmgl <- (DF$totn_outkgN/DF[,grepl("flo_out",colnames(DF))])*(10^6/(86400*1000))
#DF$totnconc_outmgl [DF$totnconc_outmgl  == Inf] <- NA

#DF$no23conc_outmgl <- (DF$no23_outkgN/DF[,grepl("flo_out",colnames(DF))])*(10^6/(86400*1000))
#DF$no23conc_outmgl [DF$no23conc_outmgl  == Inf] <- NA

#DF$sedconc_outmgl <- (DF$sed_outmtons/DF[,grepl("flo_out",colnames(DF))])*(10^9/(86400*1000))
#DF$sedconc_outmgl [DF$sedconc_outmgl  == Inf] <- NA


###################################################################################################
###################### Combine simulated and observed data ########################################
###################################################################################################

#combine channel 47 with observed Berlin Rd data
#Use USGS discharge for 10 years and Heidelberg Observed data for the 5 year period


#pull channel data

#BR_data<-DF[DF$gis_id==BR|DF$gis_id==WM ,]
#BR_data$gis_id<-as.character(BR_data$gis_id)
#replace channel no with BR and WM identifiers

#BR_data$gis_id[BR_data$gis_id == as.character(BR)]<-"BR"
#BR_data$gis_id[BR_data$gis_id == as.character(WM)]<-"WM"



#format observed data
#Obs<-reshape2::dcast(Obs,date + station ~variable)

#var_lookup<-var_lookup[!is.empty(var_lookup$var_sim),] #remove observed variables with no observed equivalent
#Obs<-Obs[,c(1:2,grep(paste(var_lookup$var_obs, collapse="|"),colnames(Obs)))] #remove observed data columns with no simulated equivalent
#names(Obs)[!is.na( match(colnames(Obs),var_lookup$var_obs) )] <- as.character(var_lookup$var_sim[na.omit( match(colnames(Obs),var_lookup$var_obs) )])   #replace observed data names with simulated names
#colnames(Obs)[3:length(colnames(Obs))]<-paste0("obs_",colnames(Obs)[3:length(colnames(Obs))]) #add obs to column names to distinguish from simulated values


#BR_data<-left_join(BR_data,Obs,by=c("date","gis_id"="station")) #combine observed and simulated data


#Grab precipitation data
#pcp<-data.frame(DF$date[DF$gis_id==1], (DF[DF$gis_id==1,grepl("precip",colnames(DF))]*1000/DF$areaha[DF$gis_id==1]))
#colnames(pcp)<-c("date","pcp_mm")


#write.csv(BR_data,"SimObs_channelDaily.csv",row.names=F)
#write.csv(pcp,"dailypcp.csv",row.names=F)






}