# Script to copy a new baseline into all folders and run to recalculate all baseline data
rm(list=ls())

library(here)
library(tidyverse)
library(tictoc)

###### Baseline ########
scenario<-'baseline'

###### Write new hru lookup ############
############### READ IN HRU-DATA ##########################
# read in old HRU data file because has the old naming conventions which I use as a reference rather than changing the code
setwd(here('Baseline'))
tmp <- file('hru-data.hru')
open(tmp, "r") #read

#read past headerline and save to rewrite the file
topOfFile<-readLines(tmp, n = 2) 

#read file 
data1<-readLines(tmp, n = -1)

close(tmp)

#read by spacing 
id<-substr(data1, 1,8)
name<-substr(data1, 9,17)
topo<-substr(data1, 18,44)
hydro<-substr(data1, 45,62)
soil<-substr(data1, 63,80)
lu_mgt<-substr(data1, 82,98)
soil_plant_init<-substr(data1, 100,116)
surf_stor<-substr(data1, 118,134)
snow<-substr(data1, 136,152)
field<-substr(data1, 154,170)

hru_data<-data.frame(id,name,topo,hydro,soil,lu_mgt,soil_plant_init,surf_stor,snow,field)

rm(id,name,topo,hydro,soil,lu_mgt,soil_plant_init,surf_stor,snow,field)

################## READ IN SOILS DATA ###############################

SSURGO<-read.table("SSURGO.txt",header=T,sep="\t")
# currently I only need hydr grp and muid
SSURGO<-SSURGO[,c(2,6)]
colnames(hru_data)[5]<-"muid"
hru_data$muid<-as.integer(hru_data$muid)

hru_data<-left_join(hru_data,SSURGO,by="muid")


################# READ IN HRU AREA ##################################

##Pull area of each hru from hru.con
tmp <- file('hru.con')
open(tmp, "r") #read
readLines(tmp, n = 2) 

HRUarea<-readLines(tmp, n = -1) 
HRUarea<-tibble(substr(HRUarea,11,17),substr(HRUarea,37,50)) # previously 44
colnames(HRUarea)<-c("name","area_ha")

close(tmp)

#remove spaces so the name characters match
hru_data$name<-str_replace_all(hru_data$name, fixed(" "), "")
HRUarea$name<-str_replace_all(HRUarea$name, fixed(" "), "")

hru_data<-left_join(hru_data,HRUarea,by="name")
hru_data$area_ha<-as.numeric(hru_data$area_ha)

#total cropland area
cropland_index<-grepl(paste(c('10','20','30','40','50','63','73','11','21','31','41','51'),collapse='|'),hru_data$lu_mgt)
cropland_area<-sum(hru_data$area_ha[  cropland_index])
print(paste("cropland area is", cropland_area, "ha")) #testing
################# READ IN HRU SLOPES ###############################

##Pull slope of each hru from topography.hyd
tmp <- file('topography.hyd')
open(tmp, "r") #read
readLines(tmp, n = 2) 

HRUslp<-readLines(tmp, n = -1) 
HRUslp<-tibble(substr(HRUslp,1,11),substr(HRUslp,24,32))
colnames(HRUslp)<-c("topo","slp")


hru_data$topo <- lapply(hru_data$topo, str_trim) # trim white space 
hru_data$topo<-unlist(hru_data$topo)
hru_data<-left_join(hru_data,HRUslp,by="topo")
hru_data$slp<-as.numeric(hru_data$slp)

close(tmp)

############## Create index for baseline grww, buffers ##############
hru_data$base_mgt <- str_extract(hru_data$lu_mgt, ".*_")


hru_data$base_mgt <- str_remove(  hru_data$base_mgt, "_.*")  # Remove characters after the first instance
hru_data$base_mgt[is.na(hru_data$base_mgt)]<-hru_data$lu_mgt[is.na(hru_data$base_mgt)] # everywhere where no _ exists, pull the whole row
hru_data$base_mgt <- str_remove_all(  hru_data$base_mgt, "\\s+") # remove spaces

hru_data$lu_mgt <- str_remove_all(  hru_data$lu_mgt, "\\s+") # remove spaces from lu so I can find where added 1,2,3 exists after the 2nd space
# since the base management is only 2 values. Will get respaced before writing the hru file


hru_data$grww[grepl(paste(c('GL'),collapse='|'),hru_data$lu_mgt)]<-'GL'
hru_data$grww[grepl(paste(c('GM'),collapse='|'),hru_data$lu_mgt)]<-'GM'
hru_data$grww[grepl(paste(c('GH'),collapse='|'),hru_data$lu_mgt)]<-'GH'

hru_data$buffers[grepl('B',hru_data$lu_mgt)]<-1

hru_data$tile[grepl('_t',hru_data$lu_mgt)]<-1

# set lu mgt where cropland index is true to only the base management--otherwise addition of numbering for scenarios won't work
# this should get moved to when I'm building out the scenarios in RunAllScripts... 10/19/23
# hru_data$lu_mgt[cropland_index]<-hru_data$base_mgt[cropland_index]

hru_data$id<-as.numeric(hru_data$id)

####### write hru lookup ########

write.csv(hru_data,'hru_lookup.csv',row.names=F)





####### Copy over baseline files and run model ####################

Baseline_files<-list.files(here("Baseline"))

file.copy(paste0(here("Baseline"),"/",Baseline_files),
          here("Scenarios","hist"),overwrite = T)

# recalculate and write avg_baseline file in scenario folder
setwd(here("Scenarios","hist"))

# system('SWATPlus_60.5.5.exe')

headers_ch<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"areaha",	"precipha.m",	"evapha.m",	
              "seepha.m",	"flo_storm.3.s",	"sed_stormtons",	"orgn_storkgN",	"sedp_storkgP",	"no3_storkgN",	"solp_storkgP",
              "chla_storkg",	"nh3_storkgN",	"no2_storkgN",	"cbod_storkg",	"dox_storkg",	"san_stortons",	"sil_stortons",	"cla_stortons",	"sag_stortons",
              "lag_stortons",	"grv_stortons",	"null1", "setl_stor",	"setlp_stor",	"flo_inm.3.s",	"sed_inmtons",	"orgn_inkgN",	"sedp_inkgP",	"no3_inkgN",
              "solp_inkgP",	"chla_inkg",	"nh3_inkgN",	"no2_inkgN",	"cbod_inkg",	"dox_inkg",	"san_intons",	"sil_intons",	"cla_intons",
              "sag_intons",	"lag_intons",	"grv_intons",	"null",	 "setl_in",	"setlp_in","flo_outm.3.s",	"sed_outmtons",	"orgn_outkgN",	"sedp_outkgP",	"no3_outkgN",
              "solp_outkgP",	"chla_outkg",	"nh3_outkgN",	"no2_outkgN",	"cbod_outkg",	"dox_outkg",	"san_outtons",	"sil_outtons",	"cla_outtons",
              "sag_outtons",	"lag_outtons",	"grv_outtons",	"null2", "setl_out",	"setlp_out", "water_tempdegC")

headers_hru<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"sedyld_tha","sedorgn_kgha","sedorgp_kgha",
               "surqno3_kgha","lat3no3_kgha","surqsolp_kgha","usle_tons","sedmin","tileno3","lchlabp","tilelabp","satexn")

headers_crop<-c(  "jday",   "mon",   "day",    "yr",    "unit", "PLANTNM", "MASS", "C", "N", "P")

tmp <- file('channel_sd_yr.txt')
open(tmp, "r") #read

#read past headerlines
readLines(tmp, n = 3) 

DF<-readLines(tmp,n=-1)

close(tmp)
DF<-strsplit(DF,split=" ") #split based on spacing
DF<-lapply(DF, function(z){ z[z != ""]}) # remove empty spaces
DF<-data.frame(do.call(rbind, DF)) #unlist
colnames(DF)<-headers_ch

# Berlin Rd 
DF<-DF%>%
  filter(gis_id=="46")

DF[,c(1:6,8:(ncol(DF)-1))]<-DF[,c(1:6,8:(ncol(DF)-1))]%>%
  unlist()%>%
  as.numeric()

baseline_data<-data.frame(matrix(nrow=4,ncol=2))
colnames(baseline_data)<-c("variable","baseline")
baseline_data$variable<-c("discharge_cms", "solp_kg", "sediment_kg", "totp_kg")

baseline_data$baseline[baseline_data$variable=="discharge_cms"]<-mean(DF$flo_outm.3.s,na.rm=T)
baseline_data$baseline[baseline_data$variable=="solp_kg"]<-sum(DF$solp_outkgP,na.rm=T)
# baseline_data$baseline[baseline_data$variable=="sedp_kg"]<-sum(DF$sedp_outkgP,na.rm=T)
baseline_data$baseline[baseline_data$variable=="sediment_kg"]<-sum(DF$sed_outmtons,na.rm=T)
baseline_data$baseline[baseline_data$variable=="totp_kg"]<-sum(DF$solp_outkgP + DF$sedp_outkgP,na.rm=T)

write.csv(baseline_data,"baseline_data_avg.csv",row.names=F)

##### Management rotations / tile plot ######################

tmp <- file('hru_ls_yr.txt')
open(tmp, "r") #read

#read past headerlines
readLines(tmp, n = 3) 



###### read in simulated data columns #########
# Calculating loss from each of the fields

data<-readLines(tmp, n = -1)
close(tmp)
DF<-strsplit(data,split=" ")
DF<-lapply(DF, function(z){ z[z != ""]})
DF<-data.frame(do.call(rbind, DF)) #unlist
colnames(DF)<-headers_hru


# DF$date<-as.Date(paste(DF$mon,DF$day,DF$yr,sep="/"), format="%m/%d/%Y")              # add date column
DF[,c(1:6,8:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:6,8:(ncol(DF)-1))]))           # convert to numerics

DF_aghru<-left_join(DF,hru_data,by=c("name"))

# index all mgt scenarios
DF_aghru<-DF_aghru %>%
  filter(base_mgt != 'frsd' & base_mgt != 'urml' & base_mgt != 'past') %>%
  mutate(totp=sedorgp_kgha+surqsolp_kgha+sedmin) %>%
  select("base_mgt", "name" ,"sedyld_tha","tilelabp","totp","surqsolp_kgha","hyd_grp", "slp","tile") %>%
  mutate(scenario=scenario)

write.csv(DF_aghru,"hru_baseline.csv",row.names=F)

################## Read and write baseline yield ########################################


tmp <- file('crop_yld_aa.txt')
open(tmp, "r") #read

#read past headerlines
readLines(tmp, n = 5) 



###### read in simulated data columns #########

#this takes ~23-32 (?) minutes
tic("reading yearly sum data")
data<-readLines(tmp, n = -1)  
DF<-strsplit(data,split=" ")
DF<-lapply(DF, function(z){ z[z != ""]}) 
DF<-data.frame(do.call(rbind, DF)) #unlist
colnames(DF)<-headers_crop



DF[,c(1:5,7:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:5,7:(ncol(DF)-1))]))           # convert to numerics

toc()

# combine with lookup table
DF_lookup<-left_join(DF,hru_data, by=c("unit"="id"))

DF_lookup<-DF_lookup %>% 
  # mutate(mgt=replace(mgt, grepl(paste0(c("SC_FT","CS_FT"),collapse='|'),lu_mgt), "CS_FT")) %>% 
  # mutate(mgt=replace(mgt, grepl(paste0(c("SC_RT","CS_RT"),collapse='|'),lu_mgt), "CS_RT")) %>% 
  # mutate(mgt=replace(mgt, grepl(paste0(c("SC_RotT","CS_RotT"),collapse='|'),lu_mgt), "CS_RotT")) %>% 
  # mutate(mgt=replace(mgt, (grepl(paste0(c("SC_NT","CS_NT"),collapse='|'),lu_mgt) & !grepl(paste0(c("SC_NTcc","CS_NTcc"),collapse='|'),lu_mgt)), "CS_NT")) %>% 
  # mutate(mgt=replace(mgt, grepl(paste0(c("SC_NTcc","CS_NTcc"),collapse='|'),lu_mgt), "CS_NTcc")) %>% 
  # mutate(mgt=replace(mgt, grepl("CSWS",lu_mgt), "CSWS")) %>% 
  # mutate(mgt=replace(mgt, grepl("CSWcc",lu_mgt), "CSWcc")) %>% 
  filter(!(PLANTNM %in% c('rye','frsd','urbn_cool','past')))

ggplot(DF_lookup,aes(x='1',y=MASS,fill=tile))+geom_boxplot()+facet_grid(PLANTNM ~ base_mgt)+ylab("plant mass (kg/ha)")+xlab("")
ggsave("yield_by_mgt.png",last_plot(),height=200,width=400, units="mm")

#convert mass to bu / acre
DF_lookup$MASS[DF_lookup$PLANTNM=='corn']<-DF_lookup$MASS[DF_lookup$PLANTNM=='corn']/62.77
DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']<-DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']/67.25
DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']<-DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']/67.25

DF_lookup$scenario<-'baseline'

write.csv(DF_lookup,file="crop_yield_baseline.csv",row.names=F)

##### Climate scenario ######

# These I manually changed and/or put into the folder
# need separate baseline folder for climate change bc pcp and tmp can be changed every time the app is opened.
Baseline_files<-Baseline_files[!Baseline_files %in% c('time.sim','print.prt','owcmet_pcp.pcp','owcmet_tmp.tmp','weather-sta.cli')]

file.copy(paste0(here("Baseline"),"/",Baseline_files),
          here("Scenarios","userClimScen"),overwrite = T)

file.copy(paste0(here("Baseline"),"/",Baseline_files),
          here("Scenarios","userClimScen_baseline"),overwrite = T)

# recalculate and write avg_baseline file in scenario folder
setwd(here("Scenarios","userClimScen_baseline"))

# system('SWATPlus_60.5.5.exe')

tmp <- file('channel_sd_yr.txt')
open(tmp, "r") #read

#read past headerlines
readLines(tmp, n = 3) 

DF<-readLines(tmp,n=-1)

close(tmp)
DF<-strsplit(DF,split=" ") #split based on spacing
DF<-lapply(DF, function(z){ z[z != ""]}) # remove empty spaces
DF<-data.frame(do.call(rbind, DF)) #unlist
colnames(DF)<-headers_ch

# Berlin Rd 
DF<-DF%>%
  filter(gis_id=="46")

DF[,c(1:6,8:(ncol(DF)-1))]<-DF[,c(1:6,8:(ncol(DF)-1))]%>%
  unlist()%>%
  as.numeric()

baseline_data<-data.frame(matrix(nrow=4,ncol=2))
colnames(baseline_data)<-c("variable","baseline")
baseline_data$variable<-c("discharge_cms", "solp_kg", "sediment_kg", "totp_kg")

baseline_data$baseline[baseline_data$variable=="discharge_cms"]<-mean(DF$flo_outm.3.s,na.rm=T)
baseline_data$baseline[baseline_data$variable=="solp_kg"]<-sum(DF$solp_outkgP,na.rm=T)
# baseline_data$baseline[baseline_data$variable=="sedp_kg"]<-sum(DF$sedp_outkgP,na.rm=T)
baseline_data$baseline[baseline_data$variable=="sediment_kg"]<-sum(DF$sed_outmtons,na.rm=T)
baseline_data$baseline[baseline_data$variable=="totp_kg"]<-sum(DF$solp_outkgP + DF$sedp_outkgP,na.rm=T)

setwd(here('Scenarios','userClimScen'))
write.csv(baseline_data,"baseline_data_avg.csv",row.names=F)

##### Management rotations / tile plot ######################
# I manually copied over the hru lookup file from the scenarios folder since they are the same
# If changing management for the baseline would need to rewrite this file
setwd(here('Scenarios','userClimScen_baseline'))


tmp <- file('hru_ls_yr.txt')
open(tmp, "r") #read

#read past headerlines
readLines(tmp, n = 3) 



###### read in simulated data columns #########
data<-readLines(tmp, n = -1)  
close(tmp)
DF<-strsplit(data,split=" ")
DF<-lapply(DF, function(z){ z[z != ""]}) 
DF<-data.frame(do.call(rbind, DF)) #unlist
colnames(DF)<-headers_hru


# DF$date<-as.Date(paste(DF$mon,DF$day,DF$yr,sep="/"), format="%m/%d/%Y")              # add date column
DF[,c(1:6,8:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:6,8:(ncol(DF)-1))]))           # convert to numerics

DF_aghru<-left_join(DF,hru_data,by=c("name"))


# index all mgt scenarios
DF_aghru<-DF_aghru %>% 
  mutate(totp=sedorgp_kgha+surqsolp_kgha+sedmin) %>% 
  select("base_mgt", "name" ,"sedyld_tha","tilelabp","totp","surqsolp_kgha","hyd_grp", "slp","tile") %>% 
  mutate(scenario=scenario)

# Could use gather instead of melt
# DF_aghru<-melt(DF_aghru,id=c('hyd_grp','mgt','name','tile','slp','scenario'))

setwd(here('Scenarios','userClimScen'))
write.csv(DF_aghru,"hru_baseline.csv",row.names=F)

################## Read and write baseline yield ########################################
setwd(here('Scenarios','userClimScen_baseline'))

tmp <- file('crop_yld_aa.txt')
open(tmp, "r") #read

#read past headerlines
readLines(tmp, n = 5) 



###### read in simulated data columns #########

#this takes ~23-32 (?) minutes
tic("reading yearly sum data")
data<-readLines(tmp, n = -1)  
DF<-strsplit(data,split=" ")
DF<-lapply(DF, function(z){ z[z != ""]}) 
DF<-data.frame(do.call(rbind, DF)) #unlist
colnames(DF)<-headers_crop



DF[,c(1:5,7:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:5,7:(ncol(DF)-1))]))           # convert to numerics

toc()

################# read in lookup table ######################################
DF_lookup<-left_join(DF,hru_data, by=c("unit"="id"))

DF_lookup<-DF_lookup %>% 
  filter(!(PLANTNM %in% c('rye','frsd','urbn_cool','past')))


ggplot(DF_lookup,aes(x='1',y=MASS,fill=tile))+geom_boxplot()+facet_grid(PLANTNM ~ base_mgt)+ylab("plant mass (kg/ha)")+xlab("")
ggsave("yield_by_mgt.png",last_plot(),height=200,width=400, units="mm")

#convert mass to bu / acre
DF_lookup$MASS[DF_lookup$PLANTNM=='corn']<-DF_lookup$MASS[DF_lookup$PLANTNM=='corn']/62.77
DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']<-DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']/67.25
DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']<-DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']/67.25

DF_lookup$scenario<-'baseline'

setwd(here('Scenarios','userClimScen'))
write.csv(DF_lookup,file="crop_yield_baseline.csv",row.names=F)

# copy baseline file to scenario folder