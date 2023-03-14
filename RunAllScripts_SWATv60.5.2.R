RunAllScripts_SWATv60.5.2<-function(scenario_dir,ClimateOption,ClimateModels,ClimateFile){

#if going at add changing management into this file, maybe also add copying over the baseline directory here
  
  if (ClimateOption=="nochange"){
    setwd(scenario_dir)
    system('SWATPlus_60.5.5.exe') #run executable
  }
  
  #Climate model runs
  if (ClimateOption=="climmod"){
    
    
    for (climatemodel in ClimateModels){
    
    pcp_file<-file.path(scenario_dir,'climate',climatemodel,'future','owcmet_pcp.pcp')  
    tmp_file<-file.path(scenario_dir,'climate',climatemodel,'future','owcmet_tmp.tmp')
    test_file<-file.path(scenario_dir,'climate',climatemodel,'future','copytest.txt')
    
    file.copy(from=pcp_file,to=scenario_dir,overwrite=T)
    file.copy(from=tmp_file,to=scenario_dir,overwrite=T)
    file.copy(from=test_file,to=scenario_dir,overwrite=T)
    
    setwd(scenario_dir)
    system('SWATPlus_60.5.5.exe') #run executable
    
      
      
    }
  
    print("setting up climate data runs")
    
  }
  
  #Extend data beyond 2020 and write to scenario folder
  if (ClimateOption=="extended"){
    
    
    
    
    
    system('SWATPlus_60.5.5.exe') #run executable 
  
  }
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  setwd(scenario_dir)
  
  
  ####### Read HRU losses #########################################################
  
  headers<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"sedyld_tha","sedorgn_kgha","sedorgp_kgha",
             "surqno3_kgha","lat3no3_kgha","surqsolp_kgha","usle_tons","sedmin","tileno3","lchlabp","tilelabp","satexn")
  
  ################# Read in HRU lookup ###########################################

  lookup<-read.csv("hru_lookup.csv")
  
  
  ################################################################################
  ################## Read in hru output ##########################################
  ################################################################################
  
  
  
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
  colnames(DF)<-headers
  
  
  DF$date<-as.Date(paste(DF$mon,DF$day,DF$yr,sep="/"), format="%m/%d/%Y")              # add date column
  DF[,c(1:6,8:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:6,8:(ncol(DF)-1))]))           # convert to numerics
  

  
  
  DF_aghru<-left_join(DF,lookup,by=c("name"))
  DF_aghru<-DF_aghru[grepl(paste0(c("CS","SC","CSW"), collapse="|"),DF_aghru$lu_mgt),]
  
  #remove all variables except year and output
  DF_aghru<-select(DF_aghru, -c("yr","jday","mon","day","unit","gis_id","name","date","id",
                                "topo","hydro","soil","lu_mgt","soil_plant_init","surf_stor","snow","field"))
  
  #Sum loss from all years and then come up with average annual loss
  DF_aghru<-colMeans(DF_aghru,na.rm=T)
  DF_aghru<-data.frame(as.list(DF_aghru))
  DF_aghru<-reshape2::melt(DF_aghru)
  
  
  return(list(DF_aghru,print("testing")))
  
  #write.table(DF_aghru,"hruLoss_summary.csv",row.names=F,col.names=F,sep="," )
  
  
  
}     



