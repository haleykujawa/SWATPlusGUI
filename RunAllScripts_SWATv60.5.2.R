RunAllScripts_SWATv60.5.2<-function(scenario_dir){


  setwd(scenario_dir)
  
  
  system('SWATPlus_60.5.5.exe') #run executable -- this one is Haley's w/ out septics
  
  
  print("Model run complete!")
  
  
  ####### Read HRU losses #########################################################
  
  headers<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"sedyld_tha","sedorgn_kgha","sedorgp_kgha",
             "surqno3_kgha","lat3no3_kgha","surqsolp_kgha","usle_tons","sedmin","tileno3","lchlabp","tilelabp","satexn")
  
  ################# Read in HRU lookup ###########################################
  #setwd(scenario_dir)
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
  
  
  return(DF_aghru)
  
  #write.table(DF_aghru,"hruLoss_summary.csv",row.names=F,col.names=F,sep="," )
  
  
  
}     



