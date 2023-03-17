RunAllScripts_SWATv60.5.2<-function(scenario_dir,SelectClimate){

#if going at add changing management into this file, maybe also add copying over the baseline directory here

### ADD CODE BELOW ####
  # copy over orginal files from baseline 
  # make changes with ChangeSWATInputs
  # Add error if SelectClimate=empty
  
### mgt files to copy into each folder ####
mgt_files<-c('hru-data.hru','hyd-sed-lte.cha','hydrology.hyd')
myplots<-list()
  
### functions ######
  spaceOutput<-function(data,nspaces){
    
    newData<-paste0(str_dup(" ",(nspaces-nchar(data))),data)
    return(newData)
    
  }
  
  spaceOutput_spacesecond<-function(data,nspaces){
    
    newData<-paste0(data,str_dup(" ",(nspaces-nchar(data))))
    return(newData)
    
  }
  

  # Baseline historical climate
  # Combine with below since now they do all the same thing
  # if (any(grep("hist",SelectClimate))){
    
    # file.copy(from = file.path(paste0(scenario_dir,"/", mgt_files)),   # Copy files
              # to = file.path(paste0(scenario_dir,"/hist", mgt_files)))
    # 
    # setwd(paste0(scenario_dir,"/hist"))
    # system('SWATPlus_60.5.5.exe') #run executable
  # }
  
  ######### Run all selected climate options ################################################
  if (any(grep(paste(c("CNRM","MIROC5","IPSL-CM5A-MR","hist"),collapse="|"),SelectClimate))){
    
    
    # for (climatemodel in ClimateModels){
    
    # Maybe instead of copying climate to output folder I copy all the scenario folder to where the data is (minus the pcp and tmp folders) and set all the models to run
    # pcp_file<-file.path(scenario_dir,'climate',climatemodel,'future','owcmet_pcp.pcp')  
    # tmp_file<-file.path(scenario_dir,'climate',climatemodel,'future','owcmet_tmp.tmp')
    # test_file<-file.path(scenario_dir,'climate',climatemodel,'future','copytest.txt')
      
    # file.copy(from=scenario_dir,to=scenario_dir,overwrite=T)
    # file.copy(from=tmp_file,to=scenario_dir,overwrite=T)
    # file.copy(from=test_file,to=scenario_dir,overwrite=T)
      
    # Only copy files that the app makes changes to --e.g., hru-data.hru, hyd-sed-lte.cha, hydrology.hyd are currently the only ones with changes
    # continue to add here as the app functionality expands
    # currently going to do this as a loop but consider using mcapply to run multiple models in parallel
    # https://bookdown.org/rdpeng/rprogdatascience/parallel-computation.html#the-parallel-package
    #potentially use future_lapply or reference SWATPlusR because they have an option for parallel computing.
    
    #remove historical option from climate runs
    # ClimateModels<-SelectClimate%>%
      # setdiff(c("hist"))
  
    
    # s<-system.time({
    
                # lapply(SelectClimate,function(climatemodel){
      # Change to for loop because plotting function not working, if wanting to get working in parallel may want to use lapply
      for (climatemodel in SelectClimate){
      
                  # Instead of running the baseline historical with the new mgt--only compare with baseline climate run (1980-1999) with historical management (2013-2020)
                  # In Likely Adoption project we got weird % changes if we compared LA historical with LA future. The absolute change was smaller than the baseline, but the % change would be larger
                  
                              # file.copy(from = file.path(paste0(scenario_dir,"/", my_files)),   # Copy files
                              # to = file.path(paste0(scenario_dir,'/climate','/',climatemodel,'/historical',"/", my_files)))
      
                              file.copy(from = file.path(paste0(scenario_dir,"/", mgt_files)),   # Copy files
                              to = file.path(paste0(scenario_dir,'/',climatemodel,"/", mgt_files)))
                              
                              print(paste0("running ",climatemodel))
                              
                              #These would also need to be combined as one function to have every single run running in parallel. Keep for now.
                              # setwd(paste0(scenario_dir,'/climate','/',climatemodel,'/historical'))
                              # system('SWATPlus_60.5.5.exe',ignore.stdout = T,ignore.stderr = T) #run executable
                              
                              setwd(paste0(scenario_dir,'/',climatemodel))
                              system('SWATPlus_60.5.5.exe',ignore.stdout = F,ignore.stderr = F) #run executable
                              
                              ### Read in channel data and compare with baseline ####
                              
                              headers<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"areaha",	"precipha.m",	"evapha.m",	
                                         "seepha.m",	"flo_storm.3.s",	"sed_stormtons",	"orgn_storkgN",	"sedp_storkgP",	"no3_storkgN",	"solp_storkgP",
                                         "chla_storkg",	"nh3_storkgN",	"no2_storkgN",	"cbod_storkg",	"dox_storkg",	"san_stortons",	"sil_stortons",	"cla_stortons",	"sag_stortons",
                                         "lag_stortons",	"grv_stortons",	"null1", "setl_stor",	"setlp_stor",	"flo_inm.3.s",	"sed_inmtons",	"orgn_inkgN",	"sedp_inkgP",	"no3_inkgN",
                                         "solp_inkgP",	"chla_inkg",	"nh3_inkgN",	"no2_inkgN",	"cbod_inkg",	"dox_inkg",	"san_intons",	"sil_intons",	"cla_intons",
                                         "sag_intons",	"lag_intons",	"grv_intons",	"null",	 "setl_in",	"setlp_in","flo_outm.3.s",	"sed_outmtons",	"orgn_outkgN",	"sedp_outkgP",	"no3_outkgN",
                                         "solp_outkgP",	"chla_outkg",	"nh3_outkgN",	"no2_outkgN",	"cbod_outkg",	"dox_outkg",	"san_outtons",	"sil_outtons",	"cla_outtons",
                                         "sag_outtons",	"lag_outtons",	"grv_outtons",	"null2", "setl_out",	"setlp_out", "water_tempdegC")#"null3","null4","null5","null6","null7")
                              
                              
                              tmp <- file('channel_sd_yr.txt')
                              open(tmp, "r") #read
                              
                              #read past headerlines
                              readLines(tmp, n = 3) 
                              
                              DF<-readLines(tmp,n=-1)
                              
                              close(tmp)
                              DF<-strsplit(DF,split=" ") #split based on spacing
                              DF<-lapply(DF, function(z){ z[z != ""]}) # remove empty spaces
                              DF<-data.frame(do.call(rbind, DF)) #unlist
                              colnames(DF)<-headers
                              
                              # Berlin Rd 
                              DF<-DF%>%
                                filter(gis_id=="46")
                              
                              DF[,c(1:6,8:(ncol(DF)-1))]<-DF[,c(1:6,8:(ncol(DF)-1))]%>%
                                unlist()%>%
                                as.numeric()
                              
                              #### Read in baseline data #####
                              baseline_data<-read.csv("baseline_data_avg.csv")
                              
                              ################ Summarize outputs and compare to baseline ############################################################
                              baseline_data$scenario[baseline_data$variable=="discharge_cms"]<-mean(DF$flo_outm.3.s,na.rm=T)
                              baseline_data$scenario[baseline_data$variable=="solp_kg"]<-sum(DF$solp_outkgP,na.rm=T)
                              baseline_data$scenario[baseline_data$variable=="sedp_kg"]<-sum(DF$sedp_outkgP,na.rm=T)
                              baseline_data$scenario[baseline_data$variable=="sediment_kg"]<-sum(DF$sed_outmtons,na.rm=T)
                              baseline_data$scenario[baseline_data$variable=="totp_kg"]<-sum(DF$solp_outkgP + DF$sedp_outkgP,na.rm=T)
                              
                              # calculate % difference between baseline and scenario
                              baseline_data$change_per<-(baseline_data$scenario-baseline_data$baseline) *100 / baseline_data$baseline
                              
                              plot1<-ggplot(baseline_data,aes(x=variable,y=change_per))+geom_bar(stat = 'identity')+ylab("Change from baseline (%)")+
                                xlab("")+ 
                                geom_text(size=16,aes(label=round(change_per)), position=position_dodge(width=0.9), vjust=-0.09,colour="black")+
                                theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                                      panel.background = element_blank(),text = element_text(size = 16),
                                      panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
                              
                              myplots[[climatemodel]]<-plot1
                              
                              
     # })
} #)
   

    plot_output<-grid.arrange(grobs = myplots, ncol=2)
    setwd(here('www'))
    ggsave("avg_change_BR.png",plot_output)
      
    }

  
  ################# Extend data beyond 2020 and write to scenario folder ###############################################
  # the code below does not work with the current GUI, there is no option for "extended". Not sure if I'm going to keep.
  if (any(grep("extended",SelectClimate))){
    
    print(pcpFile[[4]])
    
    ##### Read in extended climate data and fill gaps ##############
    # col 1 = date as dd/mm/YY and col 2 as data (pcp = mm, tmp = C)
    # could add option to input units and convert here
    new_pcp<-read.csv(pcpFile[[4]])
    colnames(new_pcp)<-c("date","tmp")
    
    # new_pcp<-read.csv(pcpFile)
    # colnames(new_pcp)<-c("date","tmp")
    
    #add date 03/02/2022 to start if it doesn't already exist
    #pcp data goes until 03/01/2022
    if (new_pcp$date[1] != c("3/2/2022")){
    new_pcp<-rbind(c("3/2/2022",NA),new_pcp)
    }
    
    
    new_pcp$date<-as.Date(new_pcp$date,format='%m/%d/%Y')
    
    # fill missing dates and remove dates before 03/02/2022
    # this dates function isn't working, need to come back and fix this 3/15/23
    # new_pcp<-new_pcp %>%
      # mutate(date = as.Date(date),format="%m/%d/%Y") %>%
      # complete(date = seq.Date(min(date), max(date), by="day")) %>%
      # filter(date > as.Date("2022-03-01"))
    
    # replace empty NA data with -99
    new_pcp$tmp[is.na(new_pcp$tmp)]<-"-99"
    
    ###### format and append data to tmp and pcp ###############
    # pcp = total rainfall per day
    # tmp = daily min and max temp
    # SWAT+ pcp file is year / doy / pcp (mm)
      
    new_pcp$year<-format(new_pcp$date, format="%Y")%>%
      as.character()
    
    new_pcp$doy<- yday(new_pcp$date)     %>%
          as.character()
    
    #make all numbers have five decimal places
     new_pcp$tmp<-as.numeric(new_pcp$tmp) %>%
     sprintf(fmt = '%#.5f') %>%
     as.character() 

    new_pcp$year<-spaceOutput_spacesecond(new_pcp$year,6) 
    new_pcp$doy<-spaceOutput_spacesecond(new_pcp$doy,5) 
    new_pcp$tmp<-spaceOutput(new_pcp$tmp,9)

    # find last date to change time.sim file
    # 15 years of pcp in original SWAT file
    nbyr <- max(as.numeric(new_pcp$year))-min(as.numeric(new_pcp$year))+1+15
    
    day_last<-str_trim(new_pcp$doy[length(new_pcp$doy)])
    year_last<-max(as.numeric(new_pcp$year))
    
    DF<-paste0(new_pcp$year,new_pcp$doy,new_pcp$tmp)
    
    # open file for reading and writing
    file_dir<-file.path(scenario_dir,'owcmet_pcp.pcp')
    tmp<-readLines(file_dir,-1)
    # read first down first two lines of tmp file
    
    #Replace years 
    tmp[3]<-paste0(nbyr,"         0    41.378   -82.508   184.000")
    
    close( file( file_dir, open="w" ) ) 
    sink(file_dir, type=c("output"), append = T)
    write(tmp,file_dir,sep = "\n",append=T)
    write(DF,file_dir,sep = "\n",append=T)
    sink()
    
    # open time.sim and rewrite end of simulation based on tmp and pcp data
    # assume tmp and pcp have same end date -- may have to compare the two and see which is smaller 
    file_dir<-file.path(scenario_dir,'time.sim')
    tmp<-readLines(file_dir,n=-1)
  
    substr(tmp[[3]],24,30)<-spaceOutput(day_last,7)
    substr(tmp[[3]],34,40)<-spaceOutput(year_last,7) 
    
    close( file( file_dir, open="w" ) ) 
    sink(file_dir, type=c("output"), append = T)
    write(tmp,file_dir,sep = "\n",append=T)
    sink()
    
    setwd(scenario_dir)
    system('SWATPlus_60.5.5.exe') #run executable 
  
  }
  
  
 

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  setwd(scenario_dir)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####### Read HRU losses #########################################################
  
  # headers<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"sedyld_tha","sedorgn_kgha","sedorgp_kgha",
             # "surqno3_kgha","lat3no3_kgha","surqsolp_kgha","usle_tons","sedmin","tileno3","lchlabp","tilelabp","satexn")
  
  ################# Read in HRU lookup ###########################################

  # lookup<-read.csv("hru_lookup.csv") #This not longer relevant with changing management. Will have to write an hru lookup for each run.
  
  
  ################## Read in hru output ##########################################
  
  
  
  # tmp <- file('hru_ls_yr.txt')
  # open(tmp, "r") #read
  
  #read past headerlines
  # readLines(tmp, n = 3) 
  
  
  
  ###### read in simulated data columns #########
  
  

  # data<-readLines(tmp, n = -1)  
  # close(tmp)
  # DF<-strsplit(data,split=" ")
  # DF<-lapply(DF, function(z){ z[z != ""]}) 
  # DF<-data.frame(do.call(rbind, DF)) #unlist
  # colnames(DF)<-headers
  
  
  # DF$date<-as.Date(paste(DF$mon,DF$day,DF$yr,sep="/"), format="%m/%d/%Y")              # add date column
  # DF[,c(1:6,8:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:6,8:(ncol(DF)-1))]))           # convert to numerics
  

  
  
  # DF_aghru<-left_join(DF,lookup,by=c("name"))
  # DF_aghru<-DF_aghru[grepl(paste0(c("CS","SC","CSW"), collapse="|"),DF_aghru$lu_mgt),]
  # 
  # #remove all variables except year and output
  # DF_aghru<-select(DF_aghru, -c("yr","jday","mon","day","unit","gis_id","name","date","id",
  #                               "topo","hydro","soil","lu_mgt","soil_plant_init","surf_stor","snow","field"))
  # 
  # #Sum loss from all years and then come up with average annual loss
  # DF_aghru<-colMeans(DF_aghru,na.rm=T)
  # DF_aghru<-data.frame(as.list(DF_aghru))
  # DF_aghru<-reshape2::melt(DF_aghru)
  # 
  # 
  # return(list(DF_aghru,print("testing")))
  # 
  # #write.table(DF_aghru,"hruLoss_summary.csv",row.names=F,col.names=F,sep="," )
  
  
  
}     



