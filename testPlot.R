#test passing more than 1 plot/graph to the GUI interface

testPlot<-function(scenario_dir,SelectClimate){

  
  baseline_dir<-here('Baseline')
  
  BR_plot<-NULL
  HRU_per<-NULL
  HRU_abs<-NULL
  yield_per<-NULL
  yield_abs<-NULL
  yield_table<-NULL
  
  BR_plot_clim<-NULL
  HRU_per_clim<-NULL
  HRU_abs_clim<-NULL
  yield_per_clim<-NULL
  yield_abs_clim<-NULL
  yield_table_clim<-NULL
  
# Calculate outputs here
  headers_ch<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"areaha",	"precipha.m",	"evapha.m",	
             "seepha.m",	"flo_storm.3.s",	"sed_stormtons",	"orgn_storkgN",	"sedp_storkgP",	"no3_storkgN",	"solp_storkgP",
             "chla_storkg",	"nh3_storkgN",	"no2_storkgN",	"cbod_storkg",	"dox_storkg",	"san_stortons",	"sil_stortons",	"cla_stortons",	"sag_stortons",
             "lag_stortons",	"grv_stortons",	"null1", "setl_stor",	"setlp_stor",	"flo_inm.3.s",	"sed_inmtons",	"orgn_inkgN",	"sedp_inkgP",	"no3_inkgN",
             "solp_inkgP",	"chla_inkg",	"nh3_inkgN",	"no2_inkgN",	"cbod_inkg",	"dox_inkg",	"san_intons",	"sil_intons",	"cla_intons",
             "sag_intons",	"lag_intons",	"grv_intons",	"null",	 "setl_in",	"setlp_in","flo_outm.3.s",	"sed_outmtons",	"orgn_outkgN",	"sedp_outkgP",	"no3_outkgN",
             "solp_outkgP",	"chla_outkg",	"nh3_outkgN",	"no2_outkgN",	"cbod_outkg",	"dox_outkg",	"san_outtons",	"sil_outtons",	"cla_outtons",
             "sag_outtons",	"lag_outtons",	"grv_outtons",	"null2", "setl_out",	"setlp_out", "water_tempdegC")#"null3","null4","null5","null6","null7")
  
  headers_hru<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"sedyld_tha","sedorgn_kgha","sedorgp_kgha",
             "surqno3_kgha","lat3no3_kgha","surqsolp_kgha","usle_tons","sedmin","tileno3","lchlabp","tilelabp","satexn")
  
  headers_crop<-c(  "jday",   "mon",   "day",    "yr",    "unit", "PLANTNM", "MASS", "C", "N", "P")
  
  #### Datat frames for final data #######
  ch_loss<-c()
  # hru_loss<-c()
  
  
  
if ('hist' %in% SelectClimate){
  
  climatemodel<-'hist'
    
  setwd(paste0(scenario_dir,'/',climatemodel))
  
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
  
  #### Read in baseline data #####
  baseline_data<-read.csv("baseline_data_avg.csv")
  
  ################ Change at Berlin Rd. ############################################################
  baseline_data$scenario[baseline_data$variable=="discharge_cms"]<-mean(DF$flo_outm.3.s,na.rm=T)
  baseline_data$scenario[baseline_data$variable=="solp_kg"]<-sum(DF$solp_outkgP,na.rm=T)
  # baseline_data$scenario[baseline_data$variable=="sedp_kg"]<-sum(DF$sedp_outkgP,na.rm=T)
  baseline_data$scenario[baseline_data$variable=="sediment_kg"]<-sum(DF$sed_outmtons,na.rm=T)
  baseline_data$scenario[baseline_data$variable=="totp_kg"]<-sum(DF$solp_outkgP + DF$sedp_outkgP,na.rm=T)
  
  # calculate % difference between baseline and scenario
  baseline_data$change_per<-(baseline_data$scenario-baseline_data$baseline) *100 / baseline_data$baseline
  
  baseline_data$legendkey<-climatemodel
  
  ch_loss<-rbind(ch_loss,baseline_data)
  
  ch_loss$variable<-factor(ch_loss$variable)
  levels(ch_loss$variable)[levels(ch_loss$variable)=="discharge_cms"]<-"Discharge"
  levels(ch_loss$variable)[levels(ch_loss$variable)=="sediment_kg"]<-"Sediment"
  levels(ch_loss$variable)[levels(ch_loss$variable)=="solp_kg"]<-"Dissolved P"
  levels(ch_loss$variable)[levels(ch_loss$variable)=="totp_kg"]<-"Total P"
  
  
  BR_plot<-ggplot(ch_loss,aes(x=variable,y=change_per))+geom_bar(stat = 'identity')+ylab("Change from baseline (%)")+
    xlab("")+ ggtitle("Change at Berlin Rd")+
    # geom_text(size=16,aes(label=round(change_per)), position=position_dodge(width=0.9), vjust=0.5,colour="black")+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          panel.background = element_blank(),text = element_text(size = 16),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
  
  
  ##### Management rotations / tile plot ######################
  
  # NewBaseline writes the *hru loss* file INTO the matching scenario folder and calls it 'hru_baseline'
  hru_loss_baseline<-read.csv("hru_baseline.csv") %>% 
    rename("sedyld_tha_b"="sedyld_tha",
           "tilelabp_b"="tilelabp",
           "totp_b"   =   "totp",
           "surqsolp_kgha_b"="surqsolp_kgha")
  
  # baseline lookup written in Baseline folder
  hru_lookup_b<-read.csv(paste0(baseline_dir,"\\hru_lookup.csv")) %>% 
    rename_with( .fn = function(.x){paste0("b_", .x)})
  
  # scenario lookup written in Scenario folder
  hru_lookup<-read.csv(paste0(scenario_dir,"\\hru_lookup_scenario.csv")) %>% 
    left_join(.,hru_lookup_b,by=c("name"="b_name")) %>% 
    mutate(changed_hru=NA) %>% 
    mutate(changed_hru=replace(changed_hru,lu_mgt != b_lu_mgt,1)) %>% 
    select(id,name,hydro,muid,lu_mgt,b_lu_mgt,hyd_grp,area_ha,slp,changed_hru,tile)
  
  
  #### ONLY MAKE HRU PLOTS IF CHANGED HRU > 1 ##########
  if (sum(hru_lookup$changed_hru==1)>=1){ # will only enter if an HRU was changed
  
  
  tmp <- file(here(scenario_dir,climatemodel,'hru_ls_yr.txt'))
  open(tmp, "r") #read
  
  #read past headerlines
  readLines(tmp, n = 3) 
  
  
  
  ###### read in scenario yearly hru loss #########
  data<-readLines(tmp, n = -1)  
  close(tmp)
  DF<-strsplit(data,split=" ")
  DF<-lapply(DF, function(z){ z[z != ""]}) 
  DF<-data.frame(do.call(rbind, DF)) #unlist
  colnames(DF)<-headers_hru
  
  
  # DF$date<-as.Date(paste(DF$mon,DF$day,DF$yr,sep="/"), format="%m/%d/%Y")              # add date column
  DF[,c(1:6,8:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:6,8:(ncol(DF)-1))])) # convert to numerics
    
  DF <- DF %>% 
    mutate(totp=sedorgp_kgha+surqsolp_kgha+sedmin+tilelabp+lchlabp) %>% 
    select("name" ,'yr',"sedyld_tha","tilelabp","totp","surqsolp_kgha") %>% 
    mutate(scenario='land management scenario')
  
  DF_aghru<-left_join(DF,hru_lookup,by=c("name"))%>% 
    mutate(scenario='Recent climate (2013-2020) - change landuse') 
  
  #### calculate reduction for all hrus and for hrus with practice applied ######
  
  
  scenariodf<-DF_aghru %>% 
    select(name,yr,sedyld_tha,tilelabp,totp,surqsolp_kgha,area_ha,slp,changed_hru,hyd_grp) %>% 
    left_join(.,hru_loss_baseline,by=c('yr','name','slp','hyd_grp')) %>% 
    mutate(totp_change=(totp-totp_b)*100/totp_b,
           surqsolp_change=(surqsolp_kgha-surqsolp_kgha_b)*100/surqsolp_kgha_b,
           sedyld_change=(sedyld_tha-sedyld_tha_b)*100/sedyld_tha_b) %>% 
    filter(!(lu_mgt %in% c('frsd_lum','urml_lum','past_lum')), changed_hru==1) # only look at changes in changed HRUs
  
 ##### new hru plots comparing hru loss from all agricultural hrus and hrus with change implemented only ##### 
  print('line 187')
  
  # Consider separating by tile/ nontiled
  
  variable_labs<-c('Sediment yield','Surface soluble P','Total P')
  names(variable_labs)<-c('sedyld_change','surqsolp_change','totp_change')
  
  # percent change plot
    HRU_per<-scenariodf %>% 
    select(name,totp_change,sedyld_change,surqsolp_change) %>% 
    gather(variable,value,-name) %>% 
    ggplot(.,aes(x=1,y=value))+geom_boxplot()+facet_wrap(~variable,scales='free_y',labeller=labeller(variable=variable_labs))+
    xlab("")+ylab("Percent change from baseline")+
    # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          panel.background = element_blank(),text = element_text(size = 16),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          legend.title = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  
  variable_labs<-c('Sediment yield (t/ha)','Surface soluble P (kg/ha)','Total P (kg/ha)')
  names(variable_labs)<-c('sedyld_tha','surqsolp_kgha','totp')

  
  # absolute change plot
    HRU_abs<-scenariodf %>% 
    select(name,totp,totp_b,surqsolp_kgha,surqsolp_kgha_b,sedyld_tha,sedyld_tha_b) %>% 
    gather(variable,value,-name) %>% 
    mutate(scenario=NA) %>% 
    mutate(scenario=replace(scenario,grepl('b',variable),'baseline (2013-2020)')) %>% 
    mutate(scenario=replace(scenario,!grepl('b',variable),'land management scenario (2013-2020)')) %>% 
    mutate_at('variable',str_replace,"_b","") %>% 
    ggplot(.,aes(x=1,y=value,fill=scenario))+geom_boxplot()+facet_wrap(~variable,scales='free_y',labeller=labeller(variable=variable_labs))+
    xlab("")+ylab("")+
    scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                                                                                     panel.background = element_blank(),text = element_text(size = 16),
                                                                                     panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          legend.title = element_blank(),
          legend.position = 'bottom',
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  

  ################## Read and write baseline yield ########################################
  yield<-read.csv('crop_yield_baseline.csv') %>% 
    rename('MASS_b'='MASS') %>% 
    select(!c('C','N','P')) %>%  # remove unused columns
    group_by(unit,PLANTNM,yr) %>% 
    summarize(MASS_b=mean(MASS_b)) # 2 plant summaries exists if plant is planted twice in rotation (I think), making left join
  # generate multiple columns. Fix by summarize data before joining
  
  tmp <- file('crop_yld_aa.txt')
  open(tmp, "r") #read
  
  #read past headerlines
  readLines(tmp, n = 5) 
  
  
  
  ###### read in simulated data columns #########
  

  data<-readLines(tmp, n = -1)  
  DF<-strsplit(data,split=" ")
  DF<-lapply(DF, function(z){ z[z != ""]}) 
  DF<-data.frame(do.call(rbind, DF)) #unlist
  colnames(DF)<-headers_crop
  
  
  
  DF[,c(1:5,7:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:5,7:(ncol(DF)-1))]))           # convert to numerics
  
  DF <-DF  %>%    group_by(unit,PLANTNM,yr) %>% 
    summarize(MASS=mean(MASS))
  
  ################# read in lookup table ######################################
  DF_lookup<-left_join(DF,hru_lookup, by=c("unit"="id"))

  #convert mass to bu / acre
  DF_lookup$MASS[DF_lookup$PLANTNM=='corn']<-DF_lookup$MASS[DF_lookup$PLANTNM=='corn']/62.77
  DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']<-DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']/67.25
  DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']<-DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']/67.25
  
  
  yield<-left_join(yield,DF_lookup,by=c('unit','PLANTNM')) %>% 
    mutate(MASS_change=(MASS-MASS_b)/MASS_b) %>% 
    select(unit,PLANTNM,MASS_b,MASS,hyd_grp,area_ha,changed_hru,tile,MASS_change)

  
  ##### New yield plot here ############ 11/8/2023
  
  give.n <- function(x){
    return(data.frame(y = max(x)*1.1, label = paste("n =",length(x),collapse="") )) 
    # experiment with the multiplier to find the perfect position
  }  
  
  # changed hrus only
    yield_per<-yield %>% 
    filter(changed_hru==1,PLANTNM %in% c('soyb','corn','wwht')) %>%
    select(unit,PLANTNM,MASS_change) %>% 
    ggplot(.,aes(x=PLANTNM,y=MASS_change))+ geom_boxplot()+
    scale_x_discrete(labels=c('corn'='Corn','soyb'='Soybean','wwht'='Winter wheat'))+
    xlab("")+ylab("Change from baseline (%)")+
    stat_summary(fun.data = give.n, geom = "text", fun.y = median, 
                 position = position_dodge(width = 0.75))+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          panel.background = element_blank(),text = element_text(size = 16),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          legend.title = element_blank())

  plot_mean <- function(x){
    return(c(y = max(x)*1.2, label = round(mean(x),0))) 
    # experiment with the multiplier to find the perfect position
  } 

  # whole watershed
  yield_abs<-yield %>% 
    filter(PLANTNM %in% c('soyb','corn','wwht')) %>%
    select(unit,PLANTNM,MASS,MASS_b,tile) %>% 
    gather(variable,value,-unit,-PLANTNM,-tile) %>% 
    mutate(tile=replace(tile,is.na(tile),'no tile fields')) %>% 
    mutate(tile=replace(tile,tile==1,'tile fields')) %>% 
    ggplot(.,aes(x=PLANTNM,y=value,fill=variable))+ geom_boxplot()+facet_wrap(~tile)+
    scale_x_discrete(labels=c('corn'='Corn','soyb'='Soybean','wwht'='Winter wheat'))+
    scale_fill_discrete(labels=c('MASS'='Land management scenario (2013-2020)',
                                 'MASS_b'='Baseline (2013-2020)'))+
    xlab("")+ylab("Yield (bu/acre)")+
    stat_summary(fun.data = plot_mean, geom = "text", fun.y = median, 
                 position = position_dodge(width = 0.75))+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          panel.background = element_blank(),text = element_text(size = 16),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          legend.title = element_blank(),legend.position='bottom')
  
 
  
  
  
  # final table with whole watershed yields
  yield_table <- yield %>% 
    filter(PLANTNM %in% c('soyb','corn','wwht')) %>%
    mutate(PLANTNM=replace(PLANTNM,PLANTNM=='soyb','Soybean')) %>% 
    mutate(PLANTNM=replace(PLANTNM,PLANTNM=='corn','Corn')) %>% 
    mutate(PLANTNM=replace(PLANTNM,PLANTNM=='wwht','Winter wheat')) %>% 
    group_by(PLANTNM) %>% 
    summarize(MASS=round(mean(MASS),0),MASS_b=round(mean(MASS_b),0)) %>%
    rename('Crop'='PLANTNM',
           'Land management scenario (bu/acre)'='MASS',
           'Baseline (bu/acre)'='MASS_b')

  } 
  
} 
  
  #### Climate scenario ######
  
  if ('userClimScen' %in% SelectClimate){
    
    climatemodel<-'userClimScen'
    
    setwd(paste0(scenario_dir,'/',climatemodel))
    
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
    
    #### Read in baseline data #####
    baseline_data<-read.csv("baseline_data_avg.csv")
    
    ################ Change at Berlin Rd. ############################################################
    baseline_data$scenario[baseline_data$variable=="discharge_cms"]<-mean(DF$flo_outm.3.s,na.rm=T)
    baseline_data$scenario[baseline_data$variable=="solp_kg"]<-sum(DF$solp_outkgP,na.rm=T)
    # baseline_data$scenario[baseline_data$variable=="sedp_kg"]<-sum(DF$sedp_outkgP,na.rm=T)
    baseline_data$scenario[baseline_data$variable=="sediment_kg"]<-sum(DF$sed_outmtons,na.rm=T)
    baseline_data$scenario[baseline_data$variable=="totp_kg"]<-sum(DF$solp_outkgP + DF$sedp_outkgP,na.rm=T)
    
    # calculate % difference between baseline and scenario
    baseline_data$change_per<-(baseline_data$scenario-baseline_data$baseline) *100 / baseline_data$baseline
    
    baseline_data$legendkey<-climatemodel
    
    ch_loss<-rbind(ch_loss,baseline_data)
    
    ch_loss$variable<-factor(ch_loss$variable)
    levels(ch_loss$variable)[levels(ch_loss$variable)=="discharge_cms"]<-"Discharge"
    levels(ch_loss$variable)[levels(ch_loss$variable)=="sediment_kg"]<-"Sediment"
    levels(ch_loss$variable)[levels(ch_loss$variable)=="solp_kg"]<-"Dissolved P"
    levels(ch_loss$variable)[levels(ch_loss$variable)=="totp_kg"]<-"Total P"
    
    # rename scenarios
    ch_loss$legendkey[ch_loss$legendkey=="baseline"]<-"Historical climate (1990-2019) - no landuse change"
    ch_loss$legendkey[ch_loss$legendkey=="userClimScen"]<-"Climate change + landuse change"
    
    
    BR_plot_clim<-ggplot(ch_loss,aes(x=variable,y=change_per))+geom_bar(stat = 'identity')+ylab("Change from baseline (%)")+
      xlab("")+ ggtitle("Change at Berlin Rd")+
      # geom_text(size=16,aes(label=round(change_per)), position=position_dodge(width=0.9), vjust=0.5,colour="black")+
      theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            panel.background = element_blank(),text = element_text(size = 16),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
    
    
    ##### Management rotations / tile plot ######################
    
    # NewBaseline writes the *hru loss* file INTO the matching scenario folder and calls it 'hru_baseline'
    hru_loss_baseline<-read.csv("hru_baseline.csv") %>% 
      rename("sedyld_tha_b"="sedyld_tha",
             "tilelabp_b"="tilelabp",
             "totp_b"   =   "totp",
             "surqsolp_kgha_b"="surqsolp_kgha")
    
    # baseline lookup written in Baseline folder
    hru_lookup_b<-read.csv(paste0(baseline_dir,"\\hru_lookup.csv")) %>% 
      rename_with( .fn = function(.x){paste0("b_", .x)})
    
    # scenario lookup written in Scenario folder
    hru_lookup<-read.csv(paste0(scenario_dir,"\\hru_lookup_scenario.csv")) %>% 
      left_join(.,hru_lookup_b,by=c("name"="b_name")) %>% 
      mutate(changed_hru=NA) %>% 
      mutate(changed_hru=replace(changed_hru,lu_mgt != b_lu_mgt,1)) %>% 
      select(id,name,hydro,muid,lu_mgt,b_lu_mgt,hyd_grp,area_ha,slp,changed_hru,tile)

      
      
      tmp <- file(here(scenario_dir,climatemodel,'hru_ls_yr.txt'))
      open(tmp, "r") #read
      
      #read past headerlines
      readLines(tmp, n = 3) 
      
      
      
      ###### read in scenario yearly hru loss #########
      data<-readLines(tmp, n = -1)  
      close(tmp)
      DF<-strsplit(data,split=" ")
      DF<-lapply(DF, function(z){ z[z != ""]}) 
      DF<-data.frame(do.call(rbind, DF)) #unlist
      colnames(DF)<-headers_hru
      
      
      # DF$date<-as.Date(paste(DF$mon,DF$day,DF$yr,sep="/"), format="%m/%d/%Y")              # add date column
      DF[,c(1:6,8:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:6,8:(ncol(DF)-1))])) # convert to numerics
      
      DF <- DF %>% 
        mutate(totp=sedorgp_kgha+surqsolp_kgha+sedmin+tilelabp+lchlabp) %>% 
        select("name" ,'yr',"sedyld_tha","tilelabp","totp","surqsolp_kgha") %>% 
        mutate(scenario='land management scenario')
      
      DF_aghru<-left_join(DF,hru_lookup,by=c("name"))%>% 
        mutate(scenario='Recent climate (2013-2020) - change landuse') 
      
      #### calculate reduction for all hrus and for hrus with practice applied ######
      
      
      scenariodf<-DF_aghru %>% 
        select(name,yr,sedyld_tha,tilelabp,totp,surqsolp_kgha,area_ha,slp,changed_hru,hyd_grp) %>% 
        left_join(.,hru_loss_baseline,by=c('yr','name','slp','hyd_grp')) %>% 
        mutate(totp_change=(totp-totp_b)*100/totp_b,
               surqsolp_change=(surqsolp_kgha-surqsolp_kgha_b)*100/surqsolp_kgha_b,
               sedyld_change=(sedyld_tha-sedyld_tha_b)*100/sedyld_tha_b) %>% 
        filter(!(lu_mgt %in% c('frsd_lum','urml_lum','past_lum'))) # look at changes in all HRUs
      
      ##### new hru plots comparing hru loss from all agricultural hrus and hrus with change implemented only ##### 
      
      # Consider separating by tile/ nontiled
      
      variable_labs<-c('Sediment yield','Surface soluble P','Total P')
      names(variable_labs)<-c('sedyld_change','surqsolp_change','totp_change')
      
      # percent change plot
      HRU_per_clim<-scenariodf %>% 
        select(name,totp_change,sedyld_change,surqsolp_change) %>% 
        gather(variable,value,-name) %>% 
        ggplot(.,aes(x=1,y=value))+geom_boxplot()+facet_wrap(~variable,scales='free_y',labeller=labeller(variable=variable_labs))+
        xlab("")+ylab("Percent change from baseline")+
        # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
        theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              panel.background = element_blank(),text = element_text(size = 16),
              panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
              legend.title = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      
      variable_labs<-c('Sediment yield (t/ha)','Surface soluble P (kg/ha)','Total P (kg/ha)')
      names(variable_labs)<-c('sedyld_tha','surqsolp_kgha','totp')
      
      
      # absolute change plot
      HRU_abs_clim<-scenariodf %>% 
        select(name,totp,totp_b,surqsolp_kgha,surqsolp_kgha_b,sedyld_tha,sedyld_tha_b) %>% 
        gather(variable,value,-name) %>% 
        mutate(scenario=NA) %>% 
        mutate(scenario=replace(scenario,grepl('b',variable),'baseline (1990-2019)')) %>% 
        mutate(scenario=replace(scenario,!grepl('b',variable),'climate + land management scenario')) %>% 
        mutate_at('variable',str_replace,"_b","") %>% 
        ggplot(.,aes(x=1,y=value,fill=scenario))+geom_boxplot()+facet_wrap(~variable,scales='free_y',labeller=labeller(variable=variable_labs))+
        xlab("")+ylab("")+
        scale_fill_manual(values=c('baseline (1990-2019)'='white', 'climate + land management scenario'='grey'))+
        theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              panel.background = element_blank(),text = element_text(size = 16),
              panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
              legend.title = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      
      ################## Read and write baseline yield ########################################
      yield<-read.csv('crop_yield_baseline.csv') %>% 
        rename('MASS_b'='MASS') %>% 
        select(!c('C','N','P')) %>%  # remove unused columns
        group_by(unit,PLANTNM,yr) %>% 
        summarize(MASS_b=mean(MASS_b)) # 2 plant summaries exists if plant is planted twice in rotation (I think), making left join
      # generate multiple columns. Fix by summarize data before joining
      
      tmp <- file('crop_yld_aa.txt')
      open(tmp, "r") #read
      
      #read past headerlines
      readLines(tmp, n = 5) 
      
      
      
      ###### read in simulated data columns #########
      
      
      data<-readLines(tmp, n = -1)  
      DF<-strsplit(data,split=" ")
      DF<-lapply(DF, function(z){ z[z != ""]}) 
      DF<-data.frame(do.call(rbind, DF)) #unlist
      colnames(DF)<-headers_crop
      
      
      
      DF[,c(1:5,7:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:5,7:(ncol(DF)-1))]))           # convert to numerics
      
      DF <-DF  %>%    group_by(unit,PLANTNM,yr) %>% 
        summarize(MASS=mean(MASS))
      
      ################# read in lookup table ######################################
      DF_lookup<-left_join(DF,hru_lookup, by=c("unit"="id"))
      
      #convert mass to bu / acre
      DF_lookup$MASS[DF_lookup$PLANTNM=='corn']<-DF_lookup$MASS[DF_lookup$PLANTNM=='corn']/62.77
      DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']<-DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']/67.25
      DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']<-DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']/67.25
      
      
      yield<-left_join(yield,DF_lookup,by=c('unit','PLANTNM')) %>% 
        mutate(MASS_change=(MASS-MASS_b)/MASS_b) %>% 
        select(unit,PLANTNM,MASS_b,MASS,hyd_grp,area_ha,changed_hru,tile,MASS_change)
      
      
      ##### New yield plot here ############ 11/8/2023
      
      give.n <- function(x){
        return(data.frame(y = max(x)*1.1, label = paste("n =",length(x),collapse="") )) 
        # experiment with the multiplier to find the perfect position
      }  
      
      # ALL HRUs
      yield_per_clim<-yield %>% 
        filter(PLANTNM %in% c('soyb','corn','wwht')) %>%
        select(unit,PLANTNM,MASS_change) %>% 
        ggplot(.,aes(x=PLANTNM,y=MASS_change))+ geom_boxplot()+
        scale_x_discrete(labels=c('corn'='Corn','soyb'='Soybean','wwht'='Winter wheat'))+
        xlab("")+ylab("Change from baseline (%)")+
        stat_summary(fun.data = give.n, geom = "text", fun.y = median, 
                     position = position_dodge(width = 0.75))+
        theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              panel.background = element_blank(),text = element_text(size = 16),
              panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
              legend.title = element_blank())
      
      plot_mean <- function(x){
        return(c(y = max(x)*1.2, label = round(mean(x),0))) 
        # experiment with the multiplier to find the perfect position
      } 
      
      # whole watershed
      yield_abs_clim<-yield %>% 
        filter(PLANTNM %in% c('soyb','corn','wwht')) %>%
        select(unit,PLANTNM,MASS,MASS_b,tile) %>% 
        gather(variable,value,-unit,-PLANTNM,-tile) %>% 
        mutate(tile=replace(tile,is.na(tile),'no tile fields')) %>% 
        mutate(tile=replace(tile,tile==1,'tile fields')) %>% 
        ggplot(.,aes(x=PLANTNM,y=value,fill=variable))+ geom_boxplot()+facet_wrap(~tile)+
        scale_x_discrete(labels=c('corn'='Corn','soyb'='Soybean','wwht'='Winter wheat'))+
        scale_fill_discrete(labels=c('MASS'='Land management scenario (2013-2020)',
                                     'MASS_b'='Baseline (2013-2020)'))+
        xlab("")+ylab("Yield (bu/acre)")+
        stat_summary(fun.data = plot_mean, geom = "text", fun.y = median, 
                     position = position_dodge(width = 0.75))+
        theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              panel.background = element_blank(),text = element_text(size = 16),
              panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
              legend.title = element_blank(),legend.position='bottom')
    
    
    
    # final table with whole watershed yields
    yield_table_clim <- yield %>% 
      filter(PLANTNM %in% c('soyb','corn','wwht')) %>%
      mutate(PLANTNM=replace(PLANTNM,PLANTNM=='soyb','Soybean')) %>% 
      mutate(PLANTNM=replace(PLANTNM,PLANTNM=='corn','Corn')) %>% 
      mutate(PLANTNM=replace(PLANTNM,PLANTNM=='wwht','Winter wheat')) %>% 
      group_by(PLANTNM) %>% 
      summarize(MASS=round(mean(MASS),0),MASS_b=round(mean(MASS_b),0)) %>%
      rename('Crop'='PLANTNM',
             'Land management scenario (bu/acre)'='MASS',
             'Baseline (bu/acre)'='MASS_b')
    
    
    
    
  } 
  
  
  return(list(print("OWC-SWAT+ run complete"), BR_plot,HRU_per, HRU_abs, yield_per,yield_abs,yield_table,
              BR_plot_clim,HRU_per_clim, HRU_abs_clim, yield_per_clim,yield_abs_clim,yield_table_clim))
  
  
  
}