#test passing more than 1 plot/graph to the GUI interface

testPlot<-function(local_dir,SelectClimate){
  
  local_dir<-gsub("\\\\", "/",local_dir)

  baseline_dir<-paste0(local_dir,'/Baseline')
  scenario_dir<-paste0(local_dir,'/Scenarios')
  
  BR_plot_hist<-NULL
  HRU_per_hist<-NULL
  HRU_abs_hist<-NULL
  yield_per_hist<-NULL
  yield_abs_hist<-NULL
  yield_table_hist<-NULL
  
  BR_plot_clim<-NULL
  HRU_per_clim<-NULL
  HRU_abs_clim<-NULL
  yield_per_clim<-NULL
  yield_abs_clim<-NULL
  yield_table_clim<-NULL
  
  # in case hru plots aren't made in historical climate return NULL
  BR_plot<-NULL
  HRU_per<-NULL
  HRU_abs<-NULL
  yield_per<-NULL
  yield_abs<-NULL
  yield_table<-NULL
  
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
  
  headers_hru_wb<-c("jday",   "mon",   "day",    "yr",    "unit",  "gis_id",  "name", "precip", "snofall",  "snomlt",   "surq_gen", "latq", "wateryld", "perc",
                    "et", "ecanopy",  "eplant", "esoil","surq_cont",  "cn", "sw_init","sw_final", "sw_ave", "sw_300", "sno_init", "sno_final",  "snopack",  "pet", "qtile",         
                    "irr",  "surq_runon",  "latq_runon",    "overbank",    "surq_cha",    "surq_res", "surq_ls","latq_cha", "latq_res", "latq_ls",  "gwtranq","satex","satex_chan",   
                    "sw_change",     "lagsurf",     "laglatq",   "lagsatex")
  
  #### Datat frames for final data #######

  # hru_loss<-c()
  
  
  
# if ('hist' %in% SelectClimate){
  for(climatemodel in SelectClimate){
    
    ch_loss<-c()
  
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
  
  
  BR_plot<-ggplot(ch_loss,aes(x=variable,y=change_per))+geom_bar(stat = 'identity',color='black')+ylab("Change from baseline (%)")+
    xlab("")+ ggtitle("Change at Berlin Rd")+
    # geom_text(size=16,aes(label=round(change_per)), position=position_dodge(width=0.9), vjust=0.5,colour="black")+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          panel.background = element_blank(),text = element_text(size = 16),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1))+geom_hline(yintercept=0)
  
  
  ##### Management rotations / tile plot ######################
  
  # NewBaseline writes the *hru loss* file INTO the matching scenario folder and calls it 'hru_baseline'
  hru_loss_baseline<-read.csv("hru_baseline.csv") %>% 
    mutate(sedyld_t=sedyld_tha*area_ha,
           tilelabp_kg=tilelabp*area_ha,
           totp_kg=totp*area_ha,
           surqsolp_kg=surqsolp_kgha*area_ha,
           qtile_m3=qtile*area_ha*10000/1000,
           surq_m3=surq_cont*area_ha*10000/1000,
           perc_m3=perc*area_ha*10000/1000) %>% 
    gather(variable,value,-yr,-lu_mgt,-name,-hyd_grp,-slp,-tile,-scenario,-area_ha)
    # rename("sedyld_tha_b"="sedyld_tha",
    #        "tilelabp_b"="tilelabp",
    #        "totp_b"   =   "totp",
    #        "surqsolp_kgha_b"="surqsolp_kgha")
  
  # baseline lookup written in Baseline folder
  hru_lookup_b<-read.csv(paste0(baseline_dir,"\\hru_lookup.csv")) %>% 
    # rename_with( .fn = function(.x){paste0("b_", .x)})
    rename("b_lu_mgt"="lu_mgt") %>% 
    select(name,b_lu_mgt)
  
  # scenario lookup written in Scenario folder
  hru_lookup<-read.csv(paste0(scenario_dir,"\\hru_lookup_scenario.csv")) %>% 
    left_join(.,hru_lookup_b,by=c("name")) %>% 
    mutate(changed_hru=NA) %>% 
    mutate(changed_hru=replace(changed_hru,lu_mgt != b_lu_mgt,1)) %>% 
    select(id,name,hydro,muid,lu_mgt,b_lu_mgt,hyd_grp,area_ha,slp,changed_hru,tile)
  
  
  #### ONLY MAKE HRU PLOTS IF CHANGED HRU > 1 ##########
  if (sum(hru_lookup$changed_hru==1)>=1 | climatemodel == 'userClimScen'){ # will only enter if an HRU was changed OR it is the climate scenario
  
  
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
    
  DF_aghru <- DF %>% 
    mutate(totp=sedorgp_kgha+surqsolp_kgha+sedmin+tilelabp+lchlabp) 
  
  
  ##### hydrology #####
  tmp <-file(here(scenario_dir,climatemodel,'hru_wb_yr.txt'))
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
  colnames(DF)<-headers_hru_wb
  
  
  # DF$date<-as.Date(paste(DF$mon,DF$day,DF$yr,sep="/"), format="%m/%d/%Y")              # add date column
  DF[,c(1:6,8:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:6,8:(ncol(DF)-1))]))           # convert to numerics
  
  DF_aghru<-left_join(DF_aghru,DF,by=c("name","yr","day","jday","mon","unit","gis_id"))
  
  DF_aghru<-DF_aghru %>% 
    left_join(.,hru_lookup,by=c("name")) %>% 
    mutate(scenario='Recent climate (2013-2020) - change landuse') %>% 
    select("yr","lu_mgt","area_ha", "name" ,"sedyld_tha","tilelabp","totp","surqsolp_kgha","qtile","surq_cont","perc","hyd_grp", "slp","tile","scenario","changed_hru") %>% 
    
    mutate(sedyld_t=sedyld_tha*area_ha,
           tilelabp_kg=tilelabp*area_ha,
           totp_kg=totp*area_ha,
           surqsolp_kg=surqsolp_kgha*area_ha,
           qtile_m3=qtile*area_ha*10000/1000,
           surq_m3=surq_cont*area_ha*10000/1000,
           perc_m3=perc*area_ha*10000/1000) %>% 
    gather(variable,value,-yr,-lu_mgt,-name,-hyd_grp,-slp,-tile,-scenario,-area_ha,-changed_hru)
  
  #### calculate reduction for all hrus and for hrus with practice applied ######
  
  # % change in only changed HRUs
  # perecent change in average annual loss 
  scenariodf<-DF_aghru %>% 
    rename("value_scen"="value",
           "lu_mgt_scen"="lu_mgt") %>% 
    left_join(.,hru_loss_baseline,by=c("yr","name","hyd_grp","slp","tile","variable")) %>% 
    filter(changed_hru==1) %>%
    # group_by(variable,name,hyd_grp,slp,tile,lu_mgt_scen,changed_hru) %>% 
    # summarize(value=mean(value,na.rm=T),value_scen=mean(value_scen,na.rm=T)) %>% # annual output for whole run
    ungroup() %>% 
    group_by(name,variable) %>% 
    summarize(value=mean(value,na.rm=T),value_scen=mean(value_scen,na.rm=T)) %>% # annual output for whole run
    mutate(percent_change=(value-value_scen)*100/value)
    # filter(!(lu_mgt %in% c('frsd_lum','urml_lum','past_lum'))) # only look at changes in changed HRUs// don't need bc changed HRUs would only be agricultural
  
  # Overall change from landscape 
  # summarize total loss from all HRUs and calculate a % change
  # for ALL HRUs
  TotalHRUchange<-DF_aghru %>% 
    rename("value_scen"="value",
           "lu_mgt_scen"="lu_mgt") %>% 
    select(-area_ha) %>% 
    left_join(.,hru_loss_baseline,by=c("yr","name","hyd_grp","slp","tile","variable")) %>% 
    filter(!(lu_mgt %in% c('frsd_lum','urml_lum','past_lum'))) %>% 
    group_by(variable,yr) %>%
    summarize(value=sum(value),value_scen=sum(value_scen),area_ha=sum(area_ha)) %>% # total annual
    ungroup() %>% 
    group_by(variable) %>% 
    summarize(value=mean(value),value_scen=mean(value_scen),area_ha=mean(area_ha)) %>% # average annual 2013-2020
    mutate(percent_change=(value_scen-value)*100/value) %>% 
    mutate(output='All row-crop fields')
  
  # for changed HRUs only
  TotalHRUchange_changed<-DF_aghru %>% 
    rename("value_scen"="value",
           "lu_mgt_scen"="lu_mgt") %>% 
    select(-area_ha) %>%
    left_join(.,hru_loss_baseline,by=c("yr","name","hyd_grp","slp","tile","variable")) %>% 
    filter(!(lu_mgt %in% c('frsd_lum','urml_lum','past_lum')),
           changed_hru==1) %>% 
    group_by(variable,yr) %>%
    summarize(value=sum(value),value_scen=sum(value_scen),area_ha=sum(area_ha)) %>% # total annual
    ungroup() %>% 
    group_by(variable) %>% 
    summarize(value=mean(value),value_scen=mean(value_scen),area_ha=mean(area_ha)) %>%# average annual 2013-2020
    mutate(percent_change=(value_scen-value)*100/value) %>%
    mutate(output='Fields with changed practices only')
  
  # combine
  
  TotalHRUchange<-rbind(TotalHRUchange,TotalHRUchange_changed) %>% 
    mutate(abs_change_kgha= (value_scen-value)) %>% 
    mutate(abs_change_lbsacre=(value_scen-value)*2.20462/area_ha*2.47105)
    
  
 ##### new hru plots comparing hru loss from all agricultural hrus and hrus with change implemented only ##### 
  
  # Consider separating by tile/ nontiled
  
  variable_labs<-c('Sediment yield','Surface soluble P','Tile P','Total P','Surface runoff','Tile discharge')
  names(variable_labs)<-c('sedyld_t','surqsolp_kg','tilelabp_kg','totp_kg','surq_m3','qtile_m3')
    
    # Percent change from all HRUs 
    
    HRU_per<-TotalHRUchange %>% 
      filter(variable %in% c('perc_m3','qtile_m3','sedyld_t','surq_m3','surqsolp_kg','tilelabp_kg','totp_kg')) %>% 
      ggplot(., aes(x=variable,y=percent_change,fill=output))+geom_bar(stat='identity',position="dodge",color='black')+    
      xlab("")+ylab("Change from baseline (%)")+
      scale_x_discrete(labels=c('sedyld_t'='Sediment yield','surqsolp_kg'='Surface soluble P',
                                'tilelabp_kg'='Tile P','totp_kg'='Total P','surq_m3'='Surface runoff','qtile_m3'='Tile discharge',
                                'perc_m3'='Percolation'))+
      # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
      scale_fill_manual(values=c('All row-crop fields'='white','Fields with changed practices only'='grey'))+
      geom_hline(yintercept=0)+
      theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            panel.background = element_blank(),text = element_text(size = 16),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
            legend.title = element_blank(),
            legend.position='top')
    
    # Change in kg/ha from all HRUs, only nutrients / sediments 
    
    HRU_abs<-TotalHRUchange %>% 
      filter(variable %in% c('sedyld_t','surqsolp_kg','tilelabp_kg','totp_kg')) %>% 
      # gather(units,change,-variable,-value,-value_scen,-area_ha,-percent_change,-output) %>% 
      ggplot(., aes(x=variable, y=abs_change_lbsacre, fill=output))+geom_bar(stat='identity',position="dodge",color='black')+    
      xlab("")+ylab("Absolute change from baseline (lb/acre)")+
      scale_x_discrete(labels=c('sedyld_t'='Sediment yield','surqsolp_kg'='Surface soluble P',
                                'tilelabp_kg'='Tile P','totp_kg'='Total P','surq_m3'='Surface runoff','qtile_m3'='Tile discharge',
                                'perc_m3'='Percolation'))+
      # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
      scale_fill_manual(values=c('All row-crop fields'='white','Fields with changed practices only'='grey'))+
      geom_hline(yintercept=0)+
      theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            panel.background = element_blank(),text = element_text(size = 16),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
            legend.title = element_blank(),
            legend.position='top')
    
  
  
  variable_labs<-c('Sediment yield (t/ha)','Surface soluble P (kg/ha)','Total P (kg/ha)')
  names(variable_labs)<-c('sedyld_tha','surqsolp_kgha','totp')

  

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
  if(sum(yield$changed_hru) > 0){
    
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
  }

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
    scale_fill_manual(labels=c('MASS'='Land management scenario (2013-2020)',
                                 'MASS_b'='Baseline (2013-2020)'),values=c("MASS"='seagreen',"MASS_b"='lightblue'))+
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
  
  # assign plots in loop to output 
  
  if (climatemodel=='hist'){
    
    BR_plot_hist<-BR_plot
    HRU_per_hist<-HRU_per
    HRU_abs_hist<-HRU_abs
    yield_per_hist<-yield_per
    yield_abs_hist<-yield_abs
    yield_table_hist<-yield_table
    
    
  }else{
    
    BR_plot_clim<-BR_plot
    HRU_per_clim<-HRU_per
    HRU_abs_clim<-HRU_abs
    yield_per_clim<-yield_per
    yield_abs_clim<-yield_abs
    yield_table_clim<-yield_table
    
    
    
    
  }
  
} 
  
  
  return(list(print("OWC-SWAT+ run complete"), BR_plot_hist,HRU_per_hist, HRU_abs_hist, yield_per_hist,yield_abs_hist,yield_table_hist,
              BR_plot_clim,HRU_per_clim, HRU_abs_clim, yield_per_clim,yield_abs_clim,yield_table_clim))
  
  
  
}