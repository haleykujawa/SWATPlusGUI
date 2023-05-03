#test passing more than 1 plot/graph to the GUI interface

testPlot<-function(scenario_dir,SelectClimate){
  # library("ggplot2")
  # library("patchwork")
  # library("ggpmisc")
  
  # May want to end up building one DF and making a plot with it--hard to compare the same results if multiple plots exist.
  # Will want to pass this script info on the inputs used to print in the 'visualize outputs' column.
  
  # x<-runif(n=10, min=1, max=20)
  # y<-runif(n=10, min=1, max=20)
  # 
  # my_table<-data.frame(x,y)
  # 
  # plot1<-ggplot(data=my_table,aes(x,y))+geom_point()
  
  # ggp_table <- ggplot() +                             # Create empty plot with table
  # theme_void() +
  # annotate(geom = "table",
             # x = 1,
             # y = 1,
             # label = list(my_table))
  
  # output_data<-plot1+ggp_table
  # setwd(here('www'))
  
  # myplots<-list('hist','CNRM','IPSL-CM5A-MR','MIROC5')
  
  # Make empty objects to return if not calculated
  BR_plot<-NULL
  HRU_plot<-NULL
  yield_plot<-NULL
  BR_plot_clim<-NULL
  HRU_plot_clim<-NULL
  yield_plot_clim<-NULL
  
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
  
  # rename scenarios
  # ch_loss$legendkey[ch_loss$legendkey=="hist"]<-"Recent climate (2013-2020)"
  
  
  BR_plot<-ggplot(ch_loss,aes(x=variable,y=change_per))+geom_bar(stat = 'identity')+ylab("Change from baseline (%)")+
    xlab("")+ ggtitle("Change at Berlin Rd")+
    # geom_text(size=16,aes(label=round(change_per)), position=position_dodge(width=0.9), vjust=0.5,colour="black")+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          panel.background = element_blank(),text = element_text(size = 16),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
  
  
  ##### Management rotations / tile plot ######################
  
  hru_loss<-read.csv("hru_baseline.csv")
  
  hru_lookup<-read.csv(paste0(scenario_dir,"\\hru_lookup.csv"))
  
  
  tmp <- file(here(scenario_dir,climatemodel,'hru_ls_yr.txt'))
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
  
  DF_aghru<-left_join(DF,hru_lookup,by=c("name"))
  # DF_aghru<-DF_aghru[grepl(paste0(c("CS","SC","CSW"), collapse="|"),DF_aghru$lu_mgt),]
  
  # index tile columns
  DF_aghru$tile<-'no tile' 
  DF_aghru$tile[grepl("_t",DF_aghru$lu_mgt)]<- 'tile'
  
  # index all mgt scenarios
  DF_aghru$mgt<-NA
  DF_aghru<-DF_aghru %>% 
    mutate(mgt=replace(mgt, grepl(paste0(c("SC_FT","CS_FT"),collapse='|'),lu_mgt), "CS_FT")) %>% 
    mutate(mgt=replace(mgt, grepl(paste0(c("SC_RT","CS_RT"),collapse='|'),lu_mgt), "CS_RT")) %>% 
    mutate(mgt=replace(mgt, grepl(paste0(c("SC_RotT","CS_RotT"),collapse='|'),lu_mgt), "CS_RotT")) %>% 
    mutate(mgt=replace(mgt, (grepl(paste0(c("SC_NT","CS_NT"),collapse='|'),lu_mgt) & !grepl(paste0(c("SC_NTcc","CS_NTcc"),collapse='|'),lu_mgt)), "CS_NT")) %>% 
    mutate(mgt=replace(mgt, grepl(paste0(c("SC_NTcc","CS_NTcc"),collapse='|'),lu_mgt), "CS_NTcc")) %>% 
    mutate(mgt=replace(mgt, grepl("CSWS",lu_mgt), "CSWS")) %>% 
    mutate(mgt=replace(mgt, grepl("CSWcc",lu_mgt), "CSWcc")) %>% 
    filter(!is.na(mgt)) %>% 
    mutate(totp=sedorgp_kgha+surqsolp_kgha+sedmin) %>% 
    select("mgt", "name" ,"sedyld_tha","tilelabp","totp","surqsolp_kgha","hyd_grp", "slp","tile") %>% 
    mutate(scenario='Recent climate (2013-2020) - change landuse')

  # Could use gather instead of melt
  # DF_aghru<-melt(DF_aghru,id=c('hyd_grp','mgt','name','tile','slp','scenario'))
  
  hru_loss<-rbind(hru_loss,DF_aghru)
  
  ### summary for hru loss table ###
  hru_loss_summary<- hru_loss %>% 
    group_by(mgt,scenario)  %>% 
    summarize(sur_solp= mean(surqsolp_kgha,na.rm=T),sed=mean(sedyld_tha,na.rm=T),
              tilep=mean(tilelabp),totp=mean(totp))
  

  hru_loss<-melt(hru_loss,id=c('mgt','name','scenario','tile','slp','hyd_grp'))
  
  # ggplot(hru_loss_boxplots, aes(x=mgt,y=value,fill=scenario))+geom_boxplot()+facet_wrap(vars(variable),scales="free_y",nrow=2)



  hru_loss$variable<-factor(hru_loss$variable, levels=c("sedyld_tha","totp","surqsolp_kgha","tilelabp"),ordered = T)
  
  levels(hru_loss$variable)[levels(hru_loss$variable) == "sedyld_tha"] <- "Sediment loss (t/ha)"
  levels(hru_loss$variable)[levels(hru_loss$variable) == "tilelabp"] <- "Soluble P (subsurface) (kg/ha)"
  levels(hru_loss$variable)[levels(hru_loss$variable) == "surqsolp_kgha"] <- "Soluble P (surface) (kg/ha)"
  levels(hru_loss$variable)[levels(hru_loss$variable) == "totp"] <- "Total P loss (surface + subsurface) (kg/ha)"
  
  hru_loss$mgt<-factor(hru_loss$mgt)
  levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CS_FT"] <- "CB - Full till"
  levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CS_RT"] <- "CB - Reduced Till"
  levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CS_RotT"] <- "CB - Rotational No Till"
  levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CS_NT"] <- "CB - No Till"
  levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CS_NTcc"] <- "CB - No Till with rye cover crop"
  levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CSWS"] <- "CBW / Double crop bean"
  levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CSWcc"] <- "CBW / rye cover crop"
  
  hru_loss$scenario[hru_loss$scenario=='baseline']<-'Baseline - no landuse change'
  hru_loss$scenario[hru_loss$scenario=='hist']<-'Scenario - make changes to landuse'
  

  
  HRU_plot<-hru_loss %>%  group_by(mgt,variable,scenario) %>%  summarize(value=mean(value)) %>%
    ggplot(.,aes(x=mgt,y=value,fill=scenario))+geom_bar(stat='identity',position='dodge')+labs(y="Loss from fields", x="")+facet_wrap(vars(variable),scales="free_y",nrow=2)+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
    panel.background = element_blank(),text = element_text(size = 16),
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1),axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0))

  ################## Read and write baseline yield ########################################
  yield<-read.csv('crop_yield_baseline.csv')
  
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
  DF_lookup<-left_join(DF,hru_lookup, by=c("unit"="id"))
  
  DF_lookup$mgt<-NA
  DF_lookup<-DF_lookup %>% 
    mutate(mgt=replace(mgt, grepl(paste0(c("SC_FT","CS_FT"),collapse='|'),lu_mgt), "CS_FT")) %>% 
    mutate(mgt=replace(mgt, grepl(paste0(c("SC_RT","CS_RT"),collapse='|'),lu_mgt), "CS_RT")) %>% 
    mutate(mgt=replace(mgt, grepl(paste0(c("SC_RotT","CS_RotT"),collapse='|'),lu_mgt), "CS_RotT")) %>% 
    mutate(mgt=replace(mgt, (grepl(paste0(c("SC_NT","CS_NT"),collapse='|'),lu_mgt) & !grepl(paste0(c("SC_NTcc","CS_NTcc"),collapse='|'),lu_mgt)), "CS_NT")) %>% 
    mutate(mgt=replace(mgt, grepl(paste0(c("SC_NTcc","CS_NTcc"),collapse='|'),lu_mgt), "CS_NTcc")) %>% 
    mutate(mgt=replace(mgt, grepl("CSWS",lu_mgt), "CSWS")) %>% 
    mutate(mgt=replace(mgt, grepl("CSWcc",lu_mgt), "CSWcc")) %>% 
    filter(!is.na(mgt),PLANTNM !='rye') 
  
  DF_lookup$tile<-'no tile' 
  DF_lookup$tile[grepl("_t",DF_lookup$lu_mgt)]<- 'tile'

  #convert mass to bu / acre
  DF_lookup$MASS[DF_lookup$PLANTNM=='corn']<-DF_lookup$MASS[DF_lookup$PLANTNM=='corn']/62.77
  DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']<-DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']/67.25
  DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']<-DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']/67.25
  
  
  DF_lookup$scenario<-climatemodel
  
  yield<-rbind(yield,DF_lookup)
  
  yield$scenario[yield$scenario=='baseline']<-'Baseline - no landuse change'
  yield$scenario[yield$scenario=='hist']<-'Scenario - make changes to landuse'
  
  yield_plot<-ggplot(yield,aes(x=PLANTNM,y=MASS,fill=scenario))+geom_boxplot()+ylab("Yield (bu/acre)")+
    xlab("")+theme_bw()
  

  
  
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
    ch_loss$legendkey[ch_loss$legendkey=="userClimScen"]<-"Future climate + landuse change"
    
    
    BR_plot_clim<-ggplot(ch_loss,aes(x=variable,y=change_per))+geom_bar(stat = 'identity')+ylab("Change from baseline (%)")+
      xlab("")+ ggtitle("Change at Berlin Rd")+
      # geom_text(size=16,aes(label=round(change_per)), position=position_dodge(width=0.9), vjust=0.5,colour="black")+
      theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            panel.background = element_blank(),text = element_text(size = 16),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
    
    
    ##### Management rotations / tile plot ######################
    
    hru_loss<-read.csv("hru_baseline.csv")
    
    
    tmp <- file(here(scenario_dir,climatemodel,'hru_ls_yr.txt'))
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
    
    DF_aghru<-left_join(DF,hru_lookup,by=c("name"))
    # DF_aghru<-DF_aghru[grepl(paste0(c("CS","SC","CSW"), collapse="|"),DF_aghru$lu_mgt),]
    
    # index tile columns
    DF_aghru$tile<-'no tile' 
    DF_aghru$tile[grepl("_t",DF_aghru$lu_mgt)]<- 'tile'
    
    # index all mgt scenarios
    DF_aghru$mgt<-NA
    DF_aghru<-DF_aghru %>% 
      mutate(mgt=replace(mgt, grepl(paste0(c("SC_FT","CS_FT"),collapse='|'),lu_mgt), "CS_FT")) %>% 
      mutate(mgt=replace(mgt, grepl(paste0(c("SC_RT","CS_RT"),collapse='|'),lu_mgt), "CS_RT")) %>% 
      mutate(mgt=replace(mgt, grepl(paste0(c("SC_RotT","CS_RotT"),collapse='|'),lu_mgt), "CS_RotT")) %>% 
      mutate(mgt=replace(mgt, (grepl(paste0(c("SC_NT","CS_NT"),collapse='|'),lu_mgt) & !grepl(paste0(c("SC_NTcc","CS_NTcc"),collapse='|'),lu_mgt)), "CS_NT")) %>% 
      mutate(mgt=replace(mgt, grepl(paste0(c("SC_NTcc","CS_NTcc"),collapse='|'),lu_mgt), "CS_NTcc")) %>% 
      mutate(mgt=replace(mgt, grepl("CSWS",lu_mgt), "CSWS")) %>% 
      mutate(mgt=replace(mgt, grepl("CSWcc",lu_mgt), "CSWcc")) %>% 
      filter(!is.na(mgt)) %>% 
      mutate(totp=sedorgp_kgha+surqsolp_kgha+sedmin) %>% 
      select("mgt", "name" ,"sedyld_tha","tilelabp","totp","surqsolp_kgha","hyd_grp", "slp","tile") %>% 
      mutate(scenario=climatemodel)
    
    # Could use gather instead of melt
    # DF_aghru<-melt(DF_aghru,id=c('hyd_grp','mgt','name','tile','slp','scenario'))
    
    hru_loss<-rbind(hru_loss,DF_aghru)
    
    ### summary for hru loss table ###
    hru_loss_summary<- hru_loss %>% 
      group_by(mgt,scenario)  %>% 
      summarize(sur_solp= mean(surqsolp_kgha,na.rm=T),sed=mean(sedyld_tha,na.rm=T),
                tilep=mean(tilelabp),totp=mean(totp))
    
    
    hru_loss<-melt(hru_loss,id=c('mgt','name','scenario','tile','slp','hyd_grp'))
    
    # ggplot(hru_loss_boxplots, aes(x=mgt,y=value,fill=scenario))+geom_boxplot()+facet_wrap(vars(variable),scales="free_y",nrow=2)
    
    
    
    hru_loss$variable<-factor(hru_loss$variable, levels=c("sedyld_tha","totp","surqsolp_kgha","tilelabp"),ordered = T)
    
    levels(hru_loss$variable)[levels(hru_loss$variable) == "sedyld_tha"] <- "Sediment loss (t/ha)"
    levels(hru_loss$variable)[levels(hru_loss$variable) == "tilelabp"] <- "Soluble P (subsurface) (kg/ha)"
    levels(hru_loss$variable)[levels(hru_loss$variable) == "surqsolp_kgha"] <- "Soluble P (surface) (kg/ha)"
    levels(hru_loss$variable)[levels(hru_loss$variable) == "totp"] <- "Total P loss (surface + subsurface) (kg/ha)"
    
    hru_loss$mgt<-factor(hru_loss$mgt)
    levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CS_FT"] <- "CB - Full till"
    levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CS_RT"] <- "CB - Reduced Till"
    levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CS_RotT"] <- "CB - Rotational No Till"
    levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CS_NT"] <- "CB - No Till"
    levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CS_NTcc"] <- "CB - No Till with rye cover crop"
    levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CSWS"] <- "CBW / Double crop bean"
    levels(hru_loss$mgt)[levels(hru_loss$mgt) == "CSWcc"] <- "CBW / rye cover crop"
    
    hru_loss$scenario[hru_loss$scenario=='baseline']<-"Historical climate (1990-2019) - no landuse change"
    hru_loss$scenario[hru_loss$scenario=='userClimScen']<-"Change climate + landuse"
    
    
    
    HRU_plot_clim<-hru_loss %>%  group_by(mgt,variable,scenario) %>%  summarize(value=mean(value)) %>%
      ggplot(.,aes(x=mgt,y=value,fill=scenario))+geom_bar(stat='identity',position='dodge')+labs(y="Loss from fields", x="")+facet_wrap(vars(variable),scales="free_y",nrow=2)+
      theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            panel.background = element_blank(),text = element_text(size = 16),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1),axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0))
    
    ################## Read and write baseline yield ########################################
    yield<-read.csv('crop_yield_baseline.csv')
    
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
    
    DF_lookup<-left_join(DF,hru_lookup, by=c("unit"="id"))
    
    DF_lookup$mgt<-NA
    DF_lookup<-DF_lookup %>% 
      mutate(mgt=replace(mgt, grepl(paste0(c("SC_FT","CS_FT"),collapse='|'),lu_mgt), "CS_FT")) %>% 
      mutate(mgt=replace(mgt, grepl(paste0(c("SC_RT","CS_RT"),collapse='|'),lu_mgt), "CS_RT")) %>% 
      mutate(mgt=replace(mgt, grepl(paste0(c("SC_RotT","CS_RotT"),collapse='|'),lu_mgt), "CS_RotT")) %>% 
      mutate(mgt=replace(mgt, (grepl(paste0(c("SC_NT","CS_NT"),collapse='|'),lu_mgt) & !grepl(paste0(c("SC_NTcc","CS_NTcc"),collapse='|'),lu_mgt)), "CS_NT")) %>% 
      mutate(mgt=replace(mgt, grepl(paste0(c("SC_NTcc","CS_NTcc"),collapse='|'),lu_mgt), "CS_NTcc")) %>% 
      mutate(mgt=replace(mgt, grepl("CSWS",lu_mgt), "CSWS")) %>% 
      mutate(mgt=replace(mgt, grepl("CSWcc",lu_mgt), "CSWcc")) %>% 
      filter(!is.na(mgt),PLANTNM !='rye') 
    
    DF_lookup$tile<-'no tile' 
    DF_lookup$tile[grepl("_t",DF_lookup$lu_mgt)]<- 'tile'
    
    #convert mass to bu / acre
    DF_lookup$MASS[DF_lookup$PLANTNM=='corn']<-DF_lookup$MASS[DF_lookup$PLANTNM=='corn']/62.77
    DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']<-DF_lookup$MASS[DF_lookup$PLANTNM=='soyb']/67.25
    DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']<-DF_lookup$MASS[DF_lookup$PLANTNM=='wwht']/67.25
    
    
    DF_lookup$scenario<-climatemodel
    
    yield<-rbind(yield,DF_lookup)
    
    yield$scenario[yield$scenario=='baseline']<-"Historical climate (1990-2019) - no landuse change"
    yield$scenario[yield$scenario=='userClimScen']<-"Change climate + landuse"
    
    yield_plot_clim<-ggplot(yield,aes(x=PLANTNM,y=MASS,fill=scenario))+geom_boxplot()+ylab("Yield (bu/acre)")+
      xlab("")+theme_bw()
    
    
    ###### Output of mgt as area for visualize results tab #######
    setwd(scenario_dir)
      
    area_output_table<-read.csv("mgt_check.csv")
    
    # area_output_table[,2]<-area_output_table[,2]*3270.16/100 # 3270.16 is the total cropland area in ha
    # area_output_table[,c(3:4)]<-area_output_table[,c(3:4)]/100*area_output_table[,2]
    # 
    # # Convert to acres and round
    # area_output_table[,c(2:4)]<-round(area_output_table[,c(2:4)]*2.47105,1)
    # 
    # area_output_table[,1]<-c('Corn Bean - Full Tillage', 'Corn Bean - No Till', 'Corn Bean - Rotational No Till',
    #                               'Corn Bean - Reduced Till', 'Corn Bean - No Till with rye cover crop',
    #                               'Corn Bean Wheat/Double crop bean','Corn Bean Wheat/rye cover crop')
    # 
    # colnames(area_output_table)<-c('Management scenario','Scenario area','Scenario + vegetated buffer area',
    #                                'Scenario + grassed waterway area')
    
    
    
    
    
  } 
  
    
    if ("hist" %in% SelectClimate & "userClimScenario" %in% SelectClimate){
      
    return(list(print("OWC-SWAT+ run complete"), BR_plot,HRU_plot, yield_plot,BR_plot_clim,HRU_plot_clim,yield_plot_clim,area_output_table))
    
    }else("hist" %in% SelectClimate){
      
      
      return(list(print("OWC-SWAT+ run complete"), BR_plot,HRU_plot, yield_plot,NULL,NULL,NULL,area_output_table))    
      
    }else{
      
      
    }

  
  
  
}