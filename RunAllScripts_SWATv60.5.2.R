RunAllScripts_SWATv60.5.2<-function(SelectClimate,stream_rate,cc_rate,subfert_rate,notill_rate,vfs_rate,local_dir){

  
  ### folders ########
  local_dir<-gsub("\\\\", "/",local_dir)
  
  baseline_path <- paste0(local_dir,"/Baseline")
  scenario_path <- paste0(local_dir,"/Scenarios")

  
  
  set.seed(1)
### ADD CODE BELOW ####
  # copy over orginal files from baseline --done 3/20
  # make changes with ChangeSWATInputs --done 3/20
  # Add error if SelectClimate=empty
  # Don't rewrite files if no changes to the baseline rates have been made
  
### mgt files to copy into each folder ####
mgt_files<-c('hru-data.hru','hyd-sed-lte.cha','hydrology.hyd')
myplots<-list()

if (is.null(SelectClimate)){
 stop('No climate data selected') 
}

# if (CSFT+CSNT+CSRT+CSRot+CSNTcc+CSWS+CSWcc > 100 | CSFT+CSNT+CSRT+CSRot+CSNTcc+CSWS+CSWcc < 100){
#   stop('Management rates do not total 100%')
# }

# add error for buffers + grww?


  
### functions ######
  spaceOutput<-function(data,nspaces){
    
    newData<-paste0(str_dup(" ",(nspaces-nchar(data))),data)
    return(newData)
    
  }
  
  spaceOutput_spacesecond<-function(data,nspaces){
    
    newData<-paste0(data,str_dup(" ",(nspaces-nchar(data))))
    return(newData)
    
  }
  
 ########## Copy over baseline files ################################################################
  file.copy(from = file.path(baseline_path,mgt_files),   # Copy files
            to = file.path(scenario_path,mgt_files),overwrite = T)
  
  
 ########## Rewrite all input files if changes made from baseline ###################################
  
  ############### READ IN DATA ##########################
  
  stream_rate<-stream_rate/100
  
  set.seed(1)
  
  
  ############################### CHANGE DITCH PARAMETERS ########################  
  setwd(baseline_path)
  tmp <- file('hyd-sed-lte.cha')
  open(tmp, "r") #read
  
  #read past headerline and save to rewrite the file
  topOfFile<-readLines(tmp, n = 2) 
  
  #read file 
  data1<-readLines(tmp, n = -1)
  
  
  close(tmp)
  
  #read by spacing 
  DF<-strsplit(data1,split=" ")
  DF<-lapply(DF, function(z){ z[z != ""]}) 
  DF<-data.frame(do.call(rbind, DF)) #unlist
  
  
  ################# NEW COLUMN NAMES ############################################
  
  #old        new
  
  #t_conc     fps
  #shear_bnk  fpn
  #hc_erod    n_conc
  #hc_ht      p_conc
  #hc_len     p_bio
  
  
  colnames(DF)<-c("name","order","wd","dp","slp","len","mann","k","erod_fact","cov_fact","wd_rto","eq_slp","d50","clay","carbon","dry_bd","side_slp",
                  "bed_load","fps","fpn","n_conc","p_conc","p_bio")
  
  
  
  ############## ASSIGN A % OF THE LENGTH ######################################
  
  DF$len_num<-as.numeric(DF$len)
  DF$per_len<-DF$len_num/sum(DF$len_num)
  
  # Convert to a % of length for streams of order 1-2
  # per_streams/total stream length = per_streams_1_2/stream length of order 1-2
  stream_rate<-stream_rate*sum(DF$len_num[DF$order == 1 | DF$order == 2])/sum(DF$len_num)
  
  
  
  ################# EDIT CHANNEL DATA #####################################
  
  #This function generates random rows to replace in the HRU table where the index is true and will meet replacing 
  #enough are to constitute a certain % of the total acres
  #if you sample too large a portion this loop will run forever because it has difficulty reaching the area standard 
  ChangeHRU<-function(hru_data,IndexVal,cropland_area,per_change,area_col){
    
    x=cropland_area*per_change
    #break if cropland_area > 1
    if (per_change > 1){
      stop("percent entered greater than 100")
    }
    
    
    #combine with a row index
    IndexVal<-cbind(IndexVal,c(1:length(IndexVal)),area_col)
    
    #pull row indicies of true values and the associated area
    sampleRow<-IndexVal[IndexVal[,1]==1,c(2,3)]
    
    
    
    
    
    spot<-c()
    area<-0
    i<-1
    #sample until the % area of cropland is met
    #ASSUMING ONCE THE AREA GOES OVER IT DOESN'T EXECUTE THAT LOOP AND JUST EXITS THE LOOP, THAT'S WHY SOME OF MY PERCENTAGES I INPUT ARE SLIGHTLY LOWER THAN INPUT (1-2%)
    #THAN THE OUTPUT
    while(area <= x){ #change to x*1.015 to get closer to the actual input number
      
      #randomly sample per_change value where they are true
      #sampling without replacement has to occur within the one go
      sample1 <- sample(sampleRow[,1],1)
      
      
      if (!is.element(sample1,spot)){ #if statement saying "if is a duplicate in "spot", sample again
        
        
        
        # if (area > x) { # don't add sample if greater than the area
        # next
        # }
        
        spot<-rbind(spot,sample1)
        area<-sampleRow[sampleRow[,1]==spot[i],2]+area
        
        
        i<-i+1
        
        # after so many samples (the number of hrus, although this isn't a guarantee all hrus were sampled)
        if(i > 9409){
          next
        }
        
      }
      
      
      
    }
    spot<-sort.int(spot)
    return(spot)
    
  }
  
  ################# CHANGE X% OF STREAM PARAMS TO REPRESENT A CONSERVATION DITCH ############
  
  if(stream_rate > 0){
    #stream order 1 or 2
    ind<- grepl(paste0(c("1","2"),collapse="|"),DF$order)
    change_ind<-ChangeHRU(DF,ind,sum(DF$len_num), stream_rate,DF$len_num)
    
    DF$erod_fact[change_ind]<-"0.00000" # Based on BOA runs
    DF$bed_load[change_ind] <-"0.3000" # Based on BOA runs # previously had as 0.85 but unsure where I got that number from
    
    
  }
  # also change other related parameters
  
  #DF$mann[grepl("0.0001",DF$erod_fact)]<-"0.00400"
  
  
  ################ REWRITE NEW CHANNEL PARAMS #################################
  
  
  # convert table to characters and strip of whitespace
  DF[] <- lapply(DF, as.character)
  DF <- lapply(DF, str_trim) # keep the first column with the correct spacing
  
  # test before deleting
  # spaceOutput<-function(data,nspaces){
  #   
  #   newData<-paste0(str_dup(" ",(nspaces-nchar(data))),data)
  #   return(newData)
  #   
  # }
  # 
  # spaceOutput_spacesecond<-function(data,nspaces){
  #   
  #   newData<-paste0(data,str_dup(" ",(nspaces-nchar(data))))
  #   return(newData)
  #   
  # }
  
  
  
  #space the first two columns appropriately
  DF[[1]]<-spaceOutput_spacesecond(DF[[1]],29) 
  DF[[2]]<-spaceOutput(DF[[2]],5)
  
  #all other columns have 14 spaces
  for (i in c(3:length(DF))){
    
    DF[[i]] <- spaceOutput(DF[[i]],14)
    
    
  }
  
  
  
  setwd(scenario_path)
  
  #unlink(tmp,force=T)
  file.remove('hyd-sed-lte.cha')
  
  sink('hyd-sed-lte.cha', type=c("output"), append = T)
  
  write(c(topOfFile),'hyd-sed-lte.cha',sep = "\n",append=T)
  
  write(c(paste0(DF[[1]],DF[[2]],DF[[3]],DF[[4]],DF[[5]],DF[[6]],DF[[7]],DF[[8]],DF[[9]],DF[[10]],
                 DF[[11]],DF[[12]],DF[[13]],DF[[14]],DF[[15]],DF[[16]],DF[[17]],DF[[18]],DF[[19]],
                 DF[[20]],DF[[21]],DF[[22]],DF[[23]])),'hyd-sed-lte.cha',sep="\n",append=T)
  
  sink()
  
  ########################### CHANGE MANAGEMENT ######################################################
  
  ##### read hru lookup ######
  hru_data<-read.csv(here('Baseline','hru_lookup.csv'))
  

  # set lu mgt where cropland index is true to only the base management--otherwise addition of numbering for scenarios won't work
  cropland_index<-grepl(paste(c('10','20','30','40','50','63','73','11','21','31','41','51'),collapse='|'),hru_data$lu_mgt)
  hru_data$lu_mgt[cropland_index]<-hru_data$base_mgt[cropland_index]
  
  
  
  
  ##### FUNCTION FOR CHANGING MANAGEMENT #######
  
ChangeMgt<-function(hru_data, name,  num, scenario_rate){
  # hru lookup table, name of practice (cc, nt, ...), numeric identifier, rate of implementation on cropland
  
  # sum exisiting area of cropland with practice
  exisiting_rate<-sum(hru_data$area[!is.na(hru_data[[name]]) ])/3840.16 # 3840.16 = total row crop area
  
  # increase in percentage points based on user input
  add_rate=(scenario_rate/100)-exisiting_rate 
  
  # Index row crop hrus without the practice for random selection
  IndexVal<-is.na(hru_data[[name]]) & cropland_index  
  
  
  # use change HRU function to get indices of where practice should be
  ind<-ChangeHRU(hru_data , IndexVal , 3840.16 , add_rate , hru_data$area_ha) # returns row index as numbers, same as the id column
  
  if (!is.na(num)){
    
  # add numeric code to add practice to mgt rotation if mgt change, otherwise, 
  # change index from NA to 1 to add practice to landuse table
    
  hru_data$lu_mgt[ind]<-paste0(hru_data$lu_mgt[ind], num)
  
  #double check the final rate
  final_rate<-(sum(hru_data$area[!is.na(hru_data[[name]]) ]) + sum(hru_data$area[ind]) ) /3840.16
  
  }else{
  
  # add buffers
  hru_data[[name]][ind]<-1
  
  final_rate<-(sum(hru_data$area[!is.na(hru_data[[name]]) ]) ) /3840.16
    
  }
  

  
  #return the edited hru lookup table and the final implementation rate
  return(list(hru_data,c(paste('the final rate is ', final_rate*100))))
  
}
  
  
  

  # cover crops
  if (cc_rate != 10){
    New_outputs<-ChangeMgt(hru_data,'cc','2',cc_rate)

    hru_data<-New_outputs[[1]]
    output_rate<-New_outputs[[2]]
    print(output_rate)
  }
  
  # sub fert
  if (subfert_rate != 0){
    New_outputs<-ChangeMgt(hru_data,'subfert','4',subfert_rate)
    
    hru_data<-New_outputs[[1]]
    output_rate<-New_outputs[[2]]
    print(output_rate)
  } 
  

  # no till
  if (notill_rate != 60){
    New_outputs<-ChangeMgt(hru_data,'nt','1',notill_rate)
    
    hru_data<-New_outputs[[1]]
    output_rate<-New_outputs[[2]]
    print(output_rate)
  } 
  
  
  # buffers
  if (vfs_rate != 35){
    New_outputs<-ChangeMgt(hru_data,'buffers',NA,vfs_rate)
    
    hru_data<-New_outputs[[1]]
    output_rate<-New_outputs[[2]]
    print(output_rate)
  } 
  
  
  
  
  ##### re - add existing or new physical practices to mgt code ######

  addtileInd<-which((hru_data$tile==1) & !grepl("_t",hru_data$lu_mgt))
  hru_data$lu_mgt[addtileInd]<-paste0(hru_data$lu_mgt[addtileInd],"_t")
  
  # add grww to any changed hrus that had grww
  addgrwwInd<-!grepl("_G",hru_data$lu_mgt) & grepl('G',hru_data$grww)
  
  # add grww
  hru_data$lu_mgt[grepl('GL',hru_data$grww) & addgrwwInd]<-paste0(hru_data$lu_mgt[grepl('GL',hru_data$grww) & addgrwwInd],'_GL')
  hru_data$lu_mgt[grepl('GM',hru_data$grww) & addgrwwInd]<-paste0(hru_data$lu_mgt[grepl('GM',hru_data$grww) & addgrwwInd],'_GM')
  hru_data$lu_mgt[grepl('GH',hru_data$grww) & addgrwwInd]<-paste0(hru_data$lu_mgt[grepl('GH',hru_data$grww) & addgrwwInd],'_GH')
  
  # add buffers to any changed HRUs
  addbuffInd<- hru_data$buffers %in% 1
  
  # add buffers
  hru_data$lu_mgt[!is.na(hru_data$buffers) & addbuffInd]<-paste0(hru_data$lu_mgt[!is.na(hru_data$buffers) & addbuffInd],'_B')
  
  
  
  # 
  # 
  # ########################## CONVERT MANAGEMENT RATES TO DECIMAL PERCENT ##############
  # # CSFT<-CSFT/100
  # # CSNT<-CSNT/100
  # # CSRT<-CSRT/100
  # # CSRot<-CSRot/100
  # # CSNTcc<-CSNTcc/100
  # # CSWS<-CSWS/100
  # # CSWcc<-CSWcc/100
  # 
  # mgt<-data.frame(matrix(nrow=12,ncol=4))
  # colnames(mgt)<-c("name","rate","vfs_rate","grww_rate")
  # 
  # mgt$name<-c("CS_FT","CS_RT","CS_RotT","CS_NT","CS_NTcc","CSWS","CSWcc","SC_FT","SC_RT","SC_RotT","SC_NT","SC_NTcc")
  # # don't know if I need all this, could just append the base name
  # # mgt$name_t<-c("CS_FT_t","CS_RT_t","CS_RotT_t","CS_NT_t","CS_NTcc_t","CSWS_t","CSWcc_t", 
  # # "SC_FT_t","SC_RT_t","SC_RotT_t","SC_NT_t","SC_NTcc_t")
  # 
  # #mgt
  # mgt$rate[mgt$name=="CS_FT"]<-(CSFT/2)/100
  # mgt$rate[mgt$name=="SC_FT"]<-(CSFT/2)/100
  # 
  # mgt$rate[mgt$name=="CS_RT"]<-(CSRT/2)/100
  # mgt$rate[mgt$name=="SC_RT"]<-(CSRT/2)/100
  # 
  # mgt$rate[mgt$name=="CS_RotT"]<-(CSRot/2)/100
  # mgt$rate[mgt$name=="SC_RotT"]<-(CSRot/2)/100
  # 
  # mgt$rate[mgt$name=="CS_NT"]<-(CSNT/2)/100
  # mgt$rate[mgt$name=="SC_NT"]<-(CSNT/2)/100
  # 
  # mgt$rate[mgt$name=="CS_NTcc"]<-(CSNTcc/2)/100
  # mgt$rate[mgt$name=="SC_NTcc"]<-(CSNTcc/2)/100
  # 
  # mgt$rate[mgt$name=="CSWS"]<-(CSWS)/100
  # 
  # mgt$rate[mgt$name=="CSWcc"]<-(CSWcc)/100
  # 
  # #buffers
  # #remove dividing by 2 bc it's halving the total rate
  # mgt$vfs_rate[mgt$name=="CS_FT"]<-(CSFT_B)/100
  # mgt$vfs_rate[mgt$name=="SC_FT"]<-(CSFT_B)/100
  # 
  # mgt$vfs_rate[mgt$name=="CS_RT"]<-(CSRT_B)/100
  # mgt$vfs_rate[mgt$name=="SC_RT"]<-(CSRT_B)/100
  # 
  # mgt$vfs_rate[mgt$name=="CS_RotT"]<-(CSRot_B)/100
  # mgt$vfs_rate[mgt$name=="SC_RotT"]<-(CSRot_B)/100
  # 
  # mgt$vfs_rate[mgt$name=="CS_NT"]<-(CSNT_B)/100
  # mgt$vfs_rate[mgt$name=="SC_NT"]<-(CSNT_B)/100
  # 
  # mgt$vfs_rate[mgt$name=="CS_NTcc"]<-(CSNTcc_B)/100
  # mgt$vfs_rate[mgt$name=="SC_NTcc"]<-(CSNTcc_B)/100
  # 
  # mgt$vfs_rate[mgt$name=="CSWS"]<-(CSWS_B)/100
  # 
  # mgt$vfs_rate[mgt$name=="CSWcc"]<-(CSWcc_B)/100
  # 
  # #grassed waterways
  # mgt$grww_rate[mgt$name=="CS_FT"]<-(CSFT_GW)/100
  # mgt$grww_rate[mgt$name=="SC_FT"]<-(CSFT_GW)/100
  # 
  # mgt$grww_rate[mgt$name=="CS_RT"]<-(CSRT_GW)/100
  # mgt$grww_rate[mgt$name=="SC_RT"]<-(CSRT_GW)/100
  # 
  # mgt$grww_rate[mgt$name=="CS_RotT"]<-(CSRot_GW)/100
  # mgt$grww_rate[mgt$name=="SC_RotT"]<-(CSRot_GW)/100
  # 
  # mgt$grww_rate[mgt$name=="CS_NT"]<-(CSNT_GW)/100
  # mgt$grww_rate[mgt$name=="SC_NT"]<-(CSNT_GW)/100
  # 
  # mgt$grww_rate[mgt$name=="CS_NTcc"]<-(CSNTcc_GW)/100
  # mgt$grww_rate[mgt$name=="SC_NTcc"]<-(CSNTcc_GW)/100
  # 
  # mgt$grww_rate[mgt$name=="CSWS"]<-(CSWS_GW)/100
  # 
  # mgt$grww_rate[mgt$name=="CSWcc"]<-(CSWcc_GW)/100
  # 
  # #remove rows where mgt rate is 0
  # mgt<-mgt[mgt$rate > 0,]
  # 
  # ############### READ IN HRU-DATA ##########################
  # setwd(baseline)
  # tmp <- file('hru-data.hru')
  # open(tmp, "r") #read
  # 
  # #read past headerline and save to rewrite the file
  # topOfFile<-readLines(tmp, n = 2) 
  # 
  # #read file 
  # data1<-readLines(tmp, n = -1)
  # 
  # close(tmp)
  # 
  # #read by spacing 
  # id<-substr(data1, 1,8)
  # name<-substr(data1, 9,17)
  # topo<-substr(data1, 18,44)
  # hydro<-substr(data1, 45,62)
  # soil<-substr(data1, 63,80)
  # lu_mgt<-substr(data1, 82,98)
  # soil_plant_init<-substr(data1, 100,116)
  # surf_stor<-substr(data1, 118,134)
  # snow<-substr(data1, 136,152)
  # field<-substr(data1, 154,170)
  # 
  # hru_data<-data.frame(id,name,topo,hydro,soil,lu_mgt,soil_plant_init,surf_stor,snow,field)
  # 
  # ################## READ IN SOILS DATA ###############################
  # #setwd('..')
  # SSURGO<-read.table("SSURGO.txt",header=T,sep="\t")
  # # currently I only need hydr grp and muid
  # SSURGO<-SSURGO[,c(2,6)]
  # colnames(hru_data)[5]<-"muid"
  # hru_data$muid<-as.integer(hru_data$muid)
  # 
  # hru_data<-left_join(hru_data,SSURGO,by="muid")
  # 
  # 
  # ################# READ IN HRU AREA ##################################
  # 
  # ##Pull area of each hru from hru.con
  # tmp <- file('hru.con')
  # open(tmp, "r") #read
  # readLines(tmp, n = 2) 
  # 
  # HRUarea<-readLines(tmp, n = -1) 
  # HRUarea<-tibble(substr(HRUarea,11,17),substr(HRUarea,37,50)) # space 44-50 to 37-50, fixed 9/14/23
  # colnames(HRUarea)<-c("name","area_ha")
  # 
  # close(tmp)
  # 
  # #remove spaces so the name characters match
  # hru_data$name<-str_replace_all(hru_data$name, fixed(" "), "")
  # HRUarea$name<-str_replace_all(HRUarea$name, fixed(" "), "")
  # 
  # hru_data<-left_join(hru_data,HRUarea,by="name")
  # hru_data$area_ha<-as.numeric(hru_data$area_ha)
  # 
  # #total cropland area
  # cropland_area<-sum(hru_data$area_ha[grepl("corn_lum",hru_data$lu_mgt) | grepl("soyb_lum",hru_data$lu_mgt) | grepl(paste(c("CS","SC"),collapse="|"),hru_data$lu_mgt)])
  # print(paste("cropland area is", cropland_area, "ha")) #testing
  # ################# READ IN HRU SLOPES ###############################
  # 
  # ##Pull slope of each hru from topography.hyd
  # tmp <- file('topography.hyd')
  # open(tmp, "r") #read
  # readLines(tmp, n = 2) 
  # 
  # HRUslp<-readLines(tmp, n = -1) 
  # HRUslp<-tibble(substr(HRUslp,1,11),substr(HRUslp,24,32))
  # colnames(HRUslp)<-c("topo","slp")
  # 
  # 
  # hru_data$topo <- lapply(hru_data$topo, str_trim) # trim white space 
  # hru_data$topo<-unlist(hru_data$topo)
  # hru_data<-left_join(hru_data,HRUslp,by="topo")
  # hru_data$slp<-as.numeric(hru_data$slp)
  # 
  # close(tmp)
  # 
  # ################# EDIT HRU DATA #####################################
  # 
  # #replace all data with a generic filler
  # hru_data$lu_mgt[grepl(paste(c("CS","SC","corn_lum","soyb_lum"),collapse="|"),hru_data$lu_mgt)]<-"rowcrop_lum"
  # 
  # ###### loop for base mgt #################################################
  # 
  # for(mgt_nm in mgt$name[-length(mgt$name)]){
  #   
  #   rt<-mgt$rate[mgt$name == mgt_nm]
  #   if (rt > 0){
  #     IndexVal<-grepl("^rowcrop_lum$",hru_data$lu_mgt)
  #     hru_data$lu_mgt[ChangeHRU(hru_data , IndexVal , cropland_area , rt , hru_data$area_ha)]<-paste0(mgt_nm,"_t")
  #   }
  #   
  # }
  # 
  # #replace remaining with the last mgt rotation or the loop will hang
  # mgt_nm<-mgt$name[length(mgt$name)]
  # rt<-mgt$rate[mgt$name == mgt_nm]
  # 
  # if (rt > 0){
  #   IndexVal<-grepl("^rowcrop_lum$",hru_data$lu_mgt)
  #   hru_data$lu_mgt[IndexVal]<-paste0(mgt_nm,"_t")
  # }
  # ##### add grww ##########################################################
  # for(mgt_nm in mgt$name[-length(mgt$name)]){
  #   
  #   if (mgt$rate[mgt$name == mgt_nm] > 0){
  #     rt<-mgt$grww_rate[mgt$name == mgt_nm]
  #     if (rt > 0){ #prevent from adding to very small fields
  #       
  #       IndexVal<-grepl(paste0("^",mgt_nm,"_t$"),hru_data$lu_mgt)
  #       mgt_area<-cropland_area*mgt$rate[mgt$name==mgt_nm]
  #       hru_data$lu_mgt[ChangeHRU(hru_data , IndexVal , mgt_area , rt , hru_data$area_ha)]<-paste0(mgt_nm,"_t_G") 
  #       
  #     }
  #   }
  # }
  # 
  # #replace remaining with the last mgt rotation or the loop will hang
  # mgt_nm<-mgt$name[length(mgt$name)]
  # 
  # if(mgt$rate[mgt$name == mgt_nm] > 0){
  #   
  #   rt<-mgt$grww_rate[mgt$name == mgt_nm]
  #   
  #   if (rt > 0){
  #     IndexVal<-grepl(paste0("^",mgt_nm,"_t$"),hru_data$lu_mgt)
  #     mgt_area<-cropland_area*mgt$rate[mgt$name==mgt_nm]
  #     hru_data$lu_mgt[ChangeHRU(hru_data , IndexVal , mgt_area , rt , hru_data$area_ha)]<-paste0(mgt_nm,"_t_G") 
  #   } 
  # }
  # # add LS, MS, or HS based on slope
  # IndexVal<-(grepl("_G",hru_data$lu_mgt) & (hru_data$slp <=  0.02))
  # hru_data$lu_mgt[IndexVal]<-paste0(hru_data$lu_mgt[IndexVal],"L") # Low slope buffers #
  # 
  # #medium
  # IndexVal<-(grepl("_G",hru_data$lu_mgt) & (0.02 < hru_data$slp & hru_data$slp <=  0.08))
  # hru_data$lu_mgt[IndexVal]<-paste0(hru_data$lu_mgt[IndexVal],"M") # Medium slope buffers #
  # 
  # #high
  # IndexVal<-(grepl("_G",hru_data$lu_mgt) & (0.08 < hru_data$slp))
  # hru_data$lu_mgt[IndexVal]<-paste0(hru_data$lu_mgt[IndexVal],"H") # High slope buffers #
  # 
  # ##### add buffers ##########################################################
  # for(mgt_nm in mgt$name[-length(mgt$name)]){
  #   if (mgt$rate[mgt$name == mgt_nm] > 0){ #don't enter loop if no mgt scenario
  #     rt<-mgt$vfs_rate[mgt$name == mgt_nm]  
  #     
  #     if (rt > 0){
  #       IndexVal<-grepl(paste0("^",mgt_nm,"_t$"),hru_data$lu_mgt)
  #       mgt_area<-cropland_area*mgt$rate[mgt$name==mgt_nm]
  #       hru_data$lu_mgt[ChangeHRU(hru_data , IndexVal , mgt_area , rt , hru_data$area_ha)]<-paste0(mgt_nm,"_t_B") 
  #       
  #     }
  #   }
  # }
  # 
  # #replace remaining with the last mgt rotation or the loop will hang
  # mgt_nm<-mgt$name[length(mgt$name)]
  # if (mgt$rate[mgt$name == mgt_nm] > 0){ 
  #   rt<-mgt$vfs_rate[mgt$name == mgt_nm]
  #   
  #   if (rt > 0){
  #     IndexVal<-grepl(paste0("^",mgt_nm,"_t$"),hru_data$lu_mgt)
  #     mgt_area<-cropland_area*mgt$rate[mgt$name==mgt_nm]
  #     hru_data$lu_mgt[ChangeHRU(hru_data , IndexVal , mgt_area , rt , hru_data$area_ha)]<-paste0(mgt_nm,"_t_B") 
  #   }
  # }
  # 
  # ###### check buffers #################################################################
  # # mgt_check<-hru_data %>%
  # # group_by(lu_mgt) %>%
  # # summarize(value=sum(area_ha,na.rm=T))
  # 
  # # mgt_check$percent<-round(mgt_check$value*100/cropland_area,2)
  # 
  # # output_buffer_rate<-sum(mgt_check$value[grepl("_B",mgt_check$lu_mgt)]*100/cropland_area)
  # # input_buffer_rate<-sum(mgt$rate*100*mgt$vfs_rate)
  # 
  # # buffer_error<-output_buffer_rate-input_buffer_rate
  # 
  # # output_grww_rate<-sum(mgt_check$value[grepl("_G",mgt_check$lu_mgt)]*100/cropland_area)
  # # input_grww_rate<-sum(mgt$rate*100*mgt$grww_rate)
  # 
  # # grww_error<-output_grww_rate-input_grww_rate
  # 
  # ############ REMOVE TILES #################################
  # rmtileInd<-( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
  # 
  # hru_data$lu_mgt[rmtileInd]<-str_remove(hru_data$lu_mgt[rmtileInd],"_t")
  # 
  # 
  # ############ CHECK % OF FINAL MGT ##############################
  # 
  # # mgt_check<-hru_data %>%
  # # group_by(lu_mgt) %>%
  # # summarize(value=sum(area_ha,na.rm=T))
  # 
  # mgt_check<-data.frame(matrix(nrow=7,ncol=4))
  # colnames(mgt_check)<-c("rot","rate","buffers","grww")
  # mgt_check$rot<-c("CSFT","CSNT","CSRotT","CSRT","CSNTcc","CSWS","CSWcc")
  # 
  # area_CSFT<-sum(hru_data$area_ha[grepl(paste(c("CS_FT","SC_FT"),collapse="|"),hru_data$lu_mgt)])
  # area_CSNT<-sum(hru_data$area_ha[grepl(paste(c("CS_NT","SC_NT"),collapse="|") ,hru_data$lu_mgt) & !grepl(paste(c("CS_NTcc","SC_NTcc"),collapse="|") ,hru_data$lu_mgt)])
  # area_CSRT<-sum(hru_data$area_ha[grepl(paste(c("CS_RT","SC_RT"),collapse="|"),hru_data$lu_mgt)])
  # area_CSRotT<-sum(hru_data$area_ha[grepl(paste(c("CS_RotT","SC_RotT"),collapse="|"),hru_data$lu_mgt)])
  # area_CSNTcc<-sum(hru_data$area_ha[grepl(paste(c("CS_NTcc","SC_NTcc"),collapse="|"),hru_data$lu_mgt)])
  # area_CSWS<-sum(hru_data$area_ha[grepl(c("CSWS"),hru_data$lu_mgt)])
  # area_CSWcc<-sum(hru_data$area_ha[grepl(c("CSWcc"),hru_data$lu_mgt)])
  # 
  # mgt_check$rate[mgt_check$rot=="CSFT"]<-area_CSFT*100/cropland_area
  # mgt_check$rate[mgt_check$rot=="CSNT"]<-area_CSNT*100/cropland_area
  # mgt_check$rate[mgt_check$rot=="CSRT"]<-area_CSRT*100/cropland_area
  # mgt_check$rate[mgt_check$rot=="CSRotT"]<-area_CSRotT*100/cropland_area
  # mgt_check$rate[mgt_check$rot=="CSNTcc"]<-area_CSNTcc*100/cropland_area
  # mgt_check$rate[mgt_check$rot=="CSWS"]<-area_CSWS*100/cropland_area
  # mgt_check$rate[mgt_check$rot=="CSWcc"]<-area_CSWcc*100/cropland_area
  # 
  # 
  # mgt_check$buffers[mgt_check$rot=="CSFT"]<-sum(hru_data$area_ha[grepl(paste(c("CS_FT","SC_FT"),collapse="|"),hru_data$lu_mgt) & grepl("_B",hru_data$lu_mgt)])*100/area_CSFT
  # mgt_check$buffers[mgt_check$rot=="CSNT"]<-sum(hru_data$area_ha[grepl(paste(c("CS_NT","SC_NT"),collapse="|") ,hru_data$lu_mgt) & !grepl(paste(c("CS_NTcc","SC_NTcc"),collapse="|") ,hru_data$lu_mgt) & grepl("_B",hru_data$lu_mgt)])*100/area_CSNT
  # mgt_check$buffers[mgt_check$rot=="CSRT"]<-sum(hru_data$area_ha[grepl(paste(c("CS_RT","SC_RT"),collapse="|"),hru_data$lu_mgt) & grepl("_B",hru_data$lu_mgt)])*100/area_CSRT
  # mgt_check$buffers[mgt_check$rot=="CSRotT"]<-sum(hru_data$area_ha[grepl(paste(c("CS_RotT","SC_RotT"),collapse="|"),hru_data$lu_mgt) & grepl("_B",hru_data$lu_mgt)])*100/area_CSRotT
  # mgt_check$buffers[mgt_check$rot=="CSNTcc"]<-sum(hru_data$area_ha[grepl(paste(c("CS_NTcc","SC_NTcc"),collapse="|"),hru_data$lu_mgt) & grepl("_B",hru_data$lu_mgt)])*100/area_CSNTcc
  # mgt_check$buffers[mgt_check$rot=="CSWS"]<-sum(hru_data$area_ha[grepl(c("CSWS"),hru_data$lu_mgt) & grepl("_B",hru_data$lu_mgt)])*100/area_CSWS
  # mgt_check$buffers[mgt_check$rot=="CSWcc"]<-sum(hru_data$area_ha[grepl(c("CSWcc"),hru_data$lu_mgt) & grepl("_B",hru_data$lu_mgt)])*100/area_CSWcc
  # 
  # 
  # mgt_check$grww[mgt_check$rot=="CSFT"]<-sum(hru_data$area_ha[grepl(paste(c("CS_FT","SC_FT"),collapse="|"),hru_data$lu_mgt) & grepl("_G",hru_data$lu_mgt)])*100/area_CSFT
  # mgt_check$grww[mgt_check$rot=="CSNT"]<-sum(hru_data$area_ha[grepl(paste(c("CS_NT","SC_NT"),collapse="|") ,hru_data$lu_mgt) & !grepl(paste(c("CS_NTcc","SC_NTcc"),collapse="|") ,hru_data$lu_mgt) & grepl("_G",hru_data$lu_mgt)])*100/area_CSNT
  # mgt_check$grww[mgt_check$rot=="CSRT"]<-sum(hru_data$area_ha[grepl(paste(c("CS_RT","SC_RT"),collapse="|"),hru_data$lu_mgt) & grepl("_G",hru_data$lu_mgt)])*100/area_CSRT
  # mgt_check$grww[mgt_check$rot=="CSRotT"]<-sum(hru_data$area_ha[grepl(paste(c("CS_RotT","SC_RotT"),collapse="|"),hru_data$lu_mgt) & grepl("_G",hru_data$lu_mgt)])*100/area_CSRotT
  # mgt_check$grww[mgt_check$rot=="CSNTcc"]<-sum(hru_data$area_ha[grepl(paste(c("CS_NTcc","SC_NTcc"),collapse="|"),hru_data$lu_mgt) & grepl("_G",hru_data$lu_mgt)])*100/area_CSNTcc
  # mgt_check$grww[mgt_check$rot=="CSWS"]<-sum(hru_data$area_ha[grepl(c("CSWS"),hru_data$lu_mgt) & grepl("_G",hru_data$lu_mgt)])*100/area_CSWS
  # mgt_check$grww[mgt_check$rot=="CSWcc"]<-sum(hru_data$area_ha[grepl(c("CSWcc"),hru_data$lu_mgt) & grepl("_G",hru_data$lu_mgt)])*100/area_CSWcc
  # 
  # 
  # 
  # # colnames(mgt_check)<-c("lum","area ha") 
  # write.csv(mgt_check,paste0(scenario_dir,"\\mgt_check.csv"),row.names=F) #testing
  
  
  ############ WRITE LOOKUP TABLE FOR PLOTTING OUTPUTS ############
  write.csv(hru_data,paste0(scenario_path,"\\hru_lookup_scenario.csv"),row.names=F)
  
  
  ############ WRITE NEW HRU-DATA.HRU #############################
  # convert table to characters and strip of whitespace
  hru_data[] <- lapply(hru_data, as.character)
  hru_data[,-c(1)] <- lapply(hru_data[,-c(1)], str_trim) # keep the first column with the correct spacing
  
  #change muid back to soil
  names(hru_data)[names(hru_data) == 'muid'] <- 'soil'
  
  #1-8 id
  
  #9-17 name, 9 spaces
  
  spaceOutput<-function(data,nspaces){
    
    newData<-paste0(str_dup(" ",(nspaces-nchar(data))-1),data)
    return(newData)
    
  }
  
  #18-44 hydro
  #45-62 soil
  #63-80 lu_mgt
  #81-98 soil_plant_init
  #99-116 surf_stor
  #117-134 snow
  #135-152 field
  hru_data$id<-spaceOutput(hru_data$id,9)
  hru_data$name<-spaceOutput(hru_data$name,9)
  hru_data$topo<-spaceOutput(hru_data$topo,27)
  hru_data$hydro<-spaceOutput(hru_data$hydro,18)
  hru_data$soil<-spaceOutput(hru_data$soil,18)
  hru_data$lu_mgt<-spaceOutput(hru_data$lu_mgt,18)
  hru_data$soil_plant_init<-spaceOutput(hru_data$soil_plant_init,18)
  hru_data$surf_stor<-spaceOutput(hru_data$surf_stor,18)
  hru_data$snow<-spaceOutput(hru_data$snow,18)
  hru_data$field<-spaceOutput(hru_data$field,18)
  
  topOfFile<-c('hru-data.hru: written by SWAT+ editor v2.0.4 on 2022-01-21 12:22
      id  name                          topo             hydro              soil            lu_mgt   soil_plant_init         surf_stor              snow             field  ')
  
  
  setwd(scenario_path)
  file.remove('hru-data.hru')
  sink('hru-data.hru', type=c("output"), append = T)
  write(c(topOfFile),'hru-data.hru',sep = "\n",append=T)
  write(c(paste(hru_data$id,hru_data$name,hru_data$topo,hru_data$hydro,hru_data$soil,
                hru_data$lu_mgt,hru_data$soil_plant_init,hru_data$surf_stor,hru_data$snow, hru_data$field)),'hru-data.hru',sep="\n",append=T)
  sink()
  
  print('I should have wrote the new hru file by now :)')
  
  ##################### CHANGE HRU PARAMETERS TO MATCH ROTATIONS ##########################
  ############### READ IN DATA ##########################
  
  tmp <- file('hydrology.hyd')
  open(tmp, "r") #read
  
  #read past headerline and save to rewrite the file
  topOfFile<-readLines(tmp, n = 2) 
  
  #read file 
  data1<-readLines(tmp, n = -1)
  
  
  close(tmp)
  headers<-c("name", "lat_ttime", "lat_sed", "can_max", "esco", "epco", "orgn_enrich", "orgp_enrich",   "cn3_swf",    "bio_mix", "perco",  "lat_orgn",
             "lat_orgp",  "harg_pet", "latq_co")
  
  #read by spacing 
  DF<-strsplit(data1,split=" ")
  DF<-lapply(DF, function(z){ z[z != ""]}) 
  DF<-data.frame(do.call(rbind, DF)) #unlist
  colnames(DF)<-headers
  
  ################## READ IN SOILS DATA ###############################
  #setwd('..')
  #SSURGO<-read.table("SSURGO.txt",header=T,sep="\t")
  # currently I only need hydr grp and muid
  #SSURGO<-SSURGO[,c(2,6)]
  #colnames(hru_data)[5]<-"muid"
  #hru_data$muid<-as.integer(hru_data$muid)
  
  #hru_data<-left_join(hru_data,SSURGO,by="muid")
  
  ################# HRU LOOKUP ########################################
  # merge hru params and hru data to only change ag hru params
  
  # remove white space from hydro in hru-data.hru
  hru_data$hydro<-str_replace_all(hru_data$hydro, fixed(" "), "")
  
  hru_data<-left_join(DF,hru_data,by=c("name"="hydro"))
  
  
  ############## CHANGE PERCO based on land use ############################
  # increase CN --> set low (0.05)
  # lower CN --> set high (0.95)
  
  hru_data$perco[grepl("frsd_lum",hru_data$lu_mgt)]<-"0.95000" #stayed the same
  hru_data$perco[grepl("past_lum",hru_data$lu_mgt)]<-"0.70000" #had at 0.7
  hru_data$perco[grepl("urml_lum",hru_data$lu_mgt)]<-"0.05000"
  hru_data$perco[grepl(paste(c("10","11"), collapse='|'),substr(hru_data$lu_mgt,1,2))]<-"0.10000" # CS FT
  
  #rotations with no conservation
  hru_data$perco[grepl(paste(c("40","41","50","51","63","73"), collapse='|'),substr(hru_data$lu_mgt,1,2))]<-"0.40000" #had all conservation at 0.4
  hru_data$perco[grepl(paste(c("20","21","30","31"), collapse='|'),substr(hru_data$lu_mgt,1,2))]<-"0.20000" #these were at 0.2
  
  
  hru_data$perco[grepl("3",substr(hru_data$lu_mgt,3,nchar(hru_data$lu_mgt)))]<-"0.10000" # decr perco parameters for full tillage
  hru_data$perco[grepl("1",substr(hru_data$lu_mgt,3,nchar(hru_data$lu_mgt))) ]<-"0.40000" # incr perco if changing to no-till

  
  DF<-hru_data[,c(1:15)]

  
  ################ REWRITE NEW HRU PARAMS #################################
  # convert table to characters and strip of whitespace
  DF[] <- lapply(DF, as.character)
  DF <- lapply(DF, str_trim) # keep the first column with the correct spacing
  
  
  spaceOutput<-function(data,nspaces){
    
    newData<-paste0(str_dup(" ",(nspaces-nchar(data))),data)
    return(newData)
    
  }
  
  spaceOutput_spacesecond<-function(data,nspaces){
    
    newData<-paste0(data,str_dup(" ",(nspaces-nchar(data))))
    return(newData)
    
  }
  
  
  
  #space the first two columns appropriately
  DF[[1]]<-spaceOutput_spacesecond(DF[[1]],21) 
  DF[[2]]<-spaceOutput(DF[[2]],9)
  
  #all other columns have 14 spaces
  for (i in c(3:length(DF))){
    
    DF[[i]] <- spaceOutput(DF[[i]],14)
    
    
  }
  
  setwd(scenario_path)
  
  #unlink(tmp,force=T)
  file.remove('hydrology.hyd')
  
  sink('hydrology.hyd', type=c("output"), append = T)
  
  write(c(topOfFile),'hydrology.hyd',sep = "\n",append=T)
  
  write(c(paste0(DF[[1]],DF[[2]],DF[[3]],DF[[4]],DF[[5]],DF[[6]],DF[[7]],DF[[8]],DF[[9]],DF[[10]],
                 DF[[11]],DF[[12]],DF[[13]],DF[[14]],DF[[15]])),'hydrology.hyd',sep="\n",append=T)
  
  sink()
  
  
  
  ######### Run all selected climate options ################################################
  if (any(grep(paste(c("userClimScen","hist"),collapse="|"),SelectClimate))){
    
    
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
        
        # 4/25 ideally, would move code from ClimateChange to here for if userClimScen selected, put together daily clim and write swat files here
      
                  # Instead of running the baseline historical with the new mgt--only compare with baseline climate run (1980-1999) with historical management (2013-2020)
                  # In Likely Adoption project we got weird % changes if we compared LA historical with LA future. The absolute change was smaller than the baseline, but the % change would be larger
                  
                              # file.copy(from = file.path(paste0(scenario_dir,"/", my_files)),   # Copy files
                              # to = file.path(paste0(scenario_dir,'/climate','/',climatemodel,'/historical',"/", my_files)))
      
                              file.copy(from = file.path(paste0(scenario_path,"/", mgt_files)),   # Copy files
                              to = file.path(paste0(scenario_path,'/',climatemodel,"/", mgt_files)),overwrite=T)
                              
                              print(paste0("running ",climatemodel))
                              
                              #These would also need to be combined as one function to have every single run running in parallel. Keep for now.
                              # setwd(paste0(scenario_dir,'/climate','/',climatemodel,'/historical'))
                              # system('SWATPlus_60.5.5.exe',ignore.stdout = T,ignore.stderr = T) #run executable
                              
                              setwd(paste0(scenario_path,'/',climatemodel))
                              system('SWATPlus_60.5.5.exe',ignore.stdout = T,ignore.stderr = T) #run executable
                              
                              # Moved to testPlot
                              ### Read in channel data and compare with baseline ####
                              
                              # headers<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"areaha",	"precipha.m",	"evapha.m",	
                              #            "seepha.m",	"flo_storm.3.s",	"sed_stormtons",	"orgn_storkgN",	"sedp_storkgP",	"no3_storkgN",	"solp_storkgP",
                              #            "chla_storkg",	"nh3_storkgN",	"no2_storkgN",	"cbod_storkg",	"dox_storkg",	"san_stortons",	"sil_stortons",	"cla_stortons",	"sag_stortons",
                              #            "lag_stortons",	"grv_stortons",	"null1", "setl_stor",	"setlp_stor",	"flo_inm.3.s",	"sed_inmtons",	"orgn_inkgN",	"sedp_inkgP",	"no3_inkgN",
                              #            "solp_inkgP",	"chla_inkg",	"nh3_inkgN",	"no2_inkgN",	"cbod_inkg",	"dox_inkg",	"san_intons",	"sil_intons",	"cla_intons",
                              #            "sag_intons",	"lag_intons",	"grv_intons",	"null",	 "setl_in",	"setlp_in","flo_outm.3.s",	"sed_outmtons",	"orgn_outkgN",	"sedp_outkgP",	"no3_outkgN",
                              #            "solp_outkgP",	"chla_outkg",	"nh3_outkgN",	"no2_outkgN",	"cbod_outkg",	"dox_outkg",	"san_outtons",	"sil_outtons",	"cla_outtons",
                              #            "sag_outtons",	"lag_outtons",	"grv_outtons",	"null2", "setl_out",	"setlp_out", "water_tempdegC")#"null3","null4","null5","null6","null7")
                              # 
                              # 
                              # tmp <- file('channel_sd_yr.txt')
                              # open(tmp, "r") #read
                              # 
                              # #read past headerlines
                              # readLines(tmp, n = 3) 
                              # 
                              # DF<-readLines(tmp,n=-1)
                              # 
                              # close(tmp)
                              # DF<-strsplit(DF,split=" ") #split based on spacing
                              # DF<-lapply(DF, function(z){ z[z != ""]}) # remove empty spaces
                              # DF<-data.frame(do.call(rbind, DF)) #unlist
                              # colnames(DF)<-headers
                              # 
                              # # Berlin Rd 
                              # DF<-DF%>%
                              #   filter(gis_id=="46")
                              # 
                              # DF[,c(1:6,8:(ncol(DF)-1))]<-DF[,c(1:6,8:(ncol(DF)-1))]%>%
                              #   unlist()%>%
                              #   as.numeric()
                              # 
                              # #### Read in baseline data #####
                              # baseline_data<-read.csv("baseline_data_avg.csv")
                              # 
                              # ################ Summarize outputs and compare to baseline ############################################################
                              # baseline_data$scenario[baseline_data$variable=="discharge_cms"]<-mean(DF$flo_outm.3.s,na.rm=T)
                              # baseline_data$scenario[baseline_data$variable=="solp_kg"]<-sum(DF$solp_outkgP,na.rm=T)
                              # baseline_data$scenario[baseline_data$variable=="sedp_kg"]<-sum(DF$sedp_outkgP,na.rm=T)
                              # baseline_data$scenario[baseline_data$variable=="sediment_kg"]<-sum(DF$sed_outmtons,na.rm=T)
                              # baseline_data$scenario[baseline_data$variable=="totp_kg"]<-sum(DF$solp_outkgP + DF$sedp_outkgP,na.rm=T)
                              # 
                              # # calculate % difference between baseline and scenario
                              # baseline_data$change_per<-(baseline_data$scenario-baseline_data$baseline) *100 / baseline_data$baseline
                              # 
                              # plot1<-ggplot(baseline_data,aes(x=variable,y=change_per))+geom_bar(stat = 'identity')+ylab("Change from baseline (%)")+
                              #   xlab("")+ 
                              #   geom_text(size=16,aes(label=round(change_per)), position=position_dodge(width=0.9), vjust=-0.09,colour="black")+
                              #   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                              #         panel.background = element_blank(),text = element_text(size = 16),
                              #         panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
                              # 
                              # myplots[[climatemodel]]<-plot1
                              
                              
     # })
} #)
   

    # plot_output<-grid.arrange(grobs = myplots, ncol=2)
    # setwd(here('www'))
    # ggsave("avg_change_BR.png",plot_output)
      
    }

}     



