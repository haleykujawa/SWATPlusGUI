#change channel parameters for manual calibration

#read in whole file as a table
# make edits
# rewrite file

### Improvements - move hard coded % up to the top

ChangeSWATInputs <- function(stream_rate,CSFT,CSNT,CSRT,CSRot,CSNTcc,CSWS,CSWcc) {


############### READ IN DATA ##########################

stream_rate<-stream_rate/100
  
set.seed(1)

 
baseline <- paste0(here("baseline"))  
scenario <- paste0(here("scenario"))


############################### CHANGE DITCH PARAMETERS ########################  
setwd(baseline)
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
  
  #add break if cropland_area > 1
  
  x=cropland_area*per_change
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
  while( area <= x ){ #change to x*1.015 to get closer to the actual input number
    
    #randomly sample per_change value where they are true
    #sampling without replacement has to occur within the one go
    sample1 <- sample(sampleRow[,1],1)
    
    if (!is.element(sample1,spot)){ #if statement saying "if is a duplicate in "spot", sample again
      
      spot<-rbind(spot,sample1)
      
      area<-sampleRow[sampleRow[,1]==spot[i],2]+area
      i<-i+1
      
    }
    
    
  }
  spot<-sort.int(spot)
  return(spot)
  
}

################# CHANGE X% OF STREAM PARAMS TO REPRESENT A CONSERVATION DITCH ############

#stream order 1 or 2
ind<- grepl(paste0(c("1","2"),collapse="|"),DF$order)

DF$erod_fact[ChangeHRU(DF,ind,sum(DF$len_num), stream_rate,DF$len_num)]<-"0.00010"

# also change other related parameters

#DF$mann[grepl("0.0001",DF$erod_fact)]<-"0.00400"


################ REWRITE NEW CHANNEL PARAMS #################################


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
DF[[1]]<-spaceOutput_spacesecond(DF[[1]],29) 
DF[[2]]<-spaceOutput(DF[[2]],5)

#all other columns have 14 spaces
for (i in c(3:length(DF))){
  
  DF[[i]] <- spaceOutput(DF[[i]],14)
  
  
}



setwd(scenario)

#unlink(tmp,force=T)
file.remove('hyd-sed-lte.cha')

sink('hyd-sed-lte.cha', type=c("output"), append = T)

write(c(topOfFile),'hyd-sed-lte.cha',sep = "\n",append=T)

write(c(paste0(DF[[1]],DF[[2]],DF[[3]],DF[[4]],DF[[5]],DF[[6]],DF[[7]],DF[[8]],DF[[9]],DF[[10]],
              DF[[11]],DF[[12]],DF[[13]],DF[[14]],DF[[15]],DF[[16]],DF[[17]],DF[[18]],DF[[19]],
              DF[[20]],DF[[21]],DF[[22]],DF[[23]])),'hyd-sed-lte.cha',sep="\n",append=T)

sink()

########################### CHANGE MANAGEMENT ######################################################

print(paste(c("Hi! testing, heres's the CSFT rate:"), as.character(CSFT)))
print(paste(c("Hi! testing, heres's the CSNT rate:"), as.character(CSNT)))

########################## CONVERT MANAGEMENT RATES TO DECIMAL PERCENT ##############
CSFT<-CSFT/100
CSNT<-CSNT/100
CSRT<-CSRT/100
CSRot<-CSRot/100
CSNTcc<-CSNTcc/100
CSWS<-CSWS/100
CSWcc<-CSWcc/100

#testing
print(CSFT)
print(CSNT)
print(CSRT)
print(CSRot)
print(CSNTcc)
print(CSWS)
print(CSWcc)

############### READ IN HRU-DATA ##########################
setwd(scenario)
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

################## READ IN SOILS DATA ###############################
#setwd('..')
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
HRUarea<-tibble(substr(HRUarea,11,17),substr(HRUarea,44,50))
colnames(HRUarea)<-c("name","area_ha")

close(tmp)

#remove spaces so the name characters match
hru_data$name<-str_replace_all(hru_data$name, fixed(" "), "")
HRUarea$name<-str_replace_all(HRUarea$name, fixed(" "), "")

hru_data<-left_join(hru_data,HRUarea,by="name")
hru_data$area_ha<-as.numeric(hru_data$area_ha)

#total cropland area
cropland_area<-sum(hru_data$area_ha[grepl("corn_lum",hru_data$lu_mgt) | grepl("soyb_lum",hru_data$lu_mgt) | grepl(paste(c("CS","SC"),collapse="|"),hru_data$lu_mgt)])
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

################# EDIT HRU DATA #####################################


##### Edit an input based on a percentage you want changed ##########

# rename CS,CSW,CSWcc with some generic filler so I know which cells have and haven't been changed
hru_data$lu_mgt[grepl(paste(c("CS","SC","corn_lum","soyb_lum"),collapse="|"),hru_data$lu_mgt)]<-"rowcrop_lum"

#tile index
#C,D, and B soils with slopes less than 2%
tileInd<-(grepl("rowcrop_lum",hru_data$lu_mgt))

#no tile index
#A soils and B soils with slope greater than or equal to 2%
#notileInd<-grepl("rowcrop_lum",hru_data$lu_mgt) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02)) 

#replace 100% replace all automated corn and soy with the CS/SC rotations I specified
hru_data$lu_mgt[ChangeHRU(hru_data,tileInd,cropland_area, 0.5,hru_data$area_ha)]<-"CS_FT_tile"

#replace the remaining 50%
tileInd<-(grepl("rowcrop_lum",hru_data$lu_mgt) )
hru_data$lu_mgt[tileInd]<-"SC_FT_tile"

# replace HRUs with A classification with no tile landuse
# notile_cropland_area<-sum(hru_data$area_ha[notileInd])
# hru_data$lu_mgt[ChangeHRU(hru_data,notileInd,notile_cropland_area, 0.5)]<-"CS_FT"

# notileInd<-grepl("rowcrop_lum",hru_data$lu_mgt) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
# hru_data$lu_mgt[notileInd]<-"SC_FT"

###### 4% of fields with Corn Soy / Soy Corn - Reduced tillage #######################

#Have to re-grab the indicies that can be replaced everytime the table changes
# using ^ character $ ensures you only grab that specific row with the exact text match
#replace with tiled fields, then out of that replace any with untiled characteristics (A, B w/ slope >2%) with the management but no tile
IndexVal<-grepl("^CS_FT_tile$",hru_data$lu_mgt)
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, CSRT/2,hru_data$area_ha)]<-"CS_RT_tile_B" # These fields have buffers #

IndexVal<-grepl("^SC_FT_tile$",hru_data$lu_mgt)
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, CSRT/2,hru_data$area_ha)]<-"SC_RT_tile"

IndexVal<-(grepl("^CS_RT_tile_B$",hru_data$lu_mgt)) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"CS_RT_B" # These fields have buffers #


#replace rows with soil type A or B with slopes >2% with same management but no tile
IndexVal<-grepl("SC_RT_tile",hru_data$lu_mgt) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02)) 
hru_data$lu_mgt[IndexVal]<-"SC_RT"



###### 15% of fields with Corn Soy / Soy Corn - Rotational no-till #######################
print(cropland_area) #testing
print(CSRot) #testing
IndexVal<-(grepl("^CS_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, CSRot/2,hru_data$area_ha)]<-"CS_RotT_tile"

IndexVal<-(grepl("^SC_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, CSRot/2,hru_data$area_ha)]<-"SC_RotT_tile_B" # These fields have buffers #

#replace rows with soil type A/B with same management but notile
IndexVal<-(grepl("SC_RotT_tile",hru_data$lu_mgt)) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"SC_RotT_B" # These fields have buffers #

IndexVal<-(grepl("CS_RotT_tile",hru_data$lu_mgt)) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"CS_RotT"



###### 40% CS full no till ###########################
IndexVal<-(grepl("^CS_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, CSNT/2,hru_data$area_ha)]<-"CS_NT_tile" #These fields have grassed waterways#

#Have to add tile here before using grassed waterways index
IndexVal<-(grepl("CS_NT_tile",hru_data$lu_mgt)) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"CS_NT" #These fields have grassed waterways #

#Replace with grassed waterway based on slope (low, medium, high)
#tiled
#low
IndexVal<-(grepl("^CS_NT_tile$",hru_data$lu_mgt) & (hru_data$slp <=  0.02))
hru_data$lu_mgt[IndexVal]<-"CS_NT_tile_GW_LS" # low slope buffers #

#medium
IndexVal<-(grepl("^CS_NT_tile$",hru_data$lu_mgt) & (0.02 < hru_data$slp & hru_data$slp <=  0.08))
hru_data$lu_mgt[IndexVal]<-"CS_NT_tile_GW_MS" # Medium slope buffers #

#high
IndexVal<-(grepl("^CS_NT_tile$",hru_data$lu_mgt) & (0.08 < hru_data$slp))
hru_data$lu_mgt[IndexVal]<-"CS_NT_tile_GW_HS" # High slope buffers #

#untiled
#low
IndexVal<-(grepl("^CS_NT$",hru_data$lu_mgt) & (hru_data$slp <=  0.02))
hru_data$lu_mgt[IndexVal]<-"CS_NT_GW_LS" # low slope buffers #

#medium
IndexVal<-(grepl("^CS_NT$",hru_data$lu_mgt) & (0.02 < hru_data$slp & hru_data$slp <=  0.08))
hru_data$lu_mgt[IndexVal]<-"CS_NT_GW_MS" # Medium slope buffers #

#high
IndexVal<-(grepl("^CS_NT$",hru_data$lu_mgt) & (0.08 < hru_data$slp))
hru_data$lu_mgt[IndexVal]<-"CS_NT_GW_HS" # High slope buffers #


IndexVal<-(grepl("^SC_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, CSNT/2,hru_data$area_ha)]<-"SC_NT_tile_B" # These fields have buffers #

#replace rows with soil type A/B with same management but notile
IndexVal<-(grepl("SC_NT_tile",hru_data$lu_mgt)) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"SC_NT_B" # These fields have buffers #



###### CS full no till w/ cc  ###########################
IndexVal<-(grepl("^CS_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, CSNTcc/2,hru_data$area_ha)]<-"CS_NTcc_tile_B" # These fields have buffers #

IndexVal<-(grepl("^SC_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, CSNTcc/2,hru_data$area_ha)]<-"SC_NTcc_tile"

#replace rows with soil type A/B with same management but notile
IndexVal<-(grepl("CS_NTcc_tile",hru_data$lu_mgt)) & (hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"SC_NTcc"

IndexVal<-(grepl("SC_NTcc_tile",hru_data$lu_mgt)) & (hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"CS_NTcc_B" # These fields have buffers #



###### 9% CSW double crop beans ###########################
IndexVal<-(grepl("^CS_FT_tile$",hru_data$lu_mgt) | grepl("^SC_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, CSWS,hru_data$area_ha)]<-"CSWS_tile"

#replace rows with soil type A/B with same management but notile
IndexVal<-(grepl("CSWS_tile",hru_data$lu_mgt)) & (hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"CSWS"



###### 1% CSW double crop beans ###########################
IndexVal<-(grepl("^CS_FT_tile$",hru_data$lu_mgt) | grepl("^SC_FT_tile$",hru_data$lu_mgt) )
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, CSWcc,hru_data$area_ha)]<-"CSWcc_tile" #These fields have grassed waterways #

#replace rows with soil type A/B with same management but notile
IndexVal<-(grepl("CSWcc_tile",hru_data$lu_mgt)) & (hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"CSWcc"

#Replace with grassed waterway based on slope (low, medium, high)
#tiled
#low
IndexVal<-(grepl("^CSWcc_tile$",hru_data$lu_mgt) & (hru_data$slp <=  0.02))
hru_data$lu_mgt[IndexVal]<-"CSWcc_tile_GW_LS" # low slope buffers #

#medium
IndexVal<-(grepl("^CSWcc_tile$",hru_data$lu_mgt) & (0.02 < hru_data$slp & hru_data$slp <=  0.08))
hru_data$lu_mgt[IndexVal]<-"CSWcc_tile_GW_MS" # Medium slope buffers #

#high
IndexVal<-(grepl("^CSWcc_tile$",hru_data$lu_mgt) & (0.08 < hru_data$slp))
hru_data$lu_mgt[IndexVal]<-"CSWcc_tile_GW_HS" # High slope buffers #

#untiled
#low
IndexVal<-(grepl("^CSWcc$",hru_data$lu_mgt) & (hru_data$slp <=  0.02))
hru_data$lu_mgt[IndexVal]<-"CSWcc_GW_LS" # low slope buffers #

#medium
IndexVal<-(grepl("^CSWcc$",hru_data$lu_mgt) & (0.02 < hru_data$slp & hru_data$slp <=  0.08))
hru_data$lu_mgt[IndexVal]<-"CSWcc_GW_MS" # Medium slope buffers #

#high
IndexVal<-(grepl("^CSWcc$",hru_data$lu_mgt) & (0.08 < hru_data$slp))
hru_data$lu_mgt[IndexVal]<-"CSWcc_GW_HS" # High slope buffers #

### Now replace all A + B 2% soils from the initialization of CS and SC FT rotations and replace with no tile ###
IndexVal<-grepl("CS_FT_tile",hru_data$lu_mgt) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02)) 
hru_data$lu_mgt[IndexVal]<-"CS_FT"

IndexVal<-grepl("SC_FT_tile",hru_data$lu_mgt) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02)) 
hru_data$lu_mgt[IndexVal]<-"SC_FT"

############ CHECK % OF FINAL MGT ##############################

 # mgt_check<-hru_data %>%
 # group_by(lu_mgt) %>%
 # summarize(value=sum(area_ha,na.rm=T))

mgt_check<-data.frame(matrix(nrow=1,ncol=7))
colnames(mgt_check)<-c("CSFT","CSNT","CSRT","CSRotT","CS_NTcc","CSWS","CSWcc")

mgt_check$CSFT<-sum(hru_data$area_ha[grepl(paste(c("CS_FT","SC_FT"),collapse="|"),hru_data$lu_mgt)])
mgt_check$CSNT<-sum(hru_data$area_ha[grepl(paste(c("CS_NT","SC_NT"),collapse="|") ,hru_data$lu_mgt) & !grepl(paste(c("CS_NTcc","SC_NTcc"),collapse="|") ,hru_data$lu_mgt)])
mgt_check$CSRT<-sum(hru_data$area_ha[grepl(paste(c("CS_RT","SC_RT"),collapse="|"),hru_data$lu_mgt)])
mgt_check$CSRotT<-sum(hru_data$area_ha[grepl(paste(c("CS_RotT","SC_RotT"),collapse="|"),hru_data$lu_mgt)])
mgt_check$CS_NTcc<-sum(hru_data$area_ha[grepl(paste(c("CS_NTcc","SC_NTcc"),collapse="|"),hru_data$lu_mgt)])
mgt_check$CSWS<-sum(hru_data$area_ha[grepl(c("CSWS"),hru_data$lu_mgt)])
mgt_check$CSWcc<-sum(hru_data$area_ha[grepl(c("CSWcc"),hru_data$lu_mgt)])

# colnames(mgt_check)<-c("lum","area ha") 

 write.csv(mgt_check,"mgt_check.csv",row.names=F)

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
hru_data$name<-spaceOutput(hru_data$name,9)
hru_data$topo<-spaceOutput(hru_data$topo,27)
hru_data$hydro<-spaceOutput(hru_data$hydro,18)
hru_data$soil<-spaceOutput(hru_data$soil,18)
hru_data$lu_mgt<-spaceOutput(hru_data$lu_mgt,18)
hru_data$soil_plant_init<-spaceOutput(hru_data$soil_plant_init,18)
hru_data$surf_stor<-spaceOutput(hru_data$surf_stor,18)
hru_data$snow<-spaceOutput(hru_data$snow,18)
hru_data$field<-spaceOutput(hru_data$field,18)


setwd(scenario)
file.remove('hru-data.hru')
sink('hru-data.hru', type=c("output"), append = T)
write(c(topOfFile),'hru-data.hru',sep = "\n",append=T)
write(c(paste(hru_data$id,hru_data$name,hru_data$topo,hru_data$hydro,hru_data$soil,
      hru_data$lu_mgt,hru_data$soil_plant_init,hru_data$surf_stor,hru_data$snow, hru_data$field)),'hru-data.hru',sep="\n",append=T)
sink()

print('I should have wrote the new hru file by now :)')

##################### CHANGE HRU PARAMETERS TO MATCH ROTATIONS ##########################

}
