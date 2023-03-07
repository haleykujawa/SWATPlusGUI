##editing management data in hru-data file to reflect more general management in OWC
#change rotations and add tile drainage..
#add 35% buffers/vfs based on slope and 21% grassed waterways

#This goes with the Management summary_new rotations_3 (New rotations + New Landuse), SWAT+ files in Scenarios --> Manually edited TxtInOut files --> NewMgt01182022

#Before running this file more than once, you need to replace the ORIGINAL HRU FILE in the text in out folder
#Also need to put the SSURGO data with soil classification in
#for the correct edits to be made. This is meant to be run anytime a new model is built in the GUI and the 
#generic management implemented with SWAT+

#read in whole file as a table
# make edits
# rewrite file

### Improvements - move hard coded % up to the top

rm(list=ls())
xlib = c("readtext","dplyr","splitstackshape","stringr")
lapply(xlib, require, character.only=T) ; rm(xlib)

set.seed(1)

setwd('C:/Users/kujawa.21/Documents/Margaret A. Davidson Fellowship/Old Woman Creek SWAT/OWC_SWAT_07292021/ParallelLakes/ParallelLakes_decHRUfixDEM/Scenarios/BaselinePlusMgt - ManualCal - NewEXE4/TxtInOut')
#where to save new hru-data file to
ScenarioDir<-getwd()

#where to save old hur-data file, create a folder inside TxTinOut called OldTxtInOut

if (dir.exists(paste0(ScenarioDir,"/OldTxtInOut"))){
  
  setwd(paste0(ScenarioDir,"/OldTxtInOut")) 
  
} else {
  
  dir.create(paste0(ScenarioDir,"/OldTxtInOut")) }

OldFileDir<-paste0(ScenarioDir,"/OldTxtInOut")





############### READ IN HRU-DATA ##########################
setwd(ScenarioDir)
tmp <- file('hru-data.hru')
open(tmp, "r") #read

#read past headerline and save to rewrite the file
topOfFile<-readLines(tmp, n = 2) 

#read file 
data1<-readLines(tmp, n = -1)

#save originial file
file.copy("hru-data.hru",OldFileDir)

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

#This function generates random rows to replace in the HRU table where the index is true and will meet replacing 
#enough are to constitute a certain % of the total acres
#if you sample too large a portion this loop will run forever because it has difficulty reaching the area standard 
ChangeHRU<-function(hru_data,IndexVal,cropland_area,per_change){
  
  x=cropland_area*per_change
  #combine with a row index
  IndexVal<-cbind(IndexVal,c(1:length(IndexVal)),hru_data$area_ha)
  
  #pull row indicies of true values and the associated area
  sampleRow<-IndexVal[IndexVal[,1]==1,c(2,3)]
  
 
  

  
  spot<-c()
  area<-0
  i<-1
  #sample until the % area of cropland is met
  #ASSUMING ONCE THE AREA GOES OVER IT DOESN'T EXECUTE THAT LOOP AND JUST EXITS THE LOOP, THAT'S WHY SOME OF MY PERCENTAGES I INPUT ARE SLIGHTLY LOWER THAN INPUT (1-2%)
  #THAN THE OUTPUT
  while( area <= x ){
  
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

#### this function changes percent of rows ###########################
ChangeHRUrows<-function(IndexVal,per_change){
  
  #combine with a row index
  IndexVal<-cbind(IndexVal,c(1:length(IndexVal)))
  #pull row indicies of true values
  sampleRow<-IndexVal[IndexVal[,1]==1,2]
  #randomly sample per_change value where they are true
  set.seed(1)
  spot <- sample(sampleRow,round(per_change*(length(sampleRow))))
  
  return(spot)
  
}


#### Flag B (somewhat poorly drained) soils to be tiled ##############

#amount of A soils
soilA_area<-sum(hru_data$area_ha[hru_data$hyd_grp=="A" & grepl(paste(c("CS","SC"),collapse="|"),hru_data$lu_mgt) & (hru_data$slp < 0.02)])
soilB_area<-sum(hru_data$area_ha[hru_data$hyd_grp=="B" & grepl(paste(c("CS","SC"),collapse="|"),hru_data$lu_mgt) & (hru_data$slp < 0.02)])
soilC_area<-sum(hru_data$area_ha[hru_data$hyd_grp=="C" & grepl(paste(c("CS","SC"),collapse="|"),hru_data$lu_mgt) ])
soilD_area<-sum(hru_data$area_ha[hru_data$hyd_grp=="D" & grepl(paste(c("CS","SC"),collapse="|"),hru_data$lu_mgt) ])

soilB_area<-soilB_area*100/cropland_area




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
hru_data$lu_mgt[ChangeHRU(hru_data,tileInd,cropland_area, 0.5)]<-"CS_FT_tile"

#replace the remaining 50%
tileInd<-(grepl("rowcrop_lum",hru_data$lu_mgt) )
hru_data$lu_mgt[tileInd]<-"SC_FT_tile"

#replace HRUs with A classification with no tile landuse
#notile_cropland_area<-sum(hru_data$area_ha[notileInd])
#hru_data$lu_mgt[ChangeHRU(hru_data,notileInd,notile_cropland_area, 0.5)]<-"CS_FT"

#notileInd<-grepl("rowcrop_lum",hru_data$lu_mgt) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02)) 
#hru_data$lu_mgt[notileInd]<-"SC_FT"



###### 4% of fields with Corn Soy / Soy Corn - Reduced tillage #######################

#Have to re-grab the indicies that can be replaced everytime the table changes
# using ^ character $ ensures you only grab that specific row with the exact text match
#replace with tiled fields, then out of that replace any with untiled characteristics (A, B w/ slope >2%) with the management but no tile
IndexVal<-grepl("^CS_FT_tile$",hru_data$lu_mgt)
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, 0.02)]<-"CS_RT_tile_B" # These fields have buffers #

IndexVal<-grepl("^SC_FT_tile$",hru_data$lu_mgt)
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, 0.02)]<-"SC_RT_tile"

IndexVal<-(grepl("^CS_RT_tile_B$",hru_data$lu_mgt)) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"CS_RT_B" # These fields have buffers #


#replace rows with soil type A or B with slopes >2% with same management but no tile
IndexVal<-grepl("SC_RT_tile",hru_data$lu_mgt) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02)) 
hru_data$lu_mgt[IndexVal]<-"SC_RT"




###### 15% of fields with Corn Soy / Soy Corn - Rotational no-till #######################
IndexVal<-(grepl("^CS_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, 0.075)]<-"CS_RotT_tile"

IndexVal<-(grepl("^SC_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, 0.075)]<-"SC_RotT_tile_B" # These fields have buffers #

#replace rows with soil type A/B with same management but notile
IndexVal<-(grepl("SC_RotT_tile",hru_data$lu_mgt)) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"SC_RotT_B" # These fields have buffers #

IndexVal<-(grepl("CS_RotT_tile",hru_data$lu_mgt)) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"CS_RotT"



###### 40% CS full no till ###########################
IndexVal<-(grepl("^CS_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, 0.2)]<-"CS_NT_tile" #These fields have grassed waterways#

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
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, 0.2)]<-"SC_NT_tile_B" # These fields have buffers #

#replace rows with soil type A/B with same management but notile
IndexVal<-(grepl("SC_NT_tile",hru_data$lu_mgt)) & ( hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"SC_NT_B" # These fields have buffers #



###### 10% CS full no till w/ cc ###########################
IndexVal<-(grepl("^CS_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, 0.05)]<-"CS_NTcc_tile_B" # These fields have buffers #

IndexVal<-(grepl("^SC_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, 0.05)]<-"SC_NTcc_tile"

#replace rows with soil type A/B with same management but notile
IndexVal<-(grepl("CS_NTcc_tile",hru_data$lu_mgt)) & (hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"SC_NTcc"

IndexVal<-(grepl("SC_NTcc_tile",hru_data$lu_mgt)) & (hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"CS_NTcc_B" # These fields have buffers #



###### 9% CSW double crop beans ###########################
IndexVal<-(grepl("^CS_FT_tile$",hru_data$lu_mgt) | grepl("^SC_FT_tile$",hru_data$lu_mgt))
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, 0.09)]<-"CSWS_tile"

#replace rows with soil type A/B with same management but notile
IndexVal<-(grepl("CSWS_tile",hru_data$lu_mgt)) & (hru_data$hyd_grp=="A" | (hru_data$hyd_grp=="B" & hru_data$slp >= 0.02))
hru_data$lu_mgt[IndexVal]<-"CSWS"



###### 1% CSW double crop beans ###########################
IndexVal<-(grepl("^CS_FT_tile$",hru_data$lu_mgt) | grepl("^SC_FT_tile$",hru_data$lu_mgt) )
hru_data$lu_mgt[ChangeHRU(hru_data,IndexVal,cropland_area, 0.01)]<-"CSWcc_tile" #These fields have grassed waterways #

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


################# STP ##############################
#rename stp to soilplant1
hru_data$soil_plant_init<-"       soilplant1"


#Reference OWC Soil Data_SWAT+HK for data that went into defining percentages
#Some of the managed lands still have soil plant 1 BECAUSE THE WHILE LOOP GETS CLOSE TO THE % BUT IT'S NOT AN EXACT METHOD
IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.02)]<-"soilnut_0-10"

IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.1)]<-"soilnut_10-20"

IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.29)]<-"soilnut_20-30"

IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.26)]<-"soilnut_30-40"

IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.07)]<-"soilnut_40-50"

IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.04)]<-"soilnut_50-60"

IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.02)]<-"soilnut_60-70"

IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.06)]<-"soilnut_70-80"

IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.03)]<-"soilnut_80-90"

IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.02)]<-"soilnut_90-100"

IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.04)]<-"soilnut_100-110"

#IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
#hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.00)]<-"soilnut_110-120"

IndexVal<-(grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.03)]<-"soilnut_120-130"

#IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
#hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.00)]<-"soilnut_130-140"

IndexVal<-(grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt) & grepl("^       soilplant1$",hru_data$soil_plant_init))
hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.02)]<-"soilnut_140-150"

#IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
#hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.00)]<-"soilnut_150-160"

#IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
#hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.00)]<-"soilnut_160-170"

#IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
#hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.00)]<-"soilnut_170-180"

#IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
#hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.00)]<-"soilnut_180-190"

#IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
#hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.00)]<-"soilnut_190-200"

#IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
#hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.00)]<-"soilnut_200-210"

#IndexVal<-((grepl("CS",hru_data$lu_mgt) | grepl("SC",hru_data$lu_mgt) | grepl("CSW",hru_data$lu_mgt)) & grepl("^       soilplant1$",hru_data$soil_plant_init))
#hru_data$soil_plant_init[ChangeHRU(hru_data,IndexVal,cropland_area, 0.00)]<-"soilnut_210-220"



################ SUMMARIZE FINAL CHANGES IN HRUs ###############################################################

final_area<-tibble()
i<-1
watershed_area<-sum(hru_data$area_ha)

for (lum in unique(hru_data$lu_mgt)){
  
  final_area[i,1]<-lum
  final_area[i,2]<-sum(hru_data$area_ha[grepl(paste("^",lum,"$",sep=""),hru_data$lu_mgt)]) #total area
  final_area[i,3]<-final_area[i,2]*100/sum(hru_data$area_ha) #percent of watershed
  final_area[i,4]<-final_area[i,2]*100/cropland_area #percent of total cropland, only relevant for managed land use, not frsd or urml
  i<-i+1
  
  
}
colnames(final_area)<-c("lum","area_ha","percent_of_watershed","percent_of_cropland")
final_area<-with(final_area,  final_area[order(final_area$lum) , ])
write.csv(final_area,"LumAreaSummary.csv",row.names=F)

final_area<-tibble()
i<-1

for (lum in unique(hru_data$soil_plant_init)){
  
  final_area[i,1]<-lum
  final_area[i,2]<-sum(hru_data$area_ha[grepl(paste("^",lum,"$",sep=""),hru_data$soil_plant_init)]) #total area
  final_area[i,3]<-final_area[i,2]*100/sum(hru_data$area_ha) #percent of watershed
  final_area[i,4]<-final_area[i,2]*100/cropland_area #percent of total cropland, only relevant for managed land use, not frsd or urml
  i<-i+1
  
  
}
colnames(final_area)<-c("soil_plant_init","area_ha","percent_of_watershed","percent_of_cropland")
#final_area<-with(final_area,  final_area[order(final_area$lum) , ])
#final_area$soil_plant_init<-factor(hru_data$soil_plant_init, levels=c("       soilplant1","soilnut_0-10","soilnut_10-20","soilnut_20-30","soilnut_30-40","soilnut_40-50","soilnut_50-60",
#                                                                   "soilnut_60-70","soilnut_70-80","soilnut_80-90","soilnut_90-100","soilnut_100-110","soilnut_120-130",
#                                                                    "soilnut_140-150"))

write.csv(final_area,"STPSummary.csv",row.names=F)


################ SAVE THE OLD HRU-DATA FILE ELSEWHERE AND REWRITE NEW HRU-DATA #################################
file.copy("hru-data.hru",OldFileDir)

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


setwd(ScenarioDir)


#file.remove('hru-data.hru')

#sink('hru-data.hru', type=c("output"), append = T)



#write(c(topOfFile),'hru-data.hru',sep = "\n",append=T)

#write(c(paste(hru_data$id,hru_data$name,hru_data$topo,hru_data$hydro,hru_data$soil,
#      hru_data$lu_mgt,hru_data$soil_plant_init,hru_data$surf_stor,hru_data$snow, hru_data$field)),'hru-data.hru',sep="\n",append=T)


#sink()

