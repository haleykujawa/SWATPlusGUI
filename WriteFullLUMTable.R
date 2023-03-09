##### Rewrite landuse.lum file so every rotation has an option to add tile, grassed waterways, and vegtative buffers ##########
rm(list = ls())
library(here)
library(stringr)
### Set up data frame with known inputs ####
# 7 rotations, CS_FT, CS_RT, CS_RotT, CS_NT, CS_NTcc, CSWS, CSWcc
# and the reverse SC_FT, SC_RT, SC_RotT, SC_NT, SC_NTcc



####### Build base data frame-- no tile, no buffers, no gww ##############
basedf<-data.frame(matrix(nrow=12, ncol=14))
colnames(basedf)<-c("name", "cal_group", "plnt_com",  "mgt",  "cn2",  "cons_prac",  "urban",            
                  "urb_ro",   "ov_mann",  "tile",   "sep",  "vfs",  "grww", "bmp")

basedf$name<-c("CS_FT", "CS_RT", "CS_RotT", "CS_NT", "CS_NTcc", "CSWS", "CSWcc","SC_FT", "SC_RT", "SC_RotT", "SC_NT", "SC_NTcc")

basedf$cal_group<-"null"

#plant community
basedf$plnt_com[grepl("CS",basedf$name)]<-"CS_comm"
basedf$plnt_com[grepl("SC",basedf$name)]<-"SC_comm"
basedf$plnt_com[grepl("CS_NTcc",basedf$name)]<-"CSR_comm"
basedf$plnt_com[grepl("SC_NTcc",basedf$name)]<-"SCR_comm" #Check that this is right and not causing an issue, I think it may need to be SRC
basedf$plnt_com[grepl("CSWS",basedf$name)]<-"CSWS_comm"
basedf$plnt_com[grepl("CSWcc",basedf$name)]<-"CSWR_comm"

#management
basedf$mgt[grepl("CS_FT",basedf$name)]<-"CS_FullTill"
basedf$mgt[grepl("SC_FT",basedf$name)]<-"SC_FullTill"
basedf$mgt[grepl("CS_RT",basedf$name)]<-"CS_ReduceTill"
basedf$mgt[grepl("SC_RT",basedf$name)]<-"SC_ReduceTill"
basedf$mgt[grepl("CS_RotT",basedf$name)]<-"CS_RotNoTill"
basedf$mgt[grepl("SC_RotT",basedf$name)]<-"SC_RotNoTill"
basedf$mgt[grepl("CS_NT",basedf$name)]<-"CS_NoTill"
basedf$mgt[grepl("SC_NT",basedf$name)]<-"SC_NoTill"
basedf$mgt[grepl("CS_NTcc",basedf$name)]<-"CS_NoTillCC"
basedf$mgt[grepl("SC_NTcc",basedf$name)]<-"SC_NoTillCC"
basedf$mgt[grepl("CSWS",basedf$name)]<-"CSWS"
basedf$mgt[grepl("CSWcc",basedf$name)]<-"CSW_CC"

#cn2
basedf$cn2[grepl("CS_FT",basedf$name)]<-"rc_strow_p"
basedf$cn2[grepl("SC_FT",basedf$name)]<-"rc_strow_p"
basedf$cn2[grepl("CS_RT",basedf$name)]<-"rc_strow_g"
basedf$cn2[grepl("SC_RT",basedf$name)]<-"rc_strow_g"
basedf$cn2[grepl("CS_RotT",basedf$name)]<-"rc_strow_g"
basedf$cn2[grepl("SC_RotT",basedf$name)]<-"rc_strow_g"
basedf$cn2[grepl("CS_NT",basedf$name)]<-"rc_strowres_g"
basedf$cn2[grepl("SC_NT",basedf$name)]<-"rc_strowres_g"
basedf$cn2[grepl("CS_NTcc",basedf$name)]<-"rc_strowres_g"
basedf$cn2[grepl("SC_NTcc",basedf$name)]<-"rc_strowres_g"
basedf$cn2[grepl("CSWS",basedf$name)]<-"rc_strowres_g"
basedf$cn2[grepl("CSWcc",basedf$name)]<-"rc_strowres_g"

#cons prac
basedf$cons_prac<-"up_down_slope"

#urban
basedf$urban<-"null"
basedf$urb_ro<-"null"

#ov_mann
basedf$ov_mann[grepl("CS_FT",basedf$name)]<-"convtill_nores"
basedf$ov_mann[grepl("SC_FT",basedf$name)]<-"convtill_nores"
basedf$ov_mann[grepl("CS_RT",basedf$name)]<-"rotTill_nores"
basedf$ov_mann[grepl("SC_RT",basedf$name)]<-"rotTill_nores"
basedf$ov_mann[grepl("CS_RotT",basedf$name)]<-"rotTill_nores"
basedf$ov_mann[grepl("SC_RotT",basedf$name)]<-"rotTill_nores"
basedf$ov_mann[grepl("CS_NT",basedf$name)]<-"notill_0.5-1res"
basedf$ov_mann[grepl("SC_NT",basedf$name)]<-"notill_0.5-1res"
basedf$ov_mann[grepl("CS_NTcc",basedf$name)]<-"notill_0.5-1res"
basedf$ov_mann[grepl("SC_NTcc",basedf$name)]<-"notill_0.5-1res"
basedf$ov_mann[grepl("CSWS",basedf$name)]<-"notill_0.5-1res"
basedf$ov_mann[grepl("CSWcc",basedf$name)]<-"notill_0.5-1res"

#tile
basedf$tile<-"null"

#sep
basedf$sep<-"null"

#vfs
basedf$vfs<-"null"

#grww
basedf$grww<-"null"

#bmp
basedf$bmp<-"null"

################## Add tile ################################

tile<-basedf
tile$tile<-"OWC_drain"
tile$name<-paste0(tile$name, "_tile")

################## Add gww to tile and non tile ############

#non tile gww
grww_hi<-basedf
grww_hi$grww<-"grwway_high"
grww_hi$name<-paste0(grww_hi$name,"_GW_HS")

grww_med<-basedf
grww_med$grww<-"grwway_med"
grww_med$name<-paste0(grww_med$name,"_GW_MS")

grww_low<-basedf
grww_low$grww<-"grwway_low"
grww_low$name<-paste0(grww_low$name,"_GW_LS")

#tile gww
grww_hi_tile<-tile
grww_hi_tile$grww<-"grwway_high"
grww_hi_tile$name<-paste0(grww_hi_tile$name,"_GW_HS")

grww_med_tile<-tile
grww_med_tile$grww<-"grwway_med"
grww_med_tile$name<-paste0(grww_med_tile$name,"_GW_MS")

grww_low_tile<-tile
grww_low_tile$grww<-"grwway_low"
grww_low_tile$name<-paste0(grww_low_tile$name,"_GW_LS")

############### Add buffers to tile and non tile ##########

#non tile buffer
vfs<-basedf
vfs$vfs<-"field_border"
vfs$name<-paste0(vfs$name,"_B")

#tile buffer
vfs_tile<-tile
vfs_tile$vfs<-"field_border"
vfs_tile$name<-paste0(vfs_tile$name,"_B")

DF<-rbind(basedf,tile,grww_hi,grww_med,grww_low,grww_hi_tile,grww_med_tile,grww_low_tile,vfs,vfs_tile)

############# Read and overwrite landuse.lum ###############
#read past first 6 lines and keep--urban and generic SWAT inputs
file_dir<-file.path(here("Baseline","landuse.lum"))
tmp <- file(file_dir)
open(tmp, "r") #read

#read past headerline and save to rewrite the file
topOfFile<-readLines(tmp, n = 8) 

spaceOutput<-function(data,nspaces){
  
  newData<-paste0(str_dup(" ",(nspaces-nchar(data))),data)
  return(newData)
  
}

spaceOutput_spacesecond<-function(data,nspaces){
  
  newData<-paste0(data,str_dup(" ",(nspaces-nchar(data))))
  return(newData)
  
}

#space the first two columns appropriately
DF[[1]]<-spaceOutput_spacesecond(DF[[1]],20) 

#all other columns have 14 spaces
for (i in c(2:length(DF))){
  
  DF[[i]] <- spaceOutput(DF[[i]],18)
  
  
}

#had issues removing the file, so instead I opened and wiped the file of 
#its contents and then wrote new data
close( file( file_dir, open="w" ) ) 

sink(file_dir, type=c("output"), append = T)

write(c(topOfFile),file_dir,sep = "\n",append=T)

write(c(paste0(DF[[1]],DF[[2]],DF[[3]],DF[[4]],DF[[5]],DF[[6]],DF[[7]],DF[[8]],DF[[9]],DF[[10]],
               DF[[11]],DF[[12]],DF[[13]],DF[[14]])),file_dir,sep="\n",append=T)

sink()