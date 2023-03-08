##### Rewrite landuse.lum file so every rotation has an option to add tile, grassed waterways, and vegtative buffers ##########
rm(list = ls())
library(here)
### Set up data frame with known inputs ####
# 7 rotations, CS_FT, CS_RT, CS_RotT, CS_NT, CS_NTcc, CSWS, CSWcc
# and the reverse SC_FT, SC_RT, SC_RotT, SC_NT, SC_NTcc

here("Baseline")

####### Build base data frame-- no tile, no buffers, no gww ##############
DF<-data.frame(matrix(nrow=12, ncol=14))
colnames(DF)<-c("name", "cal_group", "plnt_com",  "mgt",  "cn2",  "cons_prac",  "urban",            
                  "urb_ro",   "ov_mann",  "tile",   "sep",  "vfs",  "grww", "bmp")

DF$name<-c("CS_FT", "CS_RT", "CS_RotT", "CS_NT", "CS_NTcc", "CSWS", "CSWcc","SC_FT", "SC_RT", "SC_RotT", "SC_NT", "SC_NTcc")

DF$cal_group<-"null"

#plant community
DF$plnt_com[grepl("CS",DF$name)]<-"CS_comm"
DF$plnt_com[grepl("SC",DF$name)]<-"SC_comm"
DF$plnt_com[grepl("CS_NTcc",DF$name)]<-"CSR_comm"
DF$plnt_com[grepl("SC_NTcc",DF$name)]<-"SCR_comm" #Check that this is right and not causing an issue, I think it may need to be SRC
DF$plnt_com[grepl("CSWS",DF$name)]<-"CSWS_comm"
DF$plnt_com[grepl("CSWcc",DF$name)]<-"CSWR_comm"

#management
DF$mgt[grepl("CS_FT",DF$name)]<-"CS_FullTill"
DF$mgt[grepl("SC_FT",DF$name)]<-"SC_FullTill"
DF$mgt[grepl("CS_RT",DF$name)]<-"CS_ReduceTill"
DF$mgt[grepl("SC_RT",DF$name)]<-"SC_ReduceTill"
DF$mgt[grepl("CS_RotT",DF$name)]<-"CS_RotNoTill"
DF$mgt[grepl("SC_RotT",DF$name)]<-"SC_RotNoTill"
DF$mgt[grepl("CS_NT",DF$name)]<-"CS_NoTill"
DF$mgt[grepl("SC_NT",DF$name)]<-"SC_NoTill"
DF$mgt[grepl("CS_NTcc",DF$name)]<-"CS_NoTillCC"
DF$mgt[grepl("SC_NTcc",DF$name)]<-"SC_NoTillCC"
DF$mgt[grepl("CSWS",DF$name)]<-"CSWS"
DF$mgt[grepl("CSWcc",DF$name)]<-"CSW_CC"

#cn2
DF$cn2[grepl("CS_FT",DF$name)]<-"rc_strow_p"
DF$cn2[grepl("SC_FT",DF$name)]<-"rc_strow_p"
DF$cn2[grepl("CS_RT",DF$name)]<-"rc_strow_g"
DF$cn2[grepl("SC_RT",DF$name)]<-"rc_strow_g"
DF$cn2[grepl("CS_RotT",DF$name)]<-"rc_strow_g"
DF$cn2[grepl("SC_RotT",DF$name)]<-"rc_strow_g"
DF$cn2[grepl("CS_NT",DF$name)]<-"rc_strowres_g"
DF$cn2[grepl("SC_NT",DF$name)]<-"rc_strowres_g"
DF$cn2[grepl("CS_NTcc",DF$name)]<-"rc_strowres_g"
DF$cn2[grepl("SC_NTcc",DF$name)]<-"rc_strowres_g"
DF$cn2[grepl("CSWS",DF$name)]<-"rc_strowres_g"
DF$cn2[grepl("CSWcc",DF$name)]<-"rc_strowres_g"

#cons prac
DF$cons_prac<-"up_down_slope"

#urban
DF$urban<-"null"
DF$urb_ro<-"null"

#ov_mann
DF$ov_mann[grepl("CS_FT",DF$name)]<-"convtill_nores"
DF$ov_mann[grepl("SC_FT",DF$name)]<-"convtill_nores"
DF$ov_mann[grepl("CS_RT",DF$name)]<-"rotTill_nores"
DF$ov_mann[grepl("SC_RT",DF$name)]<-"rotTill_nores"
DF$ov_mann[grepl("CS_RotT",DF$name)]<-"rotTill_nores"
DF$ov_mann[grepl("SC_RotT",DF$name)]<-"rotTill_nores"
DF$ov_mann[grepl("CS_NT",DF$name)]<-"notill_0.5-1res"
DF$ov_mann[grepl("SC_NT",DF$name)]<-"notill_0.5-1res"
DF$ov_mann[grepl("CS_NTcc",DF$name)]<-"notill_0.5-1res"
DF$ov_mann[grepl("SC_NTcc",DF$name)]<-"notill_0.5-1res"
DF$ov_mann[grepl("CSWS",DF$name)]<-"notill_0.5-1res"
DF$ov_mann[grepl("CSWcc",DF$name)]<-"notill_0.5-1res"

#tile
DF$tile<-"null"

#sep
DF$sep<-"null"

#vfs
DF$vfs<-"null"

#grww
DF$grww<-"null"

#bmp
DF$bmp<-"null"

################## Add tile ################################

tile<-DF
tile$tile<-"OWC_drain"
tile$name<-paste0(tile$name, "_tile")

################## Add gww to tile and non tile ############

#non tile gww
grww_hi<-DF
grww_hi$grww<-"grwway_high"
grww_hi$name<-paste0(grww_hi$name,"_GW_HS")

grww_med<-DF
grww_med$grww<-"grwway_med"
grww_med$name<-paste0(grww_med$name,"_GW_MS")

grww_low<-DF
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
vfs<-DF
vfs$vfs<-"field_border"
vfs$name<-paste0(vfs$name,"_B")

#tile buffer
vfs_tile<-tile
vfs_tile$vfs<-"field_border"
vfs_tile$name<-paste0(vfs_tile$name,"_B")

write_to_lum<-rbind(DF,tile,grww_hi,grww_med,grww_low,grww_hi_tile,grww_med_tile,grww_low_tile,vfs,vfs_tile)

############# Read and overwrite landuse.lum ###############
