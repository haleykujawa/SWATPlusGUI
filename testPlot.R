#test passing more than 1 plot/graph to the GUI interface

testPlot<-function(scenario_dir,SelectClimate){
  library("ggplot2")
  library("patchwork")
  library("ggpmisc")
  
  # May want to end up building one DF and making a plot with it--hard to compare the same results if multiple plots exist.
  # Will want to pass this script info on the inputs used to print in the 'visualize outputs' column.
  
  # x<-runif(n=10, min=1, max=20)
  # y<-runif(n=10, min=1, max=20)
  
  # my_table<-data.frame(x,y)
  
  # plot1<-ggplot(data=my_table,aes(x,y))+geom_point()
  
  # ggp_table <- ggplot() +                             # Create empty plot with table
  # theme_void() +
  # annotate(geom = "table",
             # x = 1,
             # y = 1,
             # label = list(my_table))
  
  # output_data<-plot1+ggp_table
  # setwd(here('www'))
  
  myplots<-list('hist','CNRM','IPSL-CM5A-MR','MIROC5')
  
# Calculate outputs here
  headers<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"areaha",	"precipha.m",	"evapha.m",	
             "seepha.m",	"flo_storm.3.s",	"sed_stormtons",	"orgn_storkgN",	"sedp_storkgP",	"no3_storkgN",	"solp_storkgP",
             "chla_storkg",	"nh3_storkgN",	"no2_storkgN",	"cbod_storkg",	"dox_storkg",	"san_stortons",	"sil_stortons",	"cla_stortons",	"sag_stortons",
             "lag_stortons",	"grv_stortons",	"null1", "setl_stor",	"setlp_stor",	"flo_inm.3.s",	"sed_inmtons",	"orgn_inkgN",	"sedp_inkgP",	"no3_inkgN",
             "solp_inkgP",	"chla_inkg",	"nh3_inkgN",	"no2_inkgN",	"cbod_inkg",	"dox_inkg",	"san_intons",	"sil_intons",	"cla_intons",
             "sag_intons",	"lag_intons",	"grv_intons",	"null",	 "setl_in",	"setlp_in","flo_outm.3.s",	"sed_outmtons",	"orgn_outkgN",	"sedp_outkgP",	"no3_outkgN",
             "solp_outkgP",	"chla_outkg",	"nh3_outkgN",	"no2_outkgN",	"cbod_outkg",	"dox_outkg",	"san_outtons",	"sil_outtons",	"cla_outtons",
             "sag_outtons",	"lag_outtons",	"grv_outtons",	"null2", "setl_out",	"setlp_out", "water_tempdegC")#"null3","null4","null5","null6","null7")
  
  n=1 # counter for filling plots in list
  
  for (climatemodel in SelectClimate){
    
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
    xlab("")+ ggtitle(climatemodel)+
    geom_text(size=16,aes(label=round(change_per)), position=position_dodge(width=0.9), vjust=-0.09,colour="black")+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          panel.background = element_blank(),text = element_text(size = 16),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
  
  myplots[[n]]<-plot1
  n=n+1
  
  }
  
  
  
  # plot_output<-grid.arrange(grobs = myplots, ncol=2)
  # setwd(here('www'))
  # ggsave("avg_change_BR.png",plot_output)

  
  return(list(myplots[[1]],myplots[[2]],myplots[[3]],myplots[[4]],print("OWC-SWAT+ run complete"),  paste0('www/','avg_change_BR.png')))
  
  
  
}