#change channel parameters for manual calibration

#read in whole file as a table
# make edits
# rewrite file

### Improvements - move hard coded % up to the top

ImproveDitchParams <- function(stream_rate) {


############### READ IN DATA ##########################

stream_rate<-stream_rate/100
  
set.seed(1)

  
baseline <- paste0(getwd(),"/baseline")  
scenario <- paste0(getwd(),"/scenario")

  
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

}
