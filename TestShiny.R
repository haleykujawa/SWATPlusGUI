#change channel parameters for manual calibration

#read in whole file as a table
# make edits
# rewrite file

### Improvements - move hard coded % up to the top

TestShiny <- function(stream_rate) {


############### READ IN DATA ##########################

stream_rate<-stream_rate/100
  
set.seed(1)

  
baseline <- paste0(getwd(),"/baseline")  
scenario <- paste0(getwd(),"/scenario")

  



setwd(scenario)

#unlink(tmp,force=T)
file.remove('hyd-sed-lte2.cha')

sink('hyd-sed-lte2.cha', type=c("output"), append = T)

write(stream_rate,'hyd-sed-lte2.cha',sep = "\n",append=T)



sink()

}
