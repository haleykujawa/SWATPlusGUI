# Copy baseline files to scenario so not running scenario with old scenario data

Reset_scenario<-function(baseline_dir, scenario_dir){
  
  unlink(scenario_dir) #clears permission denied error
  my_files<-list.files(baseline_dir)
  #file.remove(scenario_dir)
  file.copy(from=paste0(baseline_dir,"/",my_files), to=paste0(scenario_dir,"/",my_files),overwrite=T,recursive=F,copy.mode=T)
  
  
  
  
  
  
}