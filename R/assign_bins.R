assign_bins <- function(bin_df, to_event){
  
  if(is.na(to_event)){
    out <- nrow(bin_df)
  }else{
    out <- which(unlist(map2(bin_df$left, bin_df$right, between, x = to_event)) == T)
    
  }
  return(out)
  
}


