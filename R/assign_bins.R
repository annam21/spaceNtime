#' Assign each measure of "to_event" to a bin for the Chi squared test
#'
#' @param bin_df data frame defining the interval of each bin
#' @param to_event observations of "to_event"
#'
#' @return Returns the bin that each observation of to_event falls in
#' @export
#'
#' @examples assign_bins(bin_df, to_event)
assign_bins <- function(bin_df, to_event){
  
  if(is.na(to_event)){
    out <- nrow(bin_df)
  }else{
    out <- which(unlist(purrr::map2(bin_df$left, bin_df$right, between, x = to_event)) == T)
    
  }
  return(out)
  
}


