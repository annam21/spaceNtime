ise_build_eh <- function(df, deploy, occ, assume0 = T){
  # This is currently for deploy version 2 only 
  # Assume0 = T if you want to assume no picture = 0 animals
  # Assume0 = F if you want to assume no picture = not working. 
  
  # Run all my data checks here
  df <- validate_df(df)
  deploy <- validate_deploy(deploy)
  occ <- validate_occ(occ)
  validate_df_deploy(df, deploy) # This one doesn't return anything
  
  # I could force a data subset here, but it all hinges on occ. 
  # If occ is correct, everything else will be. 
  
  # Build effort for each cam at each occasion
  eff <- effort_fn(deploy, occ)
  
  ### All lines until here are same as in STE... think about new fn. 
  
  # Build ISE EH
  ise <- eff %>% 
    left_join(., df, by = "cam") %>%
    filter(datetime %within% int) %>%
    select(occ, cam, count) %>%
    left_join(eff, ., by = c("occ", "cam")) %>%
    select(-int)
  
  # timelapse: NA means that camera wasn't on. 
  # motion sensor: we will assume NA should be 0
  if(assume0){
    ise <- ise %>% 
      mutate(count = replace(count, is.na(count), 0)) 
  } else {
    ise <- ise %>% 
      mutate(area = replace(area, is.na(count), 0))
  }
 return(ise)
}
