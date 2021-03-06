# Functions to validate the required data

#' Validate df
#' 
#' Make sure df fits all requirements
#'
#' @param df object df
#'
#' @return df if it passes all tests. Otherwise returns error
#' @import assertr
#'
#' @examples validate_df(df)
validate_df <- function(df){
  df %>%
    verify(has_all_names("cam", "datetime", "count")) %>%
    verify(lubridate::is.POSIXct(datetime)) %>% 
    verify(is.numeric(count)) %>% 
    verify(count >= 0)
}


#' Validate start and end columns
#' 
#' Validate any object with start and end columns
#' @param x a dataframe or tibble with columns start and end
#'
#' @return x if it passes all tests, an error otherwise
#' @export
#' @import assertr
#'
#' @examples validate_start_end(deploy)
validate_start_end <- function(x){
  x %>% 
    verify(has_all_names("start", "end")) %>% 
    verify(lubridate::is.POSIXct(start)) %>% 
    verify(lubridate::is.POSIXct(end)) %>% 
    # Make sure start and end work together
    verify(lubridate::tz(start) == lubridate::tz(end) ) %>% 
    verify(end - start >= 0) 
}

#' Validate deploy object
#'
#' @param deploy deploy object
#'
#' @return deploy if it passes all tests, error otherwise
#' @export
#' @import assertr
#'
#' @examples validate_deploy(deploy)
validate_deploy <- function(deploy){
  deploy %>% 
    verify(has_all_names("cam", "start", "end", "area")) %>% 
    # Check class of all columns
    verify(is.numeric(area)) %>% 
    verify(area >= 0) %>%
    # Check start and end
    validate_start_end(.)
  
  # See if start to end are length 0 (It's a timelapse effort)
  tl <- deploy %>% 
    filter(start != end ) 
  if(nrow(tl) == 0){
    # It's timelapse, so make sure there is only 1 effort for each time 
    deploy <- deploy %>% 
      distinct() # Force it for the user 
    deploy %>% 
      group_by(cam, start, end) %>% 
      count() %>% 
      filter(n != 1) %>% 
      assertr::verify(nrow(.) == 0)
  } else {
    # Make sure rows non-overlapping
    ov <- find_overlap(deploy)  
    if(nrow(ov) != 0){
      print(ov)
      stop("There are overlapping time intervals in deploy")
    } 
  }
  return(deploy)
}

#' Validate df and deploy together
#'
#' @param df df object
#' @param deploy deploy object
#'
#' @return error if any check fails, nothing otherwise
#' @export
#'
#' @examples validate_df_deploy(df, deploy)
validate_df_deploy <- function(df, deploy){
  stopifnot(lubridate::tz(deploy$start) == lubridate::tz(df$datetime))
  
  # # Fail if a camera in df is not in deploy
  stopifnot(class(df$cam) == class(deploy$cam))
  stopifnot(all(unique(df$cam) %in% deploy$cam) )
  
  # Fail if a camera took a photo but that time is not in deploy
  # Very similar function to find_overlap. Work on that in future
  pic_in_deploy <- left_join(df, deploy, by = "cam") %>% 
      mutate(wthn = datetime >= start & datetime <= end) %>% 
      group_by(cam) %>% 
      summarise(allgood = any(wthn)) %>%
      filter(allgood == F | is.na(allgood))
  
  if(nrow(pic_in_deploy) > 0) stop(paste("There are photos at cam", 
                                         pic_in_deploy$cam, 
                                         "outside intervals specified in deploy"))
  
}


#' Validate occ
#'
#' @param occ occ object
#'
#' @return occ if passes all tests, error otherwise
#' @export
#'
#' @examples validate_occ(occ)
validate_occ <- function(occ){
  occ %>% 
    verify(is.numeric(occ) ) %>%
    # Check start and end
    validate_start_end(.)
}