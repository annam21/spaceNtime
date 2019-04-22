#' Calculate censor for each time step
#'
#' @param effort dataframe built from effort_fn()
#'
#' @return summarised dataframe with occ and censor columns
#' @export
#'
#' @examples 
#'  occ <- build_occ(samp_freq = 3600, 
#'             samp_length = 10,
#'             study_start = study_dates[1],
#'             study_end = study_dates[2])
#' eff <- effort_fn(deploy, occ)
#' calc_censor(eff)
ste_calc_censor <- function(effort){
  # Just requires effort 
  effort %>% 
    group_by(occ) %>% 
    summarise(censor = sum(area))
}