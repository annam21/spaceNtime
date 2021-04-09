# STE "bootstrap"
# Anna Moeller 
# 4/9/21

# To create multiple sets of sampling occasions and run STE over all of them 

# Function to run a bunch of stuff at once
runests <- function(df, deploy, sp, st_area = "both", occ){
  # Flaws in this function: 
  # 1. dplyr got angry at function arguments being called the same thing as the
  #   column names in df and deploy. I had to change function arguments to 
  #   sp and st_area instead of species and study_area. Could instead use dplyr 
  #   tools to make it recognize name vs. object 
  sdat <- df %>% 
    filter(species == sp)
  
  # If separated by study_area, filter deploy
  if(!(st_area == "both")){
    deploy <- deploy %>% 
      filter(study_area == st_area)
    
    sdat <- sdat %>% 
      filter(cam %in% deploy$cam)
  }
  
  ste_eh <- ste_build_eh(sdat, deploy, occ, quiet = T)
  if(all(is.na(ste_eh$STE))){
    out <- tibble::tibble(N = NA, SE = NA, LCI = NA, UCI = NA)
  } else {
    out <- ste_estN_fn(ste_eh, study_area = 1e8)
  }
  return(out)
}

# # Old study dates
# study_dates <- as.POSIXct(c("2015-01-1 00:00:01", 
#                             "2018-02-01 00:00:01"), 
#                           tz = "GMT")
# New study dates
study_dates <- as.POSIXct(c("2015-08-27 00:00:00", 
                            "2018-02-20 00:00:00"), 
                          tz = "GMT")

# For just one set of occasions
# This took ~17 minutes for 5 minute intervals 
# occas <- build_occ(samp_freq = 3600, # seconds between the start of each sampling occasion
#                    samp_length = 10, # duration of each sampling occasion (seconds)
#                    study_start = study_dates[1],
#                    study_end = study_dates[2])
# x <- crossing(
#   sp = c("deer", "moose", "black bear", "mountain lion", "wolf"),
#   st_area = c("N", "S", "both")
# )
# 
# # Run all estimates 
# Nhat <- x %>%
#   bind_cols(
#     purrr::pmap_dfr(
#       .l = ., 
#       .f = runests,
#       df = dfnew,
#       deploy = dep,
#       occ = occas)
#   )
# 
# # Visualize data in wide format
# Nhat %>% 
#   select(sp, st_area, N) %>% 
#   pivot_wider(names_from = st_area, values_from = N)
# 
# saveRDS(Nhat, "results/STE_estimates_20210318.rds")

# For multiple sets of occasions
sf <- 600
# Each set of occasions begins *by = 60 seconds* later
st <- study_dates[1] + seq(0, sf-1, by = 60)
occas <- tibble(
  occ = lapply(st, build_occ, samp_freq = sf, samp_length = 10, 
               study_end = study_dates[2])
)

# Make all combinations of species/study area for the function above
x <- crossing(
  sp = c("deer", "moose", "black bear", "mountain lion", "wolf"),
  st_area = c("N", "S", "both"),
  occas
)

# Run all estimates 
s <- Sys.time()
Nhat <- x %>%
  bind_cols(
    purrr::pmap_dfr(
      .l = ., 
      .f = runests,
      df = dfnew,
      deploy = dep)
  )
Sys.time() - s

# Calculate 
Nhat %>% 
  group_by(sp, st_area) %>% 
  mutate(var = SE^2) %>% 
  summarize(
    N = mean(N, na.rm = T),
    # If independent, 
    SE_N = (1/n())*sqrt(sum(var, na.rm = T))
  ) %>% 
  # Calculate logCIs
  bind_cols(., logCI(.$N, .$SE_N)) %>% 
  # See how many of the occasion sets got estimates
  left_join(., Nhat %>% filter(!is.na(N)) %>% count(sp, st_area))

# Visualize data in wide format
Nhat %>% 
  group_by(sp, st_area) %>% 
  summarize(N = mean(N, na.rm = T)) %>%
  pivot_wider(names_from = st_area, values_from = N)