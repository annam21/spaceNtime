# Anna Moeller
# Starting TTE code
# 11/1/2019

df <- data.frame(
  cam = c(1,1,2,2,2),
  datetime = as.POSIXct(c("2016-01-02 12:00:00",
                          "2016-01-03 13:12:00",
                          "2016-01-02 12:00:00",
                          "2016-01-02 14:00:00",
                          "2016-01-03 16:53:42"),
                        tz = "GMT"),
  count = c(1, 0, 0, 1, 2)
)

deploy <- data.frame(
  cam = c(1, 2, 2, 2),
  start = as.POSIXct(c("2015-12-01 15:00:00",
                       "2015-12-08 00:00:00", 
                       "2016-01-01 00:00:00", 
                       "2016-01-02 00:00:00"),
                     tz = "GMT"),
  end = as.POSIXct(c("2016-01-05 00:00:00", 
                     "2015-12-19 03:30:00", 
                     "2016-01-01 05:00:00",
                     "2016-01-05 00:00:00"), 
                   tz = "GMT"),
  area = c(300, 200, 200, 450)
)

study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")

# note to self: I don't think TTE area can change within an occasion 

occ <- build_occ(samp_freq = 3600 * 10, # seconds between the start of each sampling occasion
                 samp_length = 3600 * 10, # duration of each sampling occasion (seconds)
                 study_start = study_dates[1],
                 study_end = study_dates[2]) 

# Sampling period length
lps <- 2 # length units per second
samp_per <- mean(deploy$area)/lps


# tte_calc_toevent(df, occ, effort)
tmp <- df %>%
  filter(count > 0) %>%
  left_join(effort, .,  by = "cam") %>% 
  filter(datetime %within% int) %>%
  
  # Take only the first event in the sampling occasion
  group_by(cam, occ) %>% 
  filter(!duplicated(occ)) %>% 
  
  # Join back up with all cams and occasions 
  select(cam, occ, datetime, count) %>% 
  left_join(effort, ., by = c("occ", "cam")) %>% 
  # select(cam, occ, start, end, int, area) %>% 
  arrange(cam, occ) %>%
  
  # Calculate TTE
  mutate(TTE = as.numeric(datetime) - as.numeric(start),
         TTE = TTE/samp_per) %>%
  
  # Calculate censor
  mutate(censor = as.numeric(end) - as.numeric(start),
         censor = censor/samp_per) 

# Building encounter history
# in old code, TTE encounter history was a matrix with ncam rows and nocc cols

#tte_build_eh(df, deploy, occ)
# Data checks 
# Build effort for each cam at each occasion
tictoc::tic("effort")
eff <- effort_fn(deploy, occ)
tictoc::toc()
# Calculate TTE and censor
out <- tte_calc_toevent(df, occ, effort)
# return(out)


   
# Check:
# What happens to effort if the occasion is partly missing in deploy? 

# Estimate abundance
tte_eh <- tte_build_eh(df, deploy, occ)
tte_estN_fn(tte_eh, study_area = 1e6)

tte_estN_fn <- function(){}
ste_estN_fn <- function(x, study_area){
  
  dat <- list(toevent = matrix(x$TTE, nrow = length(unique(x$cam))),
              censor = x$censor)
  
  opt <- suppressWarnings(
    stats::optim(log(1/mean(dat$toevent, na.rm = T)), 
                 exp_logl_fn, 
                 x = dat, 
                 control = list(fnscale = -1),
                 hessian = T)
  )
  
  # Estimate of lambda
  estlam <- exp(opt$par)
  
  # estlam is average density per m2
  estN <- estlam * study_area
  
  # Delta method for variance
  varB <- -1 * MASS::ginv(opt$hessian)
  form <- sprintf("~ %f * exp(x1)", study_area)
  SE_N <- msm::deltamethod(g = stats::as.formula(form), mean = opt$par, cov = varB, ses = T)
  
  CI <- logCI(estN, SE_N)
  out <- data.frame(
    N = estN, 
    SE = SE_N
  ) %>%
    bind_cols(CI)
  
  return(out)
}