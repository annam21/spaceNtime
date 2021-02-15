#' Plot coefficient of variation from power analysis simulations
#'
#' @param pwr_sim_rslt Result from ste_sim_fn or tte_sim_fn
#'
#' @return a ggplot
#' @export
#'
#' @examples
#'rslt <- ste_pwr_sim(
#'   N = c(1, 2),
#'   study_area = c(1e6, 1e8),
#'   ncam = c(30, 50),
#'   nocc = c(1000, 2000),
#'   cam_area = c(100, 150),
#'   niter = 5
#' )
#' plot_pwr_cv(rslt)
plot_pwr_cv <- function(pwr_sim_rslt){
  cv <- pwr_sim_rslt %>% 
    group_by(TrueN, NCam, NOcc, StudyArea, CamArea) %>% 
    summarize(mean.estN = mean(EstN, na.rm = T),
              sd.estN = sd(EstN, na.rm = T)) %>% 
    mutate(cv = sd.estN/mean.estN * 100,
           NOcc = as.factor(NOcc)) # For ggplot
  
  p <- ggplot(cv) + 
    geom_point(aes(x = NCam, 
                   y = cv,
                   color = NOcc)) + 
    labs(x = "Number of cameras",
         y = "CV (%)",
         color = "Number of occasions") + 
    geom_line(aes(x = NCam,
                  y = cv,
                  group = NOcc,
                  color = NOcc),
              linetype = "dashed") +
    scale_color_brewer(palette = "Set1")+
    facet_wrap(~TrueN + StudyArea,
               labeller = labeller(
                 TrueN = label_facet(pwr_sim_rslt$TrueN, "N = "),
                 StudyArea = label_facet(pwr_sim_rslt$StudyArea, "Area: ")
               )
    ) +
    # ylim(0, 50) + # To zoom in
    theme_classic()
  
  # I don't mind having N and Area in each. Overwrite if there's also >1 CamArea
  if(nrow(distinct(pwr_sim_rslt, CamArea)) > 1){
    p <- p + 
      facet_wrap(~TrueN + StudyArea + CamArea,
                 labeller = labeller(
                   TrueN = label_facet(pwr_sim_rslt$TrueN, "N = "),
                   StudyArea = label_facet(pwr_sim_rslt$StudyArea, "Area: "),
                   CamArea = label_facet(pwr_sim_rslt$CamArea, "CamArea: ")
                 )
      )
  }
  return(p)
}
