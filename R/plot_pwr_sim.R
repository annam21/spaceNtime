#' Plot each iteration from power analysis simulation 
#'
#' @param pwr_sim_rslt Result from ste_sim_fn or tte_sim_fn
#'
#' @return a ggplot
#' @export
#' @import ggplot2
#'
#' @examples
#' rslt <- ste_pwr_sim(
#'   N = c(1, 2),
#'   study_area = c(1e6, 1e8),
#'   ncam = c(30, 50),
#'   nocc = c(1000, 2000),
#'   cam_area = c(100, 150),
#'   niter = 5
#' )
#' plot_pwr_sim(rslt)

plot_pwr_sim <- function(pwr_sim_rslt){
  p <- pwr_sim_rslt %>% 
    tidyr::replace_na(
      list(EstN = 0, 
           SE = 0, 
           LCI = 0, 
           UCI = 0)
    ) %>%
    ggplot(aes(x = factor(NCam),
               y = EstN, 
               ymin = LCI,
               ymax = UCI, 
               color = factor(NOcc)
           )
    ) +
    geom_pointrange(position = position_dodge2(width = 0.5),
                    alpha = 0.8) + 
    geom_hline(aes(yintercept = TrueN), 
               linetype = "twodash") +
    scale_color_brewer(palette = "Set1")+
    coord_flip() +
    theme_classic() +
    xlab("Number of Cameras") + 
    # ylab(bquote('Density Estimate' ~('Individuals 100 km'^-2))) +
    ylab("Density Estimate") +
    labs(color = "Number of Occasions") +
    # theme(legend.position = "none") + # To get rid of legend 
    facet_wrap(~TrueN + StudyArea,
               labeller = labeller(
                 TrueN = label_facet(pwr_sim_rslt$TrueN, "N = "),
                 StudyArea = label_facet(pwr_sim_rslt$StudyArea, "Area: ")
               )
    ) 
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
