#' Custom function to rename the facets
#'
#' @param original_var Column name of the facet label in the ggplot object
#' @param custom_name Characters to add to before the current facet label
#'
#' @return A character vector
#' @export
#'
#' @examples
#' label_facet(rslt$TrueN, "N")
label_facet <- function(original_var, custom_name){
  lev <- levels(as.factor(original_var))
  lab <- paste0(custom_name, lev)
  names(lab) <- lev
  return(lab)  
}
