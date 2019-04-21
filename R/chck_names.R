#' Check that certain names exist
#' 
#' This isn't different from verify except in its output...and I don't use it anymore...
#'
#' @param x a dataframe
#' @param colnames a character vector of column names you want to check exist
#'
#' @return T or F
#' @export
#'
#' @examples chck_names(occ, c("start", "end"))
chck_names <- function(x, colnames){
  all(colnames %in% names(x))
}
# UNUSED