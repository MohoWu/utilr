#' Extract colour codes in ggplot2
#'
#' no export
#' 
ggplot2_colours <- function (n = 2) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' Convert all characters to factors in a data frame
#'
#' @param df Data frame.
#' 
#' @export
#' 

factor_df <- function(df) {
  
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                         as.factor)
  
  df
}






