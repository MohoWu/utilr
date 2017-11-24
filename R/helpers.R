#' Extract colour codes in ggplot2
#'
#' no export
#' 
ggplot2_colours <- function (n = 2) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#' Build an equation of the lmodel2 regression results
#' 
#' no export
#'

eqn <- function(df, form, method) {
  n = which(method == c("OLS", "MA", "SMA"))
  m = lmodel2::lmodel2(form, df)
  eq <- substitute(italic(y) == a + b %.% italic(x) * "," ~ ~italic(r)^2 ~ "=" ~ r2, 
                   list(a = format(m$regression.results$Intercept[n],nsmall = 2, digits = 3), 
                        b = format(m$regression.results$Slope[n], nsmall = 2, digits = 3), 
                        r2 = round(m$rsquare, digits = 2)))
  as.character(as.expression(eq))
}

#' Get the regression results from lmodel2
#'
#' no export
#' 

lmStat <- function(form, data){
  mod <- lmodel2::lmodel2(form, data, nperm = 99)
  return(mod$regression.results)
}

#' Spread a number
#' 
#' no export
#' 
spread_num <- function(n, spread.by, spread.length) {
  
  x <- floor(spread.length/2)
  seq(from = n - x * spread.by,
      to = n + x * spread.by,
      length.out = spread.length)
  
}

#' Trim white spaces
#' 
#' Returns string w/o leading or trailing whitespace.
#' 
#' @export
#' 
str_trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' Replace white space(s)with underscore
#'
#' Returns string separated by underscore.
#'   
#' @export
#' 
str_underscore <- function(x) gsub('([[:punct:]])|\\s+', '_', x)


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






