#' Scatter plot with ggplot2
#'
#' Handy scatter plot using ggplot2 that can display regression calculated by lmodel2.
#'
#' @param data A data frame.
#' @param x,y What to plot as x and y variable.
#' @param method Which regression analysis to calculate. There are three methods 
#'  to choose: "OLS"(ordinary least square), "MA"(Major Axis) and "SMA"
#'  (Standardised Major Axis or known as Reduced Major Axis). There are 
#'  calculated using the \code{\link[lmodel2]{lmodel2}} function. Default is "OLS".
#' @param mod.line Should 1:1, 2:1 and 1:2 lines be drawn. Default is \code{FALSE}.
#' @param group The column name by which the data should be grouped.
#' @param same.panel Should all the groups be plotted in the same panel. 
#'  Default is \code{TRUE}. If not they will be separated in different facets as 
#'  in \code{ggplot2}.
#' @param xpos,ypos The relative x and y positions where the regression equations 
#'  should be added. This should be a value between 0 and 1. This value multiples
#'  the maximum of the x or y limit gives the (x, y) coordinates of the annotation.
#' @param pt.col,pt.shape The colour and shape of the points if only 1 group is plotted.
#' @param ... Arguments passed to \code{\link[ggplot2]{aes}}
#' 
#' 
#' @details
#' 
#' @return A ggplot2 object
#' 
#' @import ggplot2 
#' 
#' @author Hao Wu
#' 
#' @export
#' 

plot_lmodel2 <- function (data, x, y, method = "OLS", mod.line = F, group = NA, 
                          same.panel = TRUE, xpos = 0.3, ypos = 0.8, 
                          pt.col = "blue", pt.shape = 1, ...) 
{

  if(is.na(group)) {
    data <- data[complete.cases(data[,c(x,y)]),]
    fit <- lmodel2(as.formula(paste(y, x, sep = "~")), data)
    n = which(method %in% c("OLS", "MA", "SMA"))
    plot <- ggplot(data, aes_string(x = x, y = y, ...)) + 
      geom_point(colour = pt.col, shape = pt.shape) + 
      geom_abline(intercept = fit$regression.results$Intercept[n], 
                  slope = fit$regression.results$Slope[n], col = "red") + 
      geom_abline(intercept = fit$confidence.intervals[n, 2], slope = fit$confidence.intervals[n, 5], col = "grey") + 
      geom_abline(intercept = fit$confidence.intervals[n, 3], slope = fit$confidence.intervals[n, 4], col = "grey") + 
      annotate("text", x = xpos*max(data[[x]], na.rm = T), ypos*max(data[[y]], na.rm = T), 
               label = eqn(data, as.formula(paste(y, x, sep = "~")), method), parse = TRUE, col = pt.col) +
      theme_classic()
    
  }
  if(!is.na(group)) {
    data <- data[complete.cases(data[,c(x,y,group)]),]
    regDF <- plyr::ddply(data, plyr::as.quoted(group), lmStat, form = as.formula(paste(y, x, sep = "~")))
    if(same.panel){
      eqnDF <- data.frame(text_eqn = plyr::daply(data, plyr::as.quoted(group), eqn, form = as.formula(paste(y, x, sep = "~")), method = method),
                          text_x = xpos*max(data[[x]], na.rm = T),
                          text_y = spread_num(ypos, 0.05, length(unique(data[[group]]))) * max(data[[y]], na.rm = T) 
                          )
      eqnDF$text_group <- rownames(eqnDF)
      plot <- ggplot(data, aes_string(x = x, y = y, colour = group)) + 
        geom_point() +
        geom_abline(data = subset(regDF, Method == method), 
                    aes_string(intercept = "Intercept", slope = "Slope", colour = group)) +
        geom_text(aes(x = text_x, y = text_y, colour = text_group, label = text_eqn), data = eqnDF, parse = T, show.legend = FALSE) +
        theme_classic()		
    }
    if(!same.panel) {
      eqnDF <- data.frame(text_eqn = plyr::daply(data, plyr::as.quoted(group), eqn, form = as.formula(paste(y, x, sep = "~")), method = method),
                          text_x = xpos*max(data[[x]], na.rm = T),
                          text_y = ypos*max(data[[y]], na.rm = T))
      eqnDF[[group]] <- rownames(eqnDF)
      plot <- ggplot(data, aes_string(x = x, y = y)) + 
        geom_point(aes_string(...)) +
        geom_abline(data = subset(regDF, Method == method), 
                    aes_string(intercept = "Intercept", slope = "Slope"), col = "red") +
        geom_text(data = eqnDF,aes(text_x, text_y,label = text_eqn), parse = TRUE, show.legend = FALSE) +
        facet_wrap(as.formula(paste("~", group, sep = ""))) + 
        theme_classic()
    }
  }
  
  if(mod.line) {
    plot = plot + geom_abline(intercept = 0, slope = 1) + 
      geom_abline(intercept = 0, slope = 2, linetype = 2) +
      geom_abline(intercept = 0, slope = 0.5, linetype = 2)
  }
  
  return(plot)
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
