#' Interactive time series
#'
#' This function plots an interactive time series using \code{dygraph} package.
#'
#' @param df A dataframe containing variables to plot in a wide format.
#'  Also needs to have a column named \code{date}.
#' @param vars The name of variables to plot. If not supplied, all the numeric 
#'  variables will be plotted. 
#' @param dy.label This specify the column containing additional info to add the 
#'  dygraph. Default is \code{NA}.
#' @param draw.points Draw points in the series? The default is \code{FALSE}.
#' @param point.size Default value is 2, which is generally appropriate. A value
#'  of 0.5 is very subtle.
#' @param line.width Default value is 0.5. 
#' @param tz Time zone of the time series data.#' 
#' @param dy.group This specifies the group of the dygraphs. 
#' If multiple dygraphs are plotted and you want zooming in one of the graphs to be synced with other graphs, 
#' then set all the dygraphs to the same group. 
#' @param ylab Name of y axis.
#' 
#' @examples
#' 
#' @author Hao Wu
#' 
#' @import dygraphs
#' @import dplyr
#' 
#' @export
#' 


dyplot <- function(df, vars, dy.label = NA, tz = "UTC",
                   draw.points = FALSE, point.size = 2, line.width = 0.5, 
                   highlight.individual = FALSE, 
                   dy.group = NA, ylab = NULL) {

  names(df) <- tolower(names(df))
  
  # the data
  if (missing(vars)) {
    num_id = sapply(df, is.numeric)
    theData = df[num_id]
  } else {
    theData = tryCatch(df[vars], 
                       error = function(e) {
                         df[intersect(vars, names(df))]
                       })
  }
  
  # the date
  theDate = df$date
  
  # create time series data
  ts = xts::xts(theData, order.by = theDate, tz = tz)
  
  # plot dygraph
  colour_vector <- ggplot2_colours(NCOL(theData))
  plot <- dygraph(ts, group = dy.group) %>% 
    dyOptions(colors = colour_vector, 
              useDataTimezone = TRUE, 
              drawPoints = draw.points, 
              pointSize = point.size) %>% 
    dySeries(strokeWidth = line.width) %>%
    dyAxis("y", label = ylab) %>%
    dyLegend(width = 400) %>%
    dyRangeSelector()
  
  # additional label in the legend?
  if (!is.na(dy.label)) {
    plot <- plot %>%
      # add label to the hover text
      # taken from https://stackoverflow.com/questions/27671576/how-can-i-get-tooltips-showing-in-dygraphs-without-annotation
      dyCallbacks(
        highlightCallback = sprintf(
          'function(e, x, pts, row) {
          
          // added to illustrate what is happening
          //   remove once satisfied with your code
          debugger;  
          
          var customLegend = %s
          
          
          // should get our htmlwidget
          var legendel = e.target.parentNode.parentNode
          .querySelectorAll(".dygraph-legend")[0];
          
          // should get our htmlwidget
          legendel.innerHTML = legendel.innerHTML + "<br>" + customLegend[row];
  }'
          ,# supply a vector or text that you would like
          jsonlite::toJSON(df[[dy.label]])
        )
      )
  }
  
  # Highlight individual time sereis?
  if (highlight.individual) {
    plot <- plot %>%
      dyLegend(width = 300) %>%
      
      # the following copied from https://stackoverflow.com/questions/35943583/plain-dygraphs-javascript-options-in-r-shiny
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyCSS(textConnection("
     .dygraph-legend > span { display: none; }
     .dygraph-legend > span.highlight { display: inline; }
  ")) 
  }
  
  # return
  plot
  
  
}