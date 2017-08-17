#' Extract colour codes in ggplot2
#'
ggplot2_colours <- function (n = 2) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}