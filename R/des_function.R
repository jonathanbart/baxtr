#' A Simple Descriptive Statistics Function
#'
#' Reports the mean, standard deviation, median, and inter-quartile range. Simple, decluttered output.
#' @param x An R object. Accepts three forms of the extract operator, the slot operator is untested.
#' @param na.rm a logical value indicating whether NA values should be removed before computation. Defaults to FALSE.
#' @keywords
#' Descriptive Statistics
#' @examples
#' x <- sample(1:100, size = 100, replace = TRUE)
#' des(x)
#' @export

des <- function (x, na.rm = FALSE) {
  if (na.rm == TRUE) {
    cat(noquote(paste("Mean:", round(mean(x, na.rm = TRUE), digits = 2))), "\n")
    cat(noquote(paste("SD:", round(stats::sd(x, na.rm = TRUE), digits = 2))), "\n")
    cat(noquote(paste("Median:", round(stats::median(x, na.rm = TRUE), digits = 2))), "\n")
    cat(noquote(paste("IQR:", round(stats::IQR(x, na.rm = TRUE), digits = 2))), "\n")
  } else {
    cat(noquote(paste("Mean:", round(mean(x, na.rm = FALSE), digits = 2))), "\n")
    cat(noquote(paste("SD:", round(stats::sd(x, na.rm = FALSE), digits = 2))), "\n")
    cat(noquote(paste("Median:", round(stats::median(x, na.rm = FALSE), digits = 2))), "\n")
    cat(noquote(paste("IQR:", round(stats::IQR(x, na.rm = FALSE), digits = 2))), "\n")
  }
}

# should include
# n, NA's, mean, SD, median, IQR, min, 1st Q, 3rd Q, max, skew, potential outliers?
# "simple" logical, if TRUE just print basic.
