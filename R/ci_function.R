#' Standard Confidence Interval Calculation
#'
#' Reports the mean, confidence interval, lower confidence interval, and upper confidence interval for a range of scores.
#' @param x An R object. Accepts three forms of the extract operator, the slot operator is untested.
#' @param y Define the confidence interval. Defaults to 0.95.
#' @param na.rm a logical value indicating whether NA values should be removed before computation. Defaults to FALSE.
#' @keywords
#' Confidence Interval
#' @examples
#' x <- sample(1:100, size = 100, replace = TRUE)
#' ci(x)
#' @export

ci <- function(x, y = 0.95, na.rm = FALSE) {
  if (na.rm == TRUE) data <- stats::na.omit(x) else data <- x
  n <- (length(data))
  m <- mean(data)
  conf_level <- y
  z <- stats::qt((1 + conf_level) / 2, df = n - 1)
  se <- stats::sd(data) / sqrt(n)
  ci <- z*se
  if (na.rm == TRUE) cat(noquote(paste("Cases deleted:", length(x) - length(data) )), "\n")
  cat(noquote(paste("Mean:", round(m, digits = 2))), "\n")
  cat(noquote(paste("CI:", round(ci, digits = 2))), "\n")
  cat(noquote(paste("Lower CI:", round(m - ci, digits = 2))), "\n")
  cat(noquote(paste("Upper CI:", round(m + ci, digits = 2))), "\n")
}

# make more general so can calculate for any point etimate.
