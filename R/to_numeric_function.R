#' Convert Object to a Numeric
#'
#' Robustly converts objects to numerics, converts factors to numeric as number dicplayed not reference category.
#' @param x An R object. Accepts three forms of the extract operator, the slot operator is untested.
#' @keywords
#' numeric transform
#' @examples
#' x <- sample(1:100, size = 100, replace = TRUE)
#' to_numeric(x)
#' @export

to_numeric <- function (x) {
  as.numeric(as.character(x))
}
