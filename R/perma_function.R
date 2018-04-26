#' Calculate the PERMA Profiler
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' and calculates domain scores for Positive emotion, Engagement, Relationships, Meaning, Accomplishment, General health,
#' Negative emotion, and overall wellbeing.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Wellbeing, PERMA Profiler
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Butler, J., & Kern, M. L. (2016). The PERMA-Profiler: A brief multidimensional measure of flourishing. International Journal of Wellbeing, 6(3), 1-48.
#' @examples
#' x <- c(0:10)
#' df <- data.frame(matrix(sample(x, 10*23, replace = TRUE), nrow = 5, ncol = 23))
#' perma(1, 23, df)
#' @export

perma <- function (start_col, end_col, data) {
  n <- c("a1", "e1", "p1", "n1", "a2", "h1", "m1", "r1", "m2", "e2", "lon", "h2", "p2", "n2",
         "a3", "n3", "e3", "h3", "r2", "m3", "r3", "p3", "hap")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[, n], 2, to_numeric)
  if(any(data[,n] < 0) | any(data[,n] > 10)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  data$p <- (data$p1 + data$p2 + data$p3)/3
  data$e <- (data$e1 + data$e2 + data$e3)/3
  data$r <- (data$r1 + data$r2 + data$r3)/3
  data$m <- (data$m1 + data$m2 + data$m3)/3
  data$a <- (data$a1 + data$a2 + data$a3)/3
  data$gh <- (data$h1 + data$h2 + data$h3)/3
  data$n <- (data$n1 + data$n2 + data$n3)/3
  data$over <- (data$p1 + data$p2 + data$p3 + data$e1 + data$e2 + data$e3 + data$r1 + data$r2 + data$r3 + data$m1 + data$m2 +
                  data$m3 + data$a1 +data$a2 +data$a3 + data$hap)/16
  data
}
