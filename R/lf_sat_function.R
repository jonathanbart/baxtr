#' Calculate the Satisfaction with Life Scale
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' and calculates an overall satisfaction with life score.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Wellbeing, Satisfaction with Life, The Satisfaction with Life Scale
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Diener, E. D., Emmons, R. A., Larsen, R. J., & Griffin, S. (1985). The satisfaction with life scale. Journal of Personality Assessment, 49(1), 71-75.
#' @examples
#' x <- c(1:7)
#' df <- data.frame(matrix(sample(x, 10*5, replace = TRUE), nrow = 5, ncol = 5))
#' lf_sat(1, 5, df)
#' @export

lf_sat <- function (start_col, end_col, data) {
  n <- c("sat1", "sat2", "sat3", "sat4", "sat5")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, to_numeric)
  if(any(data[,n] < 1) | any(data[,n] > 7)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  data$lf_sat <- (data$sat1 + data$sat2 + data$sat3 + data$sat4 + data$sat5)
  data
}
