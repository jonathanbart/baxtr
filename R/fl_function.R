#' Calculate the Flourishing Scale
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' and calculates an overall flourishing score.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Wellbeing, Flourishing, The Flourishing Scale
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Diener, E., Wirtz, D., Tov, W., Kim-Prieto, C., Choi, D. W., Oishi, S., & Biswas-Diener, R. (2010). New well-being measures: Short scales to assess flourishing and positive and negative feelings. Social Indicators Research, 97(2), 143-156.
#' @examples
#' x <- c(1:7)
#' df <- data.frame(matrix(sample(x, 10*10, replace = TRUE), nrow = 5, ncol = 10))
#' fl(1, 10, df)
#' @export

fl <- function (start_col, end_col, data) {
  n <- c("fl1", "fl2", "fl3", "fl4", "fl5", "fl6", "fl7", "fl8", "fl9", "fl10")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, to_numeric)
  if(any(data[,n] < 1) | any(data[,n] > 7)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  data$fl <- (data$fl1 + data$fl2 + data$fl3 + data$fl4 + data$fl5 + data$fl6 + data$fl7 + data$fl8 + data$fl9 + data$fl10)
  data
}
