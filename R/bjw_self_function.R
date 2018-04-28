#' Calculate the Just World Scale for the Self
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' and calculates a Belief in a Just World for the self score.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Belief in a Just World, Just World Scales
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Lipkus, I. M., & Bissonnette, V. L. (1996). Relationships among belief in a just world, willingness to accommodate, and marital well-being. Personality and Social Psychology Bulletin, 22(10), 1043-1056.
#' @examples
#' x <- c(1:7)
#' df <- data.frame(matrix(sample(x, 10*8, replace = TRUE), nrow = 5, ncol = 8))
#' bjw_self(1, 8, df)
#' @export

bjw_self <- function (start_col, end_col, data) {
  n <- c("bjw_self1", "bjw_self2", "bjw_self3", "bjw_self4", "bjw_self5", "bjw_self6", "bjw_self7", "bjw_self8")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, to_numeric)
  if(any(data[,n] < 1) | any(data[,n] > 7)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  data$bjw_self <- (data$bjw_self1 + data$bjw_self2 + data$bjw_self3 + data$bjw_self4 + data$bjw_self5 + data$bjw_self6 +
                      data$bjw_self7 + data$bjw_self8)/8
  data
}
