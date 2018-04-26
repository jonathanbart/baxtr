#' Calculate the Life Orientation Test - Revised (LOT-R)
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' recodes requiste columns; and calculates a LOT-R score.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Optimism, Life Orientation Test - Revised
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Scheier, M. F., Carver, C. S., & Bridges, M. W. (1994). Distinguishing optimism from neuroticism (and trait anxiety, self-mastery, and self-esteem): A reevaluation of the Life Orientation Test. Journal of Personality and Social Psychology, 67(6), 1063-1078.
#' @examples
#' x <- c(0:4)
#' df <- data.frame(matrix(sample(x, 10*10, replace = TRUE), nrow = 5, ncol = 10))
#' opt(1, 10, df)
#' @export

opt <- function (start_col, end_col, data) {
  n <- c("opt1", "opt2", "opt3", "opt4", "opt5", "opt6", "opt7", "opt8", "opt9", "opt10")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, to_numeric)
  if(any(data[,n] < 0) | any(data[,n] > 4)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  cols <- c("opt3", "opt7", "opt9")
  cols_recode <- purrr::map_chr(cols, paste0, "_recode")
  data[,cols_recode] <- apply(data[,cols], 2, car::recode, ("0=4; 1=3; 2=2; 3=1; 4=0"))
  data$opt <- (data$opt1 + data$opt3_recode + data$opt4 + data$opt7_recode + data$opt9_recode + data$opt10)
  data
}
