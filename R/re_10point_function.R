#' Calculate the Brief Resilience Scale with a 10-point response scale
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' recodes requiste columns; and calculates a Brief Resilience Scale score on a 10-point scale. The question order should be
#' the three positively worded questions followed by the three negatively worded questions.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Resilience Brief Resilience Scale 10-point
#' @note
#' This function is designed to work with a question order where the first three questions are positive and the second
#' three questions are negative. This function will give inaccurate results if question order is different.
#' @references
#' The Brief Resilience Scale as used by the SAHMRI Wellbeing and Resilience Centre.
#' @examples
#' x <- c(0:10)
#' df <- data.frame(matrix(sample(x, 10*6, replace = TRUE), nrow = 5, ncol = 6))
#' re_10point(1, 6, df)
#' @export

re_10point <- function (start_col, end_col, data) {
  n <- c("re1", "re2", "re3", "re4", "re5", "re6")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, to_numeric)
  if(any(data[,n] < 0) | any(data[,n] > 10)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  cols <- c("re4", "re5", "re6")
  cols_recode <- purrr::map_chr(cols, paste0, "_recode")
  data[,cols_recode] <- apply(data[,cols], 2, car::recode, ("0=10; 1=9; 2=8; 3=7; 4=6; 5=5; 6=4; 7=3; 8=2; 9=1; 10=0"))
  data$re <- (data$re1 + data$re2 + data$re3 + data$re4_recode + data$re5_recode + data$re6_recode)/6
  data
}
