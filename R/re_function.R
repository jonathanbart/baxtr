#' Calculate the Brief Resilience Scale
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' recodes requiste columns; and calculates a Brief Resilience Scale score.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Resilience, Brief Resilience Scale
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Smith, B. W., Dalen, J., Wiggins, K., Tooley, E., Christopher, P., & Bernard, J. (2008). The brief resilience scale: assessing the ability to bounce back. International Journal of Behavioral Medicine, 15(3), 194-200.
#' @examples
#' x <- c(1:5)
#' df <- data.frame(matrix(sample(x, 10*6, replace = TRUE), nrow = 5, ncol = 6))
#' re(1, 6, df)
#' @export

re <- function (start_col, end_col, data) {
  n <- c("re1", "re2", "re3", "re4", "re5", "re6")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, to_numeric)
  if(any(data[,n] < 1) | any(data[,n] > 5)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  cols <- c("re2", "re4", "re6")
  cols_recode <- purrr::map_chr(cols, paste0, "_recode")
  data[,cols_recode] <- apply(data[,cols], 2, car::recode, ("1=5; 2=4; 3=3; 4=2; 5=1"))
  data$re <- (data$re1 + data$re2_recode + data$re3 + data$re4_recode + data$re5 + data$re6_recode)/6
  data
}
