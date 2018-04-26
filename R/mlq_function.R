#' Calculate the Meaning in Life Questionnaire
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' recodes requiste columns; and calculates the domains of presence and search for meaning in life.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Meaning in Life, The Meaning in Life Quesitionnaire
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Steger, M. F., Frazier, P., Oishi, S., & Kaler, M. (2006). The meaning in life questionnaire: Assessing the presence of and search for meaning in life. Journal of Counseling Psychology, 53(1), 80-93.
#' @examples
#' x <- c(1:7)
#' df <- data.frame(matrix(sample(x, 10*10, replace = TRUE), nrow = 5, ncol = 10))
#' mlq(1, 10, df)
#' @export

mlq <- function (start_col, end_col, data) {
  n <- c("mlq1", "mlq2", "mlq3", "mlq4", "mlq5", "mlq6", "mlq7", "mlq8", "mlq9", "mlq10")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, to_numeric)
  if(any(data[,n] < 1) | any(data[,n] > 7)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  data$mlq9_recode <- car::recode(data$mlq9, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
  data$mlq_presence <- (data$mlq1 + data$mlq4 + data$mlq5 + data$mlq6 + data$mlq9_recode)
  data$mlq_search <- (data$mlq2 + data$mlq3 + data$mlq7 + data$mlq8 + data$mlq10)
  data
}
