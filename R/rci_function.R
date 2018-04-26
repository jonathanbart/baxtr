#' Calculate the Religious Commitment Inventory
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' and calculates an Intrapersonal Religious Commitment score, an Interpersonal Commitment score, and an overall Religious
#' Commitment score.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Religion, The Religious Commitment Inventory
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Worthington Jr, E. L., Wade, N. G., Hight, T. L., Ripley, J. S., McCullough, M. E., Berry, J. W., ... & O'connor, L. (2003). The Religious Commitment Inventory-10: Development, refinement, and validation of a brief scale for research and counseling. Journal of Counseling Psychology, 50(1), 84-96.
#' @examples
#' x <- c(1:5)
#' df <- data.frame(matrix(sample(x, 10*10, replace = TRUE), nrow = 5, ncol = 10))
#' rci(1, 10, df)
#' @export

rci <- function (start_col, end_col, data) {
  n <- c("rci1", "rci2", "rci3", "rci4", "rci5", "rci6", "rci7", "rci8", "rci9", "rci10")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, to_numeric)
  if(any(data[,n] < 1) | any(data[,n] > 5)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  data$rci_intra <- (data$rci1 + data$rci3 + data$rci4 + data$rci5 + data$rci7 + data$rci8)
  data$rci_inter <- (data$rci2 + data$rci6 + data$rci9 + data$rci10)
  data$rci <- (data$rci1 + data$rci2 + data$rci3 + data$rci4 + data$rci5 + data$rci6 + data$rci7 + data$rci8 + data$rci9 + data$rci10)
  data
}
