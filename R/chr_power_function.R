#' Calculate the Generalise Sense of Power Scale
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' recodes requiste columns; and calculates the sense of power score.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Power, The Generalise Sense of Power Scale
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Anderson, C., John, O. P., & Keltner, D. (2012). The personal sense of power. Journal of Personality, 80(2), 313-344.
#' @examples
#' x <- c(1:7)
#' df <- data.frame(matrix(sample(x, 10*8, replace = TRUE), nrow = 5, ncol = 8))
#' chr_power(1, 8, df)
#' @export

chr_power <- function (start_col, end_col, data) {
  n <- c("pwr1", "pwr2", "pwr3", "pwr4", "pwr5", "pwr6", "pwr7", "pwr8")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, to_numeric)
  if(any(data[,n] < 1) | any(data[,n] > 7)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  cols <- c("pwr2", "pwr4", "pwr6", "pwr7")
  cols_recode <- purrr::map_chr(cols, paste0, "_recode")
  data[,cols_recode] <- apply(data[,cols], 2, car::recode, ("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
  data$pwr <- (data$pwr1 + data$pwr2_recode + data$pwr3 + data$pwr4_recode + data$pwr5 + data$pwr6_recode +
                 data$pwr7_recode + data$pwr8)/8
  data
}
