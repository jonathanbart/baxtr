#' Calculate the Intrinsic Spirituality Scale
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' and calculates an overall spirituality score.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Spirituality, The Intrinsic Spirituality Scale
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Hodge, D. R. (2003). The intrinsic spirituality scale: A new six-item instrument for assessing the salience of spirituality as a motivational construct. Journal of Social Service Research, 30(1), 41-61.
#' @examples
#' x <- c(0:10)
#' df <- data.frame(matrix(sample(x, 10*6, replace = TRUE), nrow = 5, ncol = 6))
#' spr(1, 6, df)
#' @export

spr <- function (start_col, end_col, data) {
  n <- c("spr1", "spr2", "spr3", "spr4", "spr5", "spr6")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, to_numeric)
  if(any(data[,n] < 0) | any(data[,n] > 10)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  cols <- c("spr2", "spr4", "spr6")
  cols_recode <- purrr::map_chr(cols, paste0, "_recode")
  data[,cols_recode] <- apply(data[,cols], 2, car::recode, ("0=10; 1=9; 2=8; 3=7; 4=6; 5=5; 6=4; 7=3; 8=2; 9=1; 10=0"))
  data$spr <- (data$spr1 + data$spr2_recode + data$spr3 + data$spr4_recode + data$spr5 + data$spr6_recode)/6
  data
}
