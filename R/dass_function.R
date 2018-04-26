#' Calculate the Depression, Anxiety, and Stress Scale (DASS-21)
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' and calculates domain scores for depression, anxiety, and stress.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Depression, Anxiety, Stress, Depression Anxiety and Stress Scale 21
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Henry, J. D., & Crawford, J. R. (2005). The short‐form version of the Depression Anxiety Stress Scales (DASS‐21): Construct validity and normative data in a large non‐clinical sample. British Journal of Clinical Psychology, 44(2), 227-239.
#' @examples
#' x <- c(0:3)
#' df <- data.frame(matrix(sample(x, 10*21, replace = TRUE), nrow = 5, ncol = 21))
#' dass(1, 21, df)
#' @export

dass <- function (start_col, end_col, data) {
  n <- c("dass1", "dass2", "dass3", "dass4", "dass5", "dass6", "dass7", "dass8", "dass9", "dass10", "dass11",
         "dass12", "dass13", "dass14", "dass15", "dass16", "dass17", "dass18", "dass19", "dass20", "dass21")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[, n], 2, to_numeric)
  if(any(data[,n] < 0) | any(data[,n] > 3)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  data$dep <- (data$dass3 + data$dass5 + data$dass10 + data$dass13 + data$dass16 + data$dass17 + data$dass21)
  data$anx <- (data$dass2 + data$dass4 + data$dass7 + data$dass9 + data$dass15 + data$dass19 + data$dass20)
  data$stress <- (data$dass1 + data$dass6 + data$dass8 + data$dass11 + data$dass12 + data$dass14 + data$dass18)
  data
}
