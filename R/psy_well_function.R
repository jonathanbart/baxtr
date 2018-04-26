#' Calculate the Psychological Wellbeing Scale
#'
#' This function renames columns; converts all columns to numerics; tests if the scores are outside of the scale limits;
#' recodes requiste columns; and calculates domain scores for Autonomy, Environmental Mastery, Personal Growth,
#' Positive Relations, Purpose in Life, Self-acceptance, and Overall wellbeing.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Wellbeing, Psychological Wellbeing Scale
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Ryff, C. D., & Keyes, C. L. M. (1995). The structure of psychological well-being revisited. Journal of Personality and Social Psychology, 69(4), 719.
#' @examples
#' x <- c(1:6)
#' df <- data.frame(matrix(sample(x, 10*42, replace = TRUE), nrow = 5, ncol = 42))
#' psy_well(1, 42, df)
#' @export

psy_well <- function (start_col, end_col, data) {
  n <- c("au1", "em2", "gr3", "rel4", "pu5", "sa6", "au7", "em8", "gr9", "rel10", "pu11", "sa12", "au13", "em14", "gr15",
         "rel16", "pu17", "sa18", "au19", "em20", "gr21", "rel22", "pu23", "sa24", "au25", "em26", "gr27", "rel28", "pu29",
         "sa30", "au31", "em32", "gr33", "rel34", "pu35", "sa36", "au37", "em38", "gr39", "rel40", "pu41", "sa42")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[, n], 2, to_numeric)
  if(any(data[,n] < 1) | any(data[,n] > 6)) {
    cat(crayon::red("Function error: scale limits exceeded"))
    return()
  }
  cols <- c("gr3", "pu5", "rel10", "au13", "em14", "gr15", "rel16", "pu17", "sa18", "au19", "pu23", "em26", "gr27",
            "sa30", "au31", "em32", "rel34", "sa36", "gr39", "pu41")
  cols_recode <- purrr::map_chr(cols, paste0, "_recode")
  data[,cols_recode] <- apply(data[,cols], 2, car::recode, ("1=6; 2=5; 3=4; 4=3; 5=2; 6=1"))
  data$au <- (data$au1 + data$au7 + data$au13_recode + data$au19_recode + data$au25 + data$au31_recode + data$au37)
  data$em <- (data$em2 + data$em8 + data$em14_recode + data$em20 + data$em26_recode + data$em32_recode + data$em38)
  data$gr <- (data$gr3_recode + data$gr9 + data$gr15_recode + data$gr21 + data$gr27_recode + data$gr33 + data$gr39_recode)
  data$rel <- (data$rel4 + data$rel10_recode + data$rel16_recode + data$rel22 + data$rel28 + data$rel34_recode + data$rel40)
  data$pu <- (data$pu5_recode + data$pu11 + data$pu17_recode + data$pu23_recode + data$pu29 + data$pu35 + data$pu41_recode)
  data$sa <- (data$sa6 + data$sa12 + data$sa18_recode + data$sa24 + data$sa30_recode + data$sa36_recode + data$sa42)
  data$over <- (data$au + data$em + data$gr + data$rel + data$pu + data$sa)/6
  data
}
