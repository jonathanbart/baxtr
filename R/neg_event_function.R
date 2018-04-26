#' Calculate the Negative Life Event Scale
#'
#' This function renames columns; converts all columns to  numerics (only works on columns where inputs are character strings)
#' and calculates a Negative Life Event score.
#' @param start_col The column number where the scale begins, reference by number only.
#' @param end_col The column number where the scale end, reference by number only.
#' @param data The reference dataframe.
#' @keywords
#' Negative Life Event, The Negative Life Event Scale
#' @note
#' This function is designed to work with the validated question order as printed in the reference article. This function will
#' give inaccurate results if question order is different from the published validated scale.
#' @references
#' Disabato, D. J., Kashdan, T. B., Short, J. L., & Jarden, A. (2017). What predicts positive life events that influence the course of depression? A longitudinal examination of gratitude and meaning in life. Cognitive Therapy and Research, 41(3), 444-458.
#' @examples
#' df <- data.frame("var1" = c("None", "Did not happen", "Some"),
#' "var2" = c("Did not happen", "A little","Some"),
#' "var3" = c("A lot", "Did not happen", "Some"),
#' "var4" = c("A lot", "A little", "Some"),
#' "var5" = c("A lot", "Did not happen", "A little"),
#' "var6" = c("bad_event_1", "bad_event_2", "bad_event_3"),
#' "var7" = c("None", "A lot", "A little"),
#' "var8" = c("bad_event_4", "bad_event_5", "bad_event_6"),
#' "var9" = c("A lot", "A little", "None"))
#' neg_event(1, 9, df)
#' @export

neg_event <- function (start_col, end_col, data) {
  n <- c("neg1", "neg2", "neg3", "neg4", "neg5", "neg6_des", "neg6_num", "neg7_des", "neg7_num")
  names(data)[start_col:end_col] = n
  data[,n] <- apply(data[,n], 2, as.character)
  a <- c("neg1", "neg2", "neg3", "neg4", "neg5", "neg6_num", "neg7_num")
  data[,a] <- apply(data[,a], 2, factor, levels = c("Did not happen", "None", "A little", "Some", "A lot"),
                    labels = c(0, 1, 2, 3, 4))
  data[,a] <- apply(data[,a], 2, to_numeric)
  data$neg_event_score <- (data$neg1 + data$neg2+ data$neg3 + data$neg4 + data$neg5 + data$neg6_num + data$neg7_num)/7
  data
}
