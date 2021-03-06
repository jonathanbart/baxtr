% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pos_event_function.R
\name{pos_event}
\alias{pos_event}
\title{Calculate the Positive Life Event Scale}
\usage{
pos_event(start_col, end_col, data)
}
\arguments{
\item{start_col}{The column number where the scale begins, reference by number only.}

\item{end_col}{The column number where the scale end, reference by number only.}

\item{data}{The reference dataframe.}
}
\description{
This function renames columns; converts all columns to  numerics (only works on columns where inputs are character strings)
and calculates a Positive Life Event score.
}
\note{
This function is designed to work with the validated question order as printed in the reference article. This function will
give inaccurate results if question order is different from the published validated scale.
}
\examples{
df <- data.frame("var1" = c("None", "Did not happen", "Some"),
"var2" = c("Did not happen", "A little","Some"),
"var3" = c("A lot", "Did not happen", "Some"),
"var4" = c("A lot", "A little", "Some"),
"var5" = c("A lot", "Did not happen", "A little"),
"var6" = c("good_event_1", "good_event_2", "good_event_3"),
"var7" = c("None", "A lot", "A little"),
"var8" = c("good_event_4", "good_event_5", "good_event_6"),
"var9" = c("A lot", "A little", "None"))
pos_event(1, 9, df)
}
\references{
Disabato, D. J., Kashdan, T. B., Short, J. L., & Jarden, A. (2017). What predicts positive life events that influence the course of depression? A longitudinal examination of gratitude and meaning in life. Cognitive Therapy and Research, 41(3), 444-458.
}
\keyword{Event}
\keyword{Event,}
\keyword{Life}
\keyword{Positive}
\keyword{Scale}
\keyword{The}
