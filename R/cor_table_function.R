#' Correlation Table with Significance Stars
#'
#' Generates a correlation table with significance stars.
#' @param x is a data frame containing the data.
#' @param method : correlation method. "pearson"" or "spearman"" is supported.
#' @param removeTriangle : remove upper or lower triangle.
#' @param result :  if "html" or "latex" the results will be displayed in html or latex format.
#' @keywords
#' Correlation Table
#' @examples
#' x <- data.frame(replicate(5, rnorm(5)))
#' cor_table(x)
#' @export

cor_table <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                     result=c("none", "html", "latex")){
  x <- as.matrix(x)
  correlation_matrix <- Hmisc::rcorr(x, type=method[1])
  R <- correlation_matrix$r
  p <- correlation_matrix$P

  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable::xtable(Rnew), type="html")
    else print(xtable::xtable(Rnew), type="latex")
  }
}
