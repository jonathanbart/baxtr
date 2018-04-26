#' Normality Test
#'
#' Produce a distribution plot including a density curve, mean (red line), and median (blue line).
#' A Shapiro-Wilk normality test is also run and results printed to console. Remember, if the Shapiro-Wilk test is significant the
#' data is non-normally distributed.
#' @param x An R object. Accepts three forms of the extract operator, the slot operator is untested.
#' @keywords
#' Normal Distribution
#' @examples
#' x <- sample(1:100, size = 1000, replace = TRUE)
#' norm_check(x)
#' @export

norm_check <- function (x) {
  lab <- deparse(substitute(x))
  cat(noquote(paste("Results for", lab, "\n")))
  print(stats::shapiro.test(x))
  car::qqPlot(x, lwd = 1, grid = FALSE, main = lab)
  hist <- hist(x, main = lab, xlab = lab, breaks = 25, col = "gainsboro")
  mult <- hist$counts / hist$density
  dens <- stats::density(x, na.rm = TRUE)
  dens$y <- dens$y * mult[1]
  graphics::lines(dens, lwd = 1.8)
  graphics::abline(v = mean(x, na.rm = TRUE), col = "firebrick1", lty = 2)
  graphics::abline(v = stats::median(x, na.rm = TRUE), col = "dodgerblue1", lty = 2)
  graphics::legend('topright', legend = c("Mean", "Median"),
         col = c("firebrick1", "dodgerblue1"), lty = 2:2, cex = 0.8)
}

# ideas for this function
# be able to specify which normailty test to use.
# use MASS::truehist instead of hist function?
# turn on descriptive statistics
