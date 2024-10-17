#' Function to Add Normal Distribution Curve in \code{\link[ggplot2]{ggplot2}}
#' histogram
#'
#' Enhancement of \code{\link[stats]{dnorm}} to plot normal distribution or bell
#' curve as an overlay in \code{\link[ggplot2]{ggplot2}} histograms according to
#' number of records and bin width.
#'
#' @inheritParams stats::dnorm
#' @param x vector of values.
#' @param n The number of records or data points used to plot the histogram.
#' @param bw The bin width of the histogram.
#'
#' @return The density for normal distribution.
#'
#' @importFrom stats dnorm
#' @export
#'
#' @seealso \code{\link[ggplot2]{geom_histogram}},
#'   \code{\link[avial]{groupwise_histogram}}
#'
#' @examples
#' dnorm(0) * 25 == dnorm_ggplot(0, mean = 0, sd = 1, n = 5, bw = 5)
#' dnorm(1) * 21 == dnorm_ggplot(1, mean = 0, sd = 1, n = 7, bw = 3)
#'
dnorm_ggplot <- function(x, mean, sd, n, bw) {
  dnorm(x = x, mean = mean, sd = sd) * n * bw
}
