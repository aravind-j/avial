#' Bartlett's Test of Homogeneity of Error Variances
#'
#' Perform chi-square test for homogeneity of variance (Bartlett's test) to test
#' equality of several error variances or mean squared errors
#' \insertCite{gomez_Statistical_1984}{avial}.
#'
#' @param mse A vector of error variances or mean squared errors from each
#'   environment (years and/or locations).
#' @param df A vector of degrees of freedom corresponding to \code{mse}.
#'
#' @return A list with the chi-square value test statistic, corresponding
#'   degrees of freedom and p value.
#'
#' @importFrom stats pchisq
#' @export
#'
#' @seealso \code{\link[stats]{bartlett.test}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' # Examples from Page 467-471 Gomez KA and AA Gomez (1984) Statistical
#' # Procedures for Agricultural Research. 2nd ed. Wiley, New York, 680 p.
#'
#' # Different degrees of freedom
#' mse <- c(6.73920, 1.93496, 1.15500, 10.58450)
#' df <- c(19, 16, 17, 19)
#'
#' mse.bartlett.test(mse = c(6.73920, 1.93496, 1.15500, 10.58450),
#'                   df = c(19, 16, 17, 19))
#'
#' # Same degrees of freedom
#' mse <- c(11.459848, 17.696970, 10.106818)
#' df <- c(20, 20, 20)
#'
#' mse.bartlett.test(mse = c(11.459848, 17.696970, 10.106818),
#'                   df = c(20, 20, 20))
mse.bartlett.test <- function(mse, df) {

  # Checks -  todo


  # Degrees of freedom (f)
  f <- df

  # Error mean sum of squares (s^2)
  s2 <- mse

  k <- length(f)

  fs2 <- f * s2

  logs2 <- log10(s2)

  flogs2 <- f * logs2

  finv <- 1/f

  A <- sum(f)
  B <- sum(fs2)
  C <- sum(flogs2)
  D <- sum(finv)

  # Pooled variance
  s2p <- B / A

  # Chi square value

  chisqv <- (2.3026 * ((A * log10(s2p)) - C)) /
    1 + (1/(3 * (k - 1))) * (D - (1 / A))


  p.value <- pchisq(q = chisqv, df = k - 1, lower.tail = FALSE)


  out <- list(chisq.value = chisqv, df = k - 1, p.value = p.value)


  return(out)


}
