#' Bartlett Test of Homogeneity of Error Variances
#'
#' @param ems A vector of error variances or error mean sum of squares from each
#'   environment (years and/or locations).
#' @param df A vector of degrees of freedom correspondint to \code{ems}.
#'
#' @return A list with the chi-square value test statistic, corresponding
#'   degrees of freedom and p value.
#' @export
#'
#' @seealso \code{\link[stats]{bartlett.test}}
#'
#' @examples
#' # Example from Page 481 Gomez KA and AA Gomez (1984) Statistical Procedures
#' # for Agricultural Research. 2nd ed. Wiley, New York, 680 p.
#'
#' # Different degrees of freedom
#' ems <- c(6.73920, 1.93496, 1.15500, 10.58450)
#' df <- c(19, 16, 17, 19)
#'
#' ems.bartlett.test(ems = c(6.73920, 1.93496, 1.15500, 10.58450),
#'                   df = c(19, 16, 17, 19))
#'
#' # Same degrees of freedom
#' ems <- c(11.459848, 17.696970, 10.106818)
#' df <- c(20, 20, 20)
#'
#' ems.bartlett.test(ems = c(11.459848, 17.696970, 10.106818),
#'                   df = c(20, 20, 20))
ems.bartlett.test <- function(ems, df) {

  # Checks -  todo


  # Degrees of freedom (f)
  f <- df

  # Error mean sum of squares (s^2)
  s2 <- ems

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
