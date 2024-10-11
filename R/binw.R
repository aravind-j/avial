#' Calculate the Bin Width for Plotting Histograms
#'
#' @param x A numeric vector of the values from which histogram has to be
#'   generated.
#' @param method The method to compute the number of classes for the histogram.
#'
#' @return The bin width
#' @export
#'
#' @seealso \code{\link[grDevices]{nclass}}
#'
#' @examples
#' set.seed(1)
#' x <- stats::rnorm(1111)
#'
#' binw(x = x, method = "fd")
#' binw(x = x, method = "scott")
#' binw(x = x, method = "sturges")
#'
binw <- function(x, method = c("fd", "scott", "sturges")) {
  method <- match.arg(method)

  if (method == "fd") {
    bw <-   pretty(range(x, na.rm = TRUE), n = nclass.FD(na.omit(x)),
                   min.n = 1, right = TRUE)[2] -
      pretty(range(x, na.rm = TRUE), n = nclass.FD(na.omit(x)),
             min.n = 1, right = TRUE)[1]
  }
  if (method == "scott") {
    bw <-   pretty(range(x, na.rm = TRUE), n = nclass.scott(na.omit(x)),
                   min.n = 1, right = TRUE)[2] -
      pretty(range(x, na.rm = TRUE), n = nclass.scott(na.omit(x)),
             min.n = 1, right = TRUE)[1]
  }
  if (method == "sturges") {
    bw <-   pretty(range(x, na.rm = TRUE), n = nclass.Sturges(na.omit(x)),
                   min.n = 1, right = TRUE)[2] -
      pretty(range(x, na.rm = TRUE), n = nclass.Sturges(na.omit(x)),
             min.n = 1, right = TRUE)[1]
  }
  return(bw)
}
