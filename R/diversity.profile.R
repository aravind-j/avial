
#' Generate Diversity Profiles for Parametric Indices
#'
#' @param x A numeric or factor vector of observations.
#' @param group A factor vector indicating the group of each observation. Must
#'   have the same length as \code{x}.
#' @param q The order of the parametric index.
#' @param conf Confidence level of the interval. Default is 0.95.
#' @param R Integer specifying the number of permutations. Default is 1000.
#' @param parameter The parametric index. Options include \code{"hill"},
#'   \code{"renyi"} and \code{"tsallis"}. Default is \code{"hill"}.
#' @param ci.type A vector of character strings representing the type of
#'   intervals required. The value should be any subset of the values
#'   \code{c("norm", "basic", "stud", "perc", "bca")} or simply \code{"all"}
#'   which will compute all five types of intervals.
#' @inheritParams boot::boot
#' @param ...
#'
#' @returns A list of data frames with the following columns for each factor
#'   level in \code{group}. \describe{ \item{q}{} \item{observed}{}
#'   \item{mean}{} \item{lower}{} \item{upper}{} }
#' @export
#'
#' @examples
diversity.profile <- function(x, group, q = seq(0, 3, 0.1),
                              conf = 0.95, R = 1000,
                              parameter = c("hill", "renyi", "tsallis"),
                              ci.type = c("perc", "bca"),
                              parallel = c("no", "multicore", "snow"),
                              ncpus = getOption("boot.ncpus", 1L),
                              cl = NULL, ...) {

  parallel <- match.arg(parallel)
  ci.type <- match.arg(ci.type)  # only one CI type
  parameter <- match.arg(parameter)

  groups <- levels(group)
  results <- vector("list", length(groups))
  names(results) <- groups

  param.fun <- switch(parameter,
                      renyi   = renyi_stat_ci,
                      tsallis = tsallis_stat_ci,
                      hill    = hill_stat_ci)

  # Loop over groups
  for (g in groups) {

    xg <- x[group == g]

    # Compute bootstrap CI
    b_res <-
      bootstrap.ci(xg, fun = param.fun, R = R,
                   conf = conf, type = ci.type, parallel = parallel,
                   ncpus = ncpus, cl = cl, q = q, ...)

    # Convert results to data.frame per group
    ci_mat <- b_res[[ci.type]]  # just the selected CI type
    df <- data.frame(q = q,
                     observed = attr(b_res, "observed"),
                     mean = attr(b_res, "mean"),
                     lower = ci_mat["lower", ],
                     upper = ci_mat["upper", ])

    results[[g]] <- df
  }

  attr(results, "R") <- R
  attr(results, "conf") <- conf
  attr(results, "parameter") <- parameter
  attr(results, "ci.type") <- ci.type

  return(results)
}

hill_stat_ci <- function(x, q = c(0, 1, 2)) {
  vapply(q, function(qq) {
    hill_number(x, qq)
  }, numeric(1))
}

renyi_stat_ci <- function(x, q = c(0, 1, 2), base = exp(1)) {
  vapply(q, function(qq) {
    renyi_entropy(x, qq, base = base)
  }, numeric(1))
}

tsallis_stat_ci <- function(x, q = c(0, 1, 2), base = exp(1)) {
  vapply(q, function(qq) {
    tsallis_entropy(x, qq, base = base)
  }, numeric(1))
}

