
#' Bootstrap Confidence Intervals
#'
#' This function generates bootstrap resamples using \code{\link[boot]{boot}}
#' and computes confidence intervals using several standard bootstrap methods
#' via \code{\link[boot]{boot.ci}}.
#'
#' Supported interval types include normal approximation, basic, studentized
#' (bootstrap-t), percentile, and bias-corrected and accelerated (BCa)
#' intervals. If a requested interval type cannot be computed (for example,
#' studentized or BCa intervals), the function falls back to percentile
#' intervals.
#'
#' @param x A numeric or factor vector of observations.
#' @param fun A function to summarize the observations.
#' @param R Integer specifying the number of permutations. Default is 1000.
#' @inheritParams boot::boot
#' @param ... Additional arguments passed to \code{fun}.
#'
#' @returns A a named list of confidence intervals, each containing lower and
#'   upper bounds, with additional attributes storing the observed statistic and
#'   the mean of the bootstrap replicates.
#' @export
#'
#' @examples
#' library(EvaluateCore)
#'
#' pdata <- cassava_CC
#'
#' qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
#'           "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
#'           "PSTR")
#'
#' # Conver '#t qualitative data columns to factor
#' pdata[, qual] <- lapply(pdata[, qual], as.factor)
#'
#' str(pdata)
#'
#' # Bootstrap CIs
#'
#' bootstrap.ci(pdata$NMSR, mean, type = "norm")
#' bootstrap.ci(pdata$NMSR, mean, type = "basic")
#' bootstrap.ci(pdata$NMSR, mean, type = "perc")
#' bootstrap.ci(pdata$NMSR, mean, type = "bca")
#'
#' bootstrap.ci(pdata$LNGS, shannon, type = "norm")
#' bootstrap.ci(pdata$PTLC, simpson, type = "basic")
#' bootstrap.ci(pdata$LFRT, mcintosh_evenness, type = "perc")
#' bootstrap.ci(pdata$LBTEF, mcintosh_diversity, type = "bca")
#'
#' # Studentised intervals require a `fun` returning
#' # variances in addition to an estimate
#'
#' bootstrap.ci(pdata$NMSR, mean, type = "stud")
#'
#' stat_fun_mean <- function(x) {
#'   est <- mean(x)
#'   se  <- sd(x) / sqrt(length(x))
#'   c(est, se)
#' }
#'
#' bootstrap.ci(pdata$NMSR, stat_fun_mean, type = "stud")
#'
#' bootstrap.ci(pdata$DSTA, shannon, type = "stud")
#'
#' stat_fun_shannon <- function(x, base = 2) {
#'   x <- droplevels(x)          # drop unused factor levels
#'   p <- prop.table(table(x))
#'   # Only keep p > 0 to avoid log(0)
#'   p <- p[p > 0]
#'   est <- -sum(p * log(p, base = base))
#'   # Approximate SE using sqrt(Var(p * log(p)))
#'   se <- sqrt(sum((p * log(p, base = base))^2) / length(x))
#'   c(est, se)
#' }
#'
#' bootstrap.ci(pdata$DSTA, stat_fun_shannon, type = "stud")3
#'
bootstrap.ci <- function(x, fun, R = 1000, conf = 0.95,
                         type = c("norm", "basic", "stud", "perc", "bca"),
                         parallel = c("no", "multicore", "snow"),
                         ncpus = getOption("boot.ncpus", 1L),
                         cl = NULL, ...) {

  stopifnot(is.factor(x) || is.numeric(x))

  type <- match.arg(type, several.ok = TRUE)
  parallel <- match.arg(parallel)

  # Bootstrap (single statistic only)
  b <- boot::boot(x,
    statistic = function(data, i) fun(data[i], ...),
    R = R,
    parallel = parallel,
    ncpus = ncpus,
    cl = cl)

  lower.prob <- (1 - conf) / 2
  upper.prob <- 1 - lower.prob

  # Percentile fallback
  perc_fallback <- function() {
    q <- stats::quantile(b$t,
      probs = c(lower.prob, upper.prob),
      names = FALSE,
      type = 6)
    c(lower = q[1], upper = q[2])
  }

  ci_out <- lapply(type, function(tp) {

    # Percentile handled directly
    if (tp == "perc") {
      return(perc_fallback())
    }

    ci <- try(suppressWarnings(
        boot::boot.ci(b, type = tp, conf = conf)), silent = TRUE)

    # Studentized-specific handling
    if (tp == "stud") {
      if (inherits(ci, "try-error") || is.null(ci$stud)) {
        warning(
          "Studentized intervals require bootstrap standard errors; ",
          "falling back to percentile CI.",
          call. = FALSE
        )
        return(perc_fallback())
      }
    }

    # General failure fallback
    if (inherits(ci, "try-error") || is.null(ci[[tp]])) {
      return(perc_fallback())
    }

    out <- ci[[tp]][4:5]
    names(out) <- c("lower", "upper")
    out
  })

  names(ci_out) <- type

  # Attach common summaries
  attr(ci_out, "observed") <- b$t0
  attr(ci_out, "mean") <- mean(b$t)
  attr(ci_out, "R") <- R
  attr(ci_out, "conf") <- conf

  ci_out
}




