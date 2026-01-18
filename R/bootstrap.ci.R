
#' Bootstrap Confidence Intervals
#'
#' This function generates bootstrap resamples using \code{\link[boot]{boot}}
#' and computes confidence intervals using several standard bootstrap methods
#' via \code{\link[boot]{boot.ci}}. The indexing for the statistic function is
#' handled internally.
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
#' @param conf Confidence level of the interval. Default is 0.95.
#' @param type A vector of character strings representing the type of intervals
#'   required. The value should be any subset of the values \code{c("norm",
#'   "basic", "stud", "perc", "bca")} or simply \code{"all"} which will compute
#'   all five types of intervals.
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
#' bootstrap.ci(pdata$DSTA, stat_fun_shannon, type = "stud")
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

  # Clean NA / NaN from bootstrap replicates
  b$t[is.nan(b$t)] <- NA
  if (anyNA(b$t)) {
    warning("Some bootstrap replicates contain NA/NaN values.",
            call. = FALSE)
  }

  # Number of statistics
  p <- if (is.null(dim(b$t))) {
    1L
  } else {
    ncol(b$t)
  }

  lower.prob <- (1 - conf) / 2
  upper.prob <- 1 - lower.prob

  # Percentile CI (vector-safe)
  perc_ci <- function() {
    if (p == 1L) {
      q <- quantile(b$t, c(lower.prob, upper.prob),
                    na.rm = TRUE, names = FALSE, type = 6)
      c(lower = q[1], upper = q[2])
    } else {
      apply(b$t, 2, function(col) {
        q <- quantile(col, c(lower.prob, upper.prob),
                      na.rm = TRUE, names = FALSE, type = 6)
        names(q) <- c("lower", "upper")
        q
      })
    }
  }

  # Percentile CI (single value)
  perc_ci_scalar <- function() {
    q <- quantile(b$t[, 1],
                  probs = c(lower.prob, upper.prob),
                  na.rm = TRUE, names = FALSE, type = 6)
    names(q) <- c("lower", "upper")
    q
  }

  ci_out <- lapply(type, function(tp) {

    # Percentile
    if (tp == "perc") {
      return(perc_ci())
    }

    ## Studentized CI ----
    if (tp == "stud") {

      # Require estimate + SE
      # !is.matrix(b$t) || ncol(b$t) != 2
      if (!(is.matrix(b$t) && ncol(b$t) == 2)) {
        warning("Studentized CI requires fun() to return c(estimate, SE); ",
                "falling back to percentile CI.",
                call. = FALSE)
        return(perc_ci_scalar())
      }

      ci <-
        try(suppressWarnings(boot::boot.ci(b, type = "stud",
                                           conf = conf,
                                           index = 1)),
            silent = TRUE)

      if (inherits(ci, "try-error") || is.null(ci$stud)) {
        warning("Studentized CI failed; falling back to percentile CI.",
                call. = FALSE)
        return(perc_ci_scalar())
      }

      out <- ci$stud[4:5]
      names(out) <- c("lower", "upper")
      return(out)
    }

    ## OTHER CI TYPES (norm, basic, bca) -----
    if (p == 1L) {
      ci <- try(suppressWarnings(boot::boot.ci(b, type = tp, conf = conf)),
                silent = TRUE)

      tp2 <- tp
      tp2 <- ifelse(tp2 == "norm", "normal", tp2)

      if (inherits(ci, "try-error") || is.null(ci[[tp2]])) {
        warning(tp, " CI failed; using percentile CI.", call. = FALSE)
        return(perc_ci())
      }

      if (tp == "norm") {
        out <- ci[[tp2]][2:3]
      } else {
        out <- ci[[tp2]][4:5]
      }

      names(out) <- c("lower", "upper")
      return(out)
    }

    # Vector-valued -> index loop
    out <- matrix(NA_real_, nrow = 2, ncol = p,
                  dimnames = list(c("lower", "upper"), NULL))

    perc <- perc_ci()

    for (i in seq_len(p)) {
      ci <-
        try(suppressWarnings(boot::boot.ci(b, type = tp,
                                           conf = conf,
                                           index = i)),
            silent = TRUE)

      tp2 <- tp
      tp2 <- ifelse(tp2 == "norm", "normal", tp2)

      if (!inherits(ci, "try-error") && !is.null(ci[[tp2]])) {
        if (tp == "norm") {
          out[, i] <- ci[[tp2]][2:3]
        } else {
          out[, i] <- ci[[tp2]][4:5]
        }
      } else {
        warning(sprintf("%s CI failed for component %d; using percentile CI.",
                        tp, i),
                call. = FALSE)
        out[, i] <- perc[, i]
      }
    }

    out
  })


  names(ci_out) <- type

  # Attach common summaries
  attr(ci_out, "observed") <- b$t0
  # attr(ci_out, "mean") <- mean(b$t)
  attr(ci_out, "mean") <- if (is.null(dim(b$t))) {
    mean(b$t, na.rm = TRUE)
  } else {
    colMeans(b$t, na.rm = TRUE)
  }
  attr(ci_out, "R") <- R
  attr(ci_out, "conf") <- conf

  ci_out
}
