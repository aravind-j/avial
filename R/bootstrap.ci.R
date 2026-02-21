
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
#'
#' @importFrom boot boot boot.ci
#' @importFrom stats quantile
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
#' # Bootstrap CIs ----
#'
#' bootstrap.ci(pdata$NMSR, mean, type = "norm")
#' bootstrap.ci(pdata$NMSR, mean, type = "basic")
#' bootstrap.ci(pdata$NMSR, mean, type = "perc")
#' bootstrap.ci(pdata$NMSR, mean, type = "bca")
#'
#' bootstrap.ci(pdata$NMSR, mean,
#'              type = c("norm", "basic", "perc", "bca"))
#'
#' bootstrap.ci(pdata$LNGS, shannon, type = "norm")
#' bootstrap.ci(pdata$PTLC, simpson, type = "basic")
#' bootstrap.ci(pdata$LFRT, mcintosh_evenness, type = "perc")
#' bootstrap.ci(pdata$LBTEF, mcintosh_diversity, type = "bca")
#'
#' bootstrap.ci(pdata$LNGS, shannon,
#'              type = c("norm", "basic", "perc", "bca"), base = 2)
#'
#' # Studentised intervals require a `fun` returning
#' # variances in addition to an estimate
#'
#' bootstrap.ci(pdata$NMSR, mean, type = "stud")
#'
#' stat_fun_mean <- function(x) {
#'   est <- mean(x)
#'   se  <- sd(x) / sqrt(length(x))
#'   out <- c(est, se)
#'   # Important : Tells bootstrap.ci to consider second output as SE
#'   attr(out, "se") <- TRUE
#'   return(out)
#' }
#'
#' bootstrap.ci(pdata$NMSR, stat_fun_mean, type = "stud")
#'
#' bootstrap.ci(pdata$DSTA, shannon, type = "stud")
#'
#' stat_fun_shannon <- function(x, base = 2) {
#'   tab <- tabulate(x)
#'   p <- tab / length(x)
#'   # Only keep p > 0 to avoid log(0)
#'   p <- p[p > 0]
#'   est <- -sum(p * log(p, base = base))
#'   # Approximate SE using sqrt(Var(p * log(p)))
#'   se <- sqrt(sum((p * log(p, base = base))^2) / length(x))
#'   out <- c(est, se)
#'   # Important : Tells bootstrap.ci to consider second output as SE
#'   attr(out, "se") <- TRUE
#'   return(out)
#' }
#'
#' bootstrap.ci(pdata$DSTA, stat_fun_shannon, type = "stud")
#'
bootstrap.ci <- function(x, fun, R = 1000, conf = 0.95,
                         type = c("norm", "basic", "stud", "perc", "bca"),
                         parallel = c("no", "multicore", "snow"),
                         ncpus = getOption("boot.ncpus", 1L),
                         cl = NULL, seed = 123, ...) {

  # Validate input
  stopifnot(is.numeric(x) || is.factor(x))
  type <- match.arg(type, several.ok = TRUE)
  parallel <- match.arg(parallel)

  # Seed for reproducibility
  if (!is.null(seed)) {
    if (parallel == "snow" && !is.null(cl)) {
      parallel::clusterSetRNGStream(cl, iseed = seed)
    } else {
      set.seed(seed)
    }
  }

  # Convert factor to integer safely
  if (is.factor(x)) {
    x <- as.integer(x)
  }

  # Probe statistic once
  # To see if se is available for "stud"
  has_se <- FALSE
  if ("stud" %in% type) {
    test_out <- fun(x, ...)
    has_se   <- isTRUE(attr(test_out, "se"))

    p0 <- length(test_out)

    if (has_se && p0 != 2L) {
      stop('If "se" attribute is TRUE, "fun" must return c(estimate, SE).')
    }
  }

  # Bootstrap
  b <- boot::boot(x,
                  statistic = function(data, i) fun(data[i], ...),
                  R = R,
                  parallel = parallel,
                  ncpus = ncpus,
                  cl = cl)

  # Clean NaNs
  b$t[is.nan(b$t)] <- NA_real_
  tmat <- b$t
  t0 <- b$t0

  # Ensure matrix structure
  if (is.null(dim(tmat))) {
    tmat <- matrix(tmat, ncol = 1)
  }
  p <- ncol(tmat)
  R_eff <- nrow(tmat)

  if ("stud" %in% type && has_se && p != 2L) {
    stop("Bootstrap output dimension mismatch.")
  }

  # Confidence probabilities
  alpha <- (1 - conf) / 2
  probs <- c(alpha, 1 - alpha)

  # Mean / SD of bootstrap replicates
  means <- colMeans(tmat, na.rm = TRUE)
  sds <- apply(tmat, 2, sd, na.rm = TRUE)

  # Helper: drop matrix to vector if scalar
  drop_if_scalar <- function(mat) {
    if (ncol(mat) == 1L) {
      out <- mat[, 1]
      names(out) <- c("lower", "upper")
      return(out)
    }
    mat
  }

  # Initialize fallback tracker
  fallback <- vector("list", length(type))
  names(fallback) <- type

  ci_out <- vector("list", length(type))

  for (k in seq_along(type)) {
    fallback[[k]] <- rep(FALSE, p)

    tp <- type[k]
    out <- matrix(NA_real_, 2, p,
                  dimnames = list(c("lower", "upper"), NULL))

    ## Percentile ----
    if (tp == "perc") {
      qs <- apply(tmat, 2, quantile, probs = probs, na.rm = TRUE,
                  names = FALSE, type = 6)
      out[,] <- qs
      ci_out[[k]] <- drop_if_scalar(out)
      next
    }

    ## Basic ----
    if (tp == "basic") {
      qs <- apply(tmat, 2, quantile, probs = rev(probs), na.rm = TRUE,
                  names = FALSE, type = 6)
      out[,] <- 2 * t0 - qs
      ci_out[[k]] <- drop_if_scalar(out)
      next
    }

    ## Normal ----
    if (tp == "norm") {
      z <- qnorm(1 - alpha)
      out[1, ] <- t0 - z * sds
      out[2, ] <- t0 + z * sds
      ci_out[[k]] <- drop_if_scalar(out)
      next
    }

    ## Studentized ----
    # currently assumes scalar (p=1)
    # Not optimized for vector output form fun
    # so it may not mark fallback per component if fun() returns a vector
    if (tp == "stud") {
      if (!(p == 2L) || !has_se) {
        warning("Studentized CI requires fun() to return c(estimate, SE); using percentile instead.",
                call. = FALSE)
        # fallback to percentile
        qs <- apply(tmat[,1, drop=FALSE], 2, quantile,
                    probs = probs, na.rm = TRUE, type = 6)
        rownames(qs) <- c("lower", "upper")
        ci_out[[k]] <- drop_if_scalar(qs)
        fallback[[k]] <- rep(TRUE, p)
        next
      }

      # Get studentized CI from boot.ci()
      ci_try <- try(suppressWarnings(boot::boot.ci(b, type = "stud",
                                                   conf = conf)),
                    silent = TRUE)

      if (inherits(ci_try, "try-error") || is.null(ci_try$stud)) {
        warning("Studentized CI failed; using percentile.", call. = FALSE)
        qs <- apply(tmat[,1, drop=FALSE], 2, quantile,
                    probs = probs, na.rm = TRUE, type = 6)
        rownames(qs) <- c("lower", "upper")
        ci_out[[k]] <- drop_if_scalar(qs)
        fallback[[k]] <- rep(TRUE, p)
        next
      }

      # Build scalar CI matrix
      out <- matrix(ci_try$stud[4:5], ncol = 1,
                    dimnames = list(c("lower","upper"), NULL))
      ci_out[[k]] <- drop_if_scalar(out)
      next
    }

    ## BCa ----
    if (tp == "bca") {
      # Check for bootstrap variation per component
      zero_var <- apply(tmat, 2, function(v) length(unique(v)) == 1)
      if (all(zero_var)) {
        warning("No bootstrap variation; BCa undefined. Using percentile.",
                call. = FALSE)
        qs <- apply(tmat, 2, quantile, probs = probs, na.rm = TRUE,
                    names = FALSE, type = 6)
        out[,] <- qs
        ci_out[[k]] <- drop_if_scalar(out)
        fallback[[k]] <- rep(TRUE, p)  # mark all components as fallback
        next
      }

      # Bias correction
      z0 <- qnorm(colMeans(tmat < matrix(t0, R_eff, p, byrow = TRUE),
                           na.rm = TRUE))

      # Jackknife acceleration
      jack <- try({
        pdf(NULL)                        # suppress plotting
        jvals <- boot::jack.after.boot(b, useJ = TRUE)$jack.values
        dev.off()
        jvals
      }, silent = TRUE)

      if (inherits(jack, "try-error") || is.null(jack)) {
        warning("Jackknife not available; BCa cannot be computed. Using percentile CI.",
                call. = FALSE)
        qs <- apply(tmat, 2, quantile, probs = probs, na.rm = TRUE,
                    names = FALSE, type = 6)
        out[,] <- qs
        ci_out[[k]] <- drop_if_scalar(out)
        fallback[[k]][!zero_var] <- TRUE  # mark only components with nonzero variation as fallback
        next
      }

      if (is.null(dim(jack))) jack <- matrix(jack, ncol = 1)

      acc <- apply(jack, 2, function(u) {
        ubar <- mean(u)
        num <- sum((ubar - u)^3)
        den <- 6 * (sum((ubar - u)^2))^(3/2)
        if (den == 0) 0 else num / den
      })

      z_alpha <- qnorm(probs)
      for (j in seq_len(p)) {
        if (zero_var[j]) {
          # fallback for components with no variation
          out[, j] <- quantile(tmat[, j], probs = probs, na.rm = TRUE,
                               names = FALSE, type = 6)
          fallback[[k]][j] <- TRUE
          next
        }

        # BCa adjustment
        adj <- z0[j] + (z0[j] + z_alpha) / (1 - acc[j] * (z0[j] + z_alpha))
        adj_probs <- pnorm(adj)
        out[, j] <- quantile(tmat[, j], probs = adj_probs,
                             na.rm = TRUE, names = FALSE, type = 6)
      }

      ci_out[[k]] <- drop_if_scalar(out)
    }
  }

  names(ci_out) <- type
  attr(ci_out, "observed") <- t0
  attr(ci_out, "mean") <- means
  attr(ci_out, "fallback") <- fallback
  attr(ci_out, "R") <- R
  attr(ci_out, "conf") <- conf

  ci_out
}

