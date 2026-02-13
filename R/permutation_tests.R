
#' Permutation Tests
#'
#' These functions perform permutation-based hypothesis tests to compare groups
#' with respect to a summary statistic (e.g., mean, diversity index).
#' \itemize{ \item{\code{perm.test.global} performs a global test across all
#' groups simultaneously, using a weighted sum of squares between group summary
#' indices as the test statistic} \item{\code{perm.test.pairwise} performs
#' pairwise tests between all combinations of groups, comparing the absolute
#' difference in summary statistics and optionally adjusting p-values for
#' multiple comparisons.} }
#'
#' @param x A numeric or factor vector of observations.
#' @param group A factor vector indicating the group of each observation. Must
#'   have the same length as \code{x}.
#' @param fun A function to summarize values within each group.
#' @param R Integer specifying the number of permutations. Default is 1000.
#' @param max_invalid Numeric between 0 and 1. Maximum allowed proportion of
#'   invalid permutations (i.e., permutations for which the test statistic is
#'   non-finite). If the proportion of invalid permutations exceeds this
#'   threshold, the function stops execution with an error, indicating that the
#'   statistic function is not permutation-stable.
#' @param p.adjust.method (perm.test.pairwise only) Method for adjusting
#'   p-values for multiple comparisons. Options include \code{"bonferroni"} and
#'   \code{"holm"}. Default is \code{"bonferroni"}.
#' @inheritParams boot::boot
#' @param ... Additional arguments passed to \code{fun}.
#'
#' @returns \describe{ \item{\code{perm.test.global}}{A list of the following
#'   elements. \describe{ \item{test_stat}{The test statistic value.}
#'   \item{observed_values}{The observed values for each group.}
#'   \item{p_value}{The p value.} } } \item{\code{perm.test.pairwise}}{A data
#'   frame of the following columns. \describe{
#'   \item{Comparison}{The comparison.} \item{p.value}{The p value.}
#'   \item{adj.p.value}{The adjusted p value.} } } }
#'
#' @name permutation_tests
#' @rdname permutation_tests
#'
#' @importFrom stats p.adjust
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
#' # Global tests ----
#'
#' perm.test.global(x = pdata$NMSR, group = pdata$CUAL, fun = mean,
#'                  R = 100)
#'
#' perm.test.global(x = pdata$LNGS, group = pdata$CUAL, fun = shannon,
#'                  R = 100)
#'
#' perm.test.global(x = pdata$PTLC, group = pdata$CUAL, fun = simpson,
#'                  R = 100)
#'
#' # Pairwise tests ----
#'
#' perm.test.pairwise(x = pdata$NMSR, group = pdata$CUAL, fun = mean,
#'                    R = 100)
#'
#' perm.test.pairwise(x = pdata$LNGS, group = pdata$CUAL, fun = shannon,
#'                    R = 100)
#'
#' perm.test.pairwise(x = pdata$PTLC, group = pdata$CUAL, fun = simpson,
#'                    R = 100)
#'
NULL

# Global permutation test ----
#' @rdname permutation_tests
#' @export
perm.test.global <- function(x, group, fun, R = 1000,
                             max.invalid = 0.25,
                             parallel = c("no", "multicore", "snow"),
                             ncpus = 1L,
                             cl = NULL,
                             ...) {

  parallel <- match.arg(parallel)

  stopifnot(length(x) == length(group))
  stopifnot(is.factor(x) || is.numeric(x))
  stopifnot(is.factor(group))

  if (!is.numeric(max.invalid) || length(max.invalid) != 1L ||
      max.invalid <= 0 || max.invalid >= 1) {
    stop("'max.invalid' must be a single number between 0 and 1.", call. = FALSE)
  }

  group <- droplevels(group)

  # group-wise observed indices
  obs_indices <- tapply(x, group, fun, ...)

  # counts
  tab <- table(group)
  counts <- as.vector(tab)
  names(counts) <- names(tab)

  # enforce order
  obs_indices <- obs_indices[names(counts)]

  # grand mean
  grand_mean <- sum(obs_indices * counts) / sum(counts)

  # Test statistic

  ##[1] variance between indices
  # test_stat <- var(obs_indices)
  #[2] Weighted and centered SS between groups
  test_stat <- sum(counts * ((obs_indices - grand_mean) ^ 2))
  ##[3] Weighted and uncentered SS between groups
  # test_stat <-  sum(obs_indices^2 * counts)

  # permutation loop to get null variance
  null_stats <- numeric(R)
  n_valid <- 0L

  n_invalid_total <- 0
  n_generated_total <- 0

  while (n_valid < R) {

    batch_size <- min(R - n_valid, R)

    # permutation loop to get null variance
    batch <- apply_parallel(seq_len(batch_size), function(i) {
      shuffled_group <- sample(group)
      perm_indices <- tapply(x, shuffled_group, fun, ...)

      perm_tab <- table(shuffled_group)
      perm_counts <- as.vector(perm_tab)
      names(perm_counts) <- names(perm_tab)

      # enforce order
      perm_indices <- perm_indices[names(perm_counts)]

      perm_grand_mean <- sum(perm_indices * perm_counts) / sum(perm_counts)

      # stat <- var(perm_indices)
      stat <- sum(perm_counts * ((perm_indices - perm_grand_mean) ^ 2))
      # stat <- sum(perm_indices^2 * perm_counts)

      if (!is.finite(stat)) {
        return(NA_real_)
      } else {
        stat
      }

    }, parallel = parallel, ncpus = ncpus, cl = cl)

    batch <- unlist(batch)

    n_generated_total <- n_generated_total + length(batch)
    n_invalid_total   <- n_invalid_total + sum(!is.finite(batch))

    # EARLY STOP CHECK
    if (n_generated_total > 0) {
      current_prop_invalid <- n_invalid_total / n_generated_total

      if (current_prop_invalid > max.invalid) {
        stop(
          sprintf(
            "Permutation test aborted: statistic function '%s' is not permutation-stable (%.1f%% invalid permutations, threshold = %.0f%%).",
            deparse(substitute(fun)),
            100 * current_prop_invalid,
            100 * max.invalid
          ),
          call. = FALSE
        )
      }
    }

    batch <- batch[is.finite(batch)] # check if valid

    if (length(batch) > 0) {
      idx <- seq.int(from = n_valid + 1L,
                     length.out = length(batch))
      null_stats[idx] <- batch
      n_valid <- n_valid + length(batch)
    }
  }

  # null_stats <- null_stats[seq_len(R)]

  if (n_generated_total > 0) {
    prop_invalid <- n_invalid_total / n_generated_total

    if (prop_invalid > 0.02) {
      warning(
        sprintf("Function '%s' is not permutation-stable (%.1f%% invalid permutations).",
                deparse(substitute(fun)),
                100 * prop_invalid),
        call. = FALSE
      )
    }
  }

  # Calculate P-value
  # phipson_permutation_2010
  p_val <- #  mean(null_stats >= test_stat)
    (sum(null_stats >= test_stat) + 1) / (R + 1)

  out <- list(test_stat = test_stat,
              observed_values = obs_indices,
              p_value = p_val)

  return(out)
}

# Pair-wise permutation test ----
#' @rdname permutation_tests
#' @export
perm.test.pairwise <- function(x, group, fun, R = 1000,
                               p.adjust.method = c("bonferroni", "holm"),
                               max.invalid = 0.25,
                               parallel = c("no", "multicore", "snow"),
                               ncpus = 1L,
                               cl = NULL,
                               ...)  {

  parallel <- match.arg(parallel)

  p.adjust.method <- match.arg(p.adjust.method)

  stopifnot(length(x) == length(group))
  stopifnot(is.factor(x) || is.numeric(x))
  stopifnot(is.factor(group))

  if (!is.numeric(max.invalid) || length(max.invalid) != 1L ||
      max.invalid <= 0 || max.invalid >= 1) {
    stop("'max.invalid' must be a single number between 0 and 1.", call. = FALSE)
  }

  group <- droplevels(group)

  lvls <- levels(group)
  pairs <- combn(lvls, 2, simplify = FALSE)

  if (parallel == "snow") {
    parallel::clusterSetRNGStream(cl, iseed = 123)
  }

  pw_results_list <- vector("list", length(pairs))
  names(pw_results_list) <-
    vapply(pairs, function(p) {
      paste(p[1], "vs", p[2])
    }, character(1))

  for (i in seq_along(pairs)) {
    p <- pairs[[i]]
    # Filter for pair
    mask <- group %in% p
    sub_x <- x[mask]
    sub_g <- droplevels(group[mask])

    # Pairwise Permutation
    grp_levels <- levels(sub_g)
    obs_vals <- tapply(sub_x, sub_g, fun, ...)
    obs_vals <- obs_vals[grp_levels]
    obs_diff <- abs(diff(obs_vals))  # two-tailed

    null_diffs <- numeric(R)
    n_valid <- 0L

    n_invalid_total <- 0
    n_generated_total <- 0

    while (n_valid < R) {

      batch_size <- R - n_valid

      batch <- apply_parallel(seq_len(batch_size), function(i) {

        perm_g <- sample(sub_g)
        perm_vals <- tapply(sub_x, perm_g, fun, ...)
        perm_vals <- perm_vals[grp_levels]

        if (any(!is.finite(perm_vals))) {
          return(NA_real_)
        } else {
          d <- abs(diff(perm_vals))
        }

        if (!is.finite(d)) {
          return(NA_real_)
        } else {
          d
        }

      }, parallel = parallel, ncpus = ncpus, cl = cl)

      batch <- unlist(batch)

      n_generated_total <- n_generated_total + length(batch)
      n_invalid_total   <- n_invalid_total + sum(!is.finite(batch))

      # EARLY STOP CHECK
      if (n_generated_total > 0) {
        current_prop_invalid <- n_invalid_total / n_generated_total

        if (current_prop_invalid > max.invalid) {
          stop(
            sprintf(
              "Permutation test aborted: statistic function '%s' is not permutation-stable for comparison '%s' (%.1f%% invalid permutations, threshold = %.0f%%).",
              deparse(substitute(fun)),
              names(pw_results_list)[i],
              100 * current_prop_invalid,
              100 * max.invalid
            ),
            call. = FALSE
          )
        }
      }

      batch <- batch[is.finite(batch)]

      if (length(batch) > 0) {
        idx <- seq.int(from = n_valid + 1L,
                       length.out = length(batch))
        null_diffs[idx] <- batch
        n_valid <- n_valid + length(batch)
      }

    }

    # null_diffs <- null_diffs[seq_len(R)]

    if (n_generated_total > 0) {
      prop_invalid <- n_invalid_total / n_generated_total

      if (prop_invalid > 0.02) {
        warning(
          sprintf(
            "Function '%s' is not permutation-stable for comparison '%s' (%.1f%% invalid permutations).",
            deparse(substitute(fun)),
            names(pw_results_list)[i],
            100 * prop_invalid
          ),
          call. = FALSE
        )
      }
    }

    # phipson_permutation_2010
    p_val <- # mean(null_diffs >= obs_diff)
      (sum(null_diffs >= obs_diff) + 1) / (R + 1)

    # data.frame(Comparison = paste(p[1], "vs", p[2]), p.value = p_val)
    data.frame(p.value = p_val)
    pw_results_list[[i]] <- data.frame(p.value = p_val)
  }

  pw_results <- dplyr::bind_rows(pw_results_list, .id = "Comparison")

  # P-value correction
  # pw_results$adj.p.value <- pmin(pw_results$p.value * nrow(pw_results), 1)
  pw_results$adj.p.value <- p.adjust(pw_results$p.value,
                                     method = p.adjust.method)

  return(pw_results)
}


# parallel lapply switch function ----
apply_parallel <- function(X, FUN,
                           parallel = c("no", "multicore", "snow"),
                           ncpus = 1L,
                           cl = NULL,
                           ...) {

  parallel <- match.arg(parallel)

  if (parallel == "no") {
    # Sequential
    lapply(X, FUN, ...)

  } else if (parallel == "multicore") {
    # Unix only
    parallel::mclapply(X, FUN, mc.cores = ncpus, ...)

  } else if (parallel == "snow") {
    # Windows & snow clusters
    if (is.null(cl)) stop("Please supply a cluster object 'cl' for snow mode")
    parallel::parLapply(cl, X, FUN, ...)
  }
}

