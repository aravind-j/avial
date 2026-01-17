
#' Permutation tests
#'
#' These functions perform permutation-based hypothesis tests to compare groups
#' with respect to a summary statistic (e.g., mean, diversity index).
#'
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
#' @param p.adjust.method (perm.test.pairwise only) Method for adjusting
#'   p-values for multiple comparisons. Options include \code{"bonferroni"} and
#'   \code{"holm"}. Default is \code{"bonferroni"}.
#' @param ... Additional arguments passed to \code{fun}.
#'
#' @returns
#'
#' @name permutation_tests
#' @rdname permutation_tests
#'
#' @examples
#'
NULL

# Global permutation test ----
#' @rdname permutation_tests
#' @export
perm.test.global <- function(x, group, fun, R = 1000, ...) {

  stopifnot(length(x) == length(group))
  # stopifnot(is.factor(x))
  stopifnot(is.factor(group))

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
  null_stats <- vapply(seq_len(R), function(i) {
    shuffled_group <- sample(group)
    perm_indices <- tapply(x, shuffled_group, fun, ...)

    perm_tab <- table(shuffled_group)
    perm_counts <- as.vector(perm_tab)
    names(perm_counts) <- names(perm_tab)

    # enforce order
    perm_indices <- perm_indices[names(perm_counts)]

    perm_grand_mean <- sum(perm_indices * perm_counts) / sum(perm_counts)

    # var(perm_indices)
    sum(perm_counts * ((perm_indices - perm_grand_mean) ^ 2))
    # sum(perm_indices^2 * perm_counts)
  }, numeric(1))

  # Calculate P-value
  # phipson_permutation_2010
  p_val <- #  mean(null_stats >= test_stat)
    (sum(null_stats >= test_stat) + 1) / (R + 1)

  out <- list(test_stat = test_stat,
              observed_indices = obs_indices,
              p_value = p_val)

  return(out)
}

# Pair-wise permutation test ----
#' @rdname permutation_tests
#' @export
perm.test.pairwise <- function(x, group, fun, R = 1000,
                               p.adjust.method = c("bonferroni", "holm"),
                               ...)  {

  p.adjust.method <- match.arg(p.adjust.method)

  stopifnot(length(x) == length(group))
  # stopifnot(is.factor(x))
  stopifnot(is.factor(group))

  lvls <- levels(group)
  pairs <- combn(lvls, 2, simplify = FALSE)

  pw_results <- do.call(rbind, lapply(pairs, function(p) {
    # Filter for pair
    mask <- group %in% p
    sub_x <- droplevels(x[mask])
    sub_g <- droplevels(group[mask])
    # Pairwise Permutation
    grp_levels <- levels(sub_g)
    obs_vals <- tapply(sub_x, sub_g, fun, ...)
    obs_vals <- obs_vals[grp_levels]
    obs_diff <- abs(diff(obs_vals[grp_levels]))  # two-tailed

    null_diffs <- vapply(seq_len(R), function(i) {
      perm_g <- sample(sub_g)
      perm_vals <- tapply(sub_x, perm_g, fun, ...)
      perm_vals <- perm_vals[grp_levels]
      abs(diff(perm_vals[grp_levels]))
    }, numeric(1))

    # phipson_permutation_2010
    p_val <- # mean(null_diffs >= obs_diff)
      (sum(null_diffs >= obs_diff) + 1) / (R + 1)

    data.frame(Comparison = paste(p[1], "vs", p[2]), p.value = p_val)
  }))

  # P-value correction
  # pw_results$adj.p.value <- pmin(pw_results$p.value * nrow(pw_results), 1)
  pw_results$adj.p.value <- p.adjust(pw_results$p.value,
                                     method = p.adjust.method)

  return(pw_results)
}
