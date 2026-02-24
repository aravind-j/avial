
#' Generate Diversity Profiles for Parametric Indices
#'
#' @param x A numeric or factor vector of observations.
#' @param group A factor vector indicating the group of each observation. Must
#'   have the same length as \code{x}.
#' @param q The order of the parametric index.
#' @param na.omit logical. If \code{TRUE}, missing values (\code{NA}) are
#'   ignored and not included as a distinct factor level for computation.
#'   Default is \code{TRUE}.
#' @param R Integer specifying the number of permutations. Default is 1000.
#' @param parameter The parametric index. Options include \code{"hill"},
#'   \code{"renyi"} and \code{"tsallis"}. Default is \code{"hill"}.
#' @param ci.conf Confidence level of the bootstrap interval. Default is 0.95.
#' @param ci.type A vector of character strings representing the type of
#'   intervals required. The options are \code{c("perc", "bca")}.
#' @param seed Integer. Random seed used to ensure reproducibility of
#'   bootstrap. Default is 123.
#' @inheritParams boot::boot
#'
#' @returns A list of data frames with the following columns for each factor
#'   level in \code{group}. \describe{ \item{q}{} \item{observed}{}
#'   \item{mean}{} \item{lower}{} \item{upper}{} }
#' @export
#'
#' @examples
#'
#' library(EvaluateCore)
#' library(dplyr)
#' library(ggplot2)
#'
#' pdata <- cassava_CC
#'
#' qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
#'           "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
#'           "PSTR")
#'
#' # Convert qualitative data columns to factor
#' pdata[, qual] <- lapply(pdata[, qual], as.factor)
#'
#' str(pdata)
#'
#' important_q <- c(0, 1, 2)
#' important_labels <- c("0D", "1D", "2D")
#'
#' # Hill profile - Percentile CIs ----
#'
#' hill_profile1 <-
#'   diversity.profile(x = pdata$CUAL, group = pdata$LNGS,
#'                     parameter = "hill", ci.type = "perc")
#' hill_profile1
#'
#' hill_profile1_df <- dplyr::bind_rows(hill_profile1, .id = "group")
#'
#' hill_points1_df <- hill_profile1_df %>%
#'   filter(q %in% important_q) %>%
#'   mutate(order_label = factor(q, levels = important_q,
#'                               labels = important_labels))
#'
#' ggplot(hill_profile1_df, aes(x = q, y = observed,
#'                              color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = hill_points1_df, aes(shape = order_label),
#'     size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 19, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'     color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(hill_profile1_df, aes(x = q, y = observed)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
#'   geom_line(color = "black", linewidth = 1) +
#'   facet_wrap(~ group, scales = "free_y") +
#'   labs(x = "Order (q)", y = "Hill number") +
#'   theme_bw()
#'
#' # Rényi profile - Percentile CIs ----
#'
#' renyi_profile1 <-
#'   diversity.profile(pdata$CUAL, group = pdata$LNGS,
#'                     parameter = "renyi", ci.type = "perc")
#' renyi_profile1
#'
#' renyi_profile1_df <- dplyr::bind_rows(renyi_profile1, .id = "group")
#'
#' renyi_points1_df <- renyi_profile1_df %>%
#'   filter(q %in% important_q) %>%
#'   mutate(order_label = factor(q, levels = important_q,
#'                               labels = important_labels))
#'
#' ggplot(renyi_profile1_df, aes(x = q, y = observed,
#'                               color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = renyi_points1_df, aes(shape = order_label),
#'              size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 19, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'        color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(renyi_profile1_df, aes(x = q, y = observed)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
#'   geom_line(color = "black", linewidth = 1) +
#'   facet_wrap(~ group, scales = "free_y") +
#'   labs(x = "Order (q)", y = "Hill number") +
#'   theme_bw()
#'
#' # Tsallis profile - Percentile CIs ----
#'
#' tsallis_profile1 <-
#'   diversity.profile(pdata$CUAL, group = pdata$LNGS,
#'                     parameter = "tsallis", ci.type = "perc")
#' tsallis_profile1 <-
#'   diversity.profile(x = pdata$CUAL, group = pdata$LNGS,
#'                     parameter = "hill", ci.type = "perc")
#' tsallis_profile1
#'
#' tsallis_profile1_df <- dplyr::bind_rows(tsallis_profile1, .id = "group")
#'
#' tsallis_points1_df <- tsallis_profile1_df %>%
#'   filter(q %in% important_q) %>%
#'   mutate(order_label = factor(q, levels = important_q,
#'                               labels = important_labels))
#'
#' ggplot(tsallis_profile1_df, aes(x = q, y = observed,
#'                                 color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = tsallis_points1_df, aes(shape = order_label),
#'              size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 19, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'        color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(tsallis_profile1_df, aes(x = q, y = observed)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
#'   geom_line(color = "black", linewidth = 1) +
#'   facet_wrap(~ group, scales = "free_y") +
#'   labs(x = "Order (q)", y = "Hill number") +
#'   theme_bw()
#'
#' # Hill profile - BCa CIs ----
#'
#' hill_profile2 <-
#'   diversity.profile(pdata$CUAL, group = pdata$LNGS,
#'                     parameter = "hill", ci.type = "bca")
#' hill_profile2
#'
#' hill_profile2_df <- dplyr::bind_rows(hill_profile2, .id = "group")
#'
#' hill_points2_df <- hill_profile2_df %>%
#'   filter(q %in% important_q) %>%
#'   mutate(order_label = factor(q, levels = important_q,
#'                               labels = important_labels))
#'
#' ggplot(hill_profile2_df, aes(x = q, y = observed,
#'                              color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = hill_points2_df, aes(shape = order_label),
#'              size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 19, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'        color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(hill_profile2_df, aes(x = q, y = observed)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
#'   geom_line(color = "black", linewidth = 1) +
#'   facet_wrap(~ group, scales = "free_y") +
#'   labs(x = "Order (q)", y = "Hill number") +
#'   theme_bw()
#'
#' # Rényi profile - BCa CIs ----
#'
#' renyi_profile2 <-
#'   diversity.profile(pdata$CUAL, group = pdata$LNGS,
#'                     parameter = "renyi", ci.type = "bca")
#' renyi_profile2
#'
#' renyi_profile2_df <- dplyr::bind_rows(renyi_profile2, .id = "group")
#'
#' renyi_points2_df <- renyi_profile2_df %>%
#'   filter(q %in% important_q) %>%
#'   mutate(order_label = factor(q, levels = important_q,
#'                               labels = important_labels))
#'
#' ggplot(renyi_profile2_df, aes(x = q, y = observed,
#'                               color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = renyi_points2_df, aes(shape = order_label),
#'              size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 19, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'        color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(renyi_profile2_df, aes(x = q, y = observed)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
#'   geom_line(color = "black", linewidth = 1) +
#'   facet_wrap(~ group, scales = "free_y") +
#'   labs(x = "Order (q)", y = "Hill number") +
#'   theme_bw()
#'
#' # Tsallis profile - BCa CIs ----
#'
#' tsallis_profile2 <-
#'   diversity.profile(pdata$CUAL, group = pdata$LNGS,
#'                     parameter = "tsallis", ci.type = "bca")
#' tsallis_profile2
#'
#' tsallis_profile2_df <- dplyr::bind_rows(tsallis_profile2, .id = "group")
#'
#' tsallis_points2_df <- tsallis_profile2_df %>%
#'   filter(q %in% important_q) %>%
#'   mutate(order_label = factor(q, levels = important_q,
#'                               labels = important_labels))
#'
#' ggplot(tsallis_profile2_df, aes(x = q, y = observed,
#'                                 color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = tsallis_points2_df, aes(shape = order_label),
#'              size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 19, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'        color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(tsallis_profile2_df, aes(x = q, y = observed)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
#'   geom_line(color = "black", linewidth = 1) +
#'   facet_wrap(~ group, scales = "free_y") +
#'   labs(x = "Order (q)", y = "Hill number") +
#'   theme_bw()
diversity.profile <- function(x, group, q = seq(0, 3, 0.1),
                              na.omit = TRUE,
                              ci.conf = 0.95, R = 1000,
                              parameter = c("hill", "renyi", "tsallis"),
                              ci.type = c("perc", "bca"),
                              parallel = c("no", "multicore", "snow"),
                              ncpus = getOption("boot.ncpus", 1L),
                              cl = NULL,
                              seed = 123) {

  # Validate input
  stopifnot(is.factor(x))
  stopifnot(is.factor(group))
  parallel <- match.arg(parallel)
  ci.type <- match.arg(ci.type)  # only one CI type
  parameter <- match.arg(parameter)

  if (!na.omit) {
    if (any(is.na(x))) {
      addNA(x)
    } else {
      x
    }
  }

  # Seed for reproducibility
  if (!is.null(seed)) {
    if (parallel == "snow" && !is.null(cl)) {
      parallel::clusterSetRNGStream(cl, iseed = seed)
    } else {
      set.seed(seed)
    }
  }

  x <- droplevels(x)
  group <- droplevels(group)

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
    xg <- droplevels(xg)

    # Compute bootstrap CI
    b_res <- withCallingHandlers(
      bootstrap.ci(xg, fun = param.fun, R = R,
                   conf = ci.conf, type = ci.type,
                   parallel = parallel,
                   ncpus = ncpus, cl = cl, q = q, seed = seed),
      warning = function(w) {
        new_msg <- paste0("[Group: ", g, "] ", conditionMessage(w))
        warning(new_msg, call. = FALSE)
        invokeRestart("muffleWarning")
      }
    )

    # Convert results to data.frame per group
    ci_mat <- b_res[[ci.type]]  # just the selected CI type
    df <- data.frame(q = q,
                     observed = attr(b_res, "observed"),
                     mean = attr(b_res, "mean"),
                     lower = ci_mat["lower", ],
                     upper = ci_mat["upper", ],
                     ci.type = attr(b_res, "fallback")[[ci.type]])
    df$ci.type <- ifelse(df$ci.type, "perc", ci.type)

    results[[g]] <- df
  }

  attr(results, "R") <- R
  attr(results, "conf") <- ci.conf
  attr(results, "parameter") <- parameter
  attr(results, "ci.type") <- ci.type

  return(results)
}


hill_stat_ci <- function(x, q = c(0, 1, 2)) {

  # Convert once
  if (is.factor(x)) {
    x <- as.integer(x)
  }

  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(rep(NA_real_, length(q)))
  }

  k <- max(x)
  n <- length(x)

  counts <- tabulate(x, nbins = k)
  p <- counts / n

  out <- numeric(length(q))

  for (j in seq_along(q)) {
    qq <- q[j]

    if (qq == 0) {
      out[j] <- sum(counts > 0)

    } else if ((abs(qq - 1) < 1e-8)) { # (q == 1) floating-point tolerance.
      out[j] <- exp(-sum(p * log(p, base = exp(1)))) # hill numbers are base invariant

    } else {
      out[j] <- (sum(p^qq))^(1 / (1 - qq))
    }
  }

  out
}

renyi_stat_ci <- function(x, q = c(0, 1, 2)) {

  if (is.factor(x)) {
    x <- as.integer(x)
  }

  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(rep(NA_real_, length(q)))
  }

  k <- max(x)
  n <- length(x)

  counts <- tabulate(x, nbins = k)
  p <- counts / n

  out <- numeric(length(q))

  for (j in seq_along(q)) {
    qq <- q[j]

    if (qq == 0) {
      out[j] <- log(sum(counts > 0))  # log richness

    } else if ((abs(qq - 1) < 1e-8)) { # (q == 1) floating-point tolerance.
      out[j] <- -sum(p * log(p, base = exp(1)))  # Shannon

    } else {
      out[j] <- (1 / (1 - qq)) * log(sum(p^qq))
    }
  }

  out
}

tsallis_stat_ci <- function(x, q = c(0, 1, 2)) {

  if (is.factor(x)) {
    x <- as.integer(x)
  }

  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(rep(NA_real_, length(q)))
  }

  k <- max(x)
  n <- length(x)

  counts <- tabulate(x, nbins = k)
  p <- counts / n

  out <- numeric(length(q))

  for (j in seq_along(q)) {
    qq <- q[j]

    if (qq == 0) {
      out[j] <- sum(counts > 0) - 1  # richness - 1

    } else if ((abs(qq - 1) < 1e-8)) { # (q == 1) floating-point tolerance.
      out[j] <- -sum(p * log(p, base = exp(1)))  # Shannon

    } else {
      out[j] <- (1 / (qq - 1)) * (1 - sum(p^qq))
    }
  }

  out
}

