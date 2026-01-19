
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
#'   intervals required. The options are \code{c("perc", "bca")}.
#' @inheritParams boot::boot
#' @param base The logarithm base to be used for computation of Rényi and
#'   Tsallis entropy. Default is \code{exp(1)}.
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
#' ggplot(hill_profile1_df, aes(x = q, y = mean,
#'                              color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = hill_points1_df, aes(shape = order_label),
#'     size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 18, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'     color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(hill_profile1_df, aes(x = q, y = mean)) +
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
#' ggplot(renyi_profile1_df, aes(x = q, y = mean,
#'                               color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = renyi_points1_df, aes(shape = order_label),
#'              size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 18, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'        color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(renyi_profile1_df, aes(x = q, y = mean)) +
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
#' ggplot(tsallis_profile1_df, aes(x = q, y = mean,
#'                                 color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = tsallis_points1_df, aes(shape = order_label),
#'              size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 18, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'        color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(tsallis_profile1_df, aes(x = q, y = mean)) +
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
#' ggplot(hill_profile2_df, aes(x = q, y = mean,
#'                              color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = hill_points2_df, aes(shape = order_label),
#'              size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 18, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'        color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(hill_profile2_df, aes(x = q, y = mean)) +
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
#' ggplot(renyi_profile2_df, aes(x = q, y = mean,
#'                               color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = renyi_points2_df, aes(shape = order_label),
#'              size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 18, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'        color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(renyi_profile2_df, aes(x = q, y = mean)) +
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
#' ggplot(tsallis_profile2_df, aes(x = q, y = mean,
#'                                 color = group, fill = group)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#'   geom_line(linewidth = 1) +
#'   geom_vline(xintercept = c(0, 1, 2), linetype = "dashed",
#'              color = "grey60") +
#'   geom_point(data = tsallis_points2_df, aes(shape = order_label),
#'              size = 3, stroke = 1, inherit.aes = TRUE) +
#'   scale_shape_manual(values = c(17, 18, 15), name = "Important q")  +
#'   labs(x = "Order (q)", y = "Hill number",
#'        color = "Group", fill = "Group") +
#'   theme_bw()
#'
#' ggplot(tsallis_profile2_df, aes(x = q, y = mean)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
#'   geom_line(color = "black", linewidth = 1) +
#'   facet_wrap(~ group, scales = "free_y") +
#'   labs(x = "Order (q)", y = "Hill number") +
#'   theme_bw()
diversity.profile <- function(x, group, q = seq(0, 3, 0.1),
                              conf = 0.95, R = 1000,
                              parameter = c("hill", "renyi", "tsallis"),
                              ci.type = c("perc", "bca"),
                              parallel = c("no", "multicore", "snow"),
                              ncpus = getOption("boot.ncpus", 1L),
                              cl = NULL, base = exp(1)) {

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
    if (parameter == "hill") {
      b_res <-
        bootstrap.ci(xg, fun = param.fun, R = R,
                     conf = conf, type = ci.type, parallel = parallel,
                     ncpus = ncpus, cl = cl, q = q)
    } else {
      b_res <-
        bootstrap.ci(xg, fun = param.fun, R = R,
                     conf = conf, type = ci.type, parallel = parallel,
                     ncpus = ncpus, cl = cl, q = q, base = base)
    }

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

