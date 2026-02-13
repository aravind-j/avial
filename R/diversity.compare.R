
#' Compare Diversity Measures
#'
#' @inheritParams diversity.calc
#' @inheritParams permutation_tests
#' @inheritParams diversity.profile
#'
#' @returns
#'
#' @importFrom multcompView multcompLetters
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr bind_rows
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
#' # Convert qualitative data columns to factor
#' pdata[, qual] <- lapply(pdata[, qual], as.factor)
#'
#' str(pdata)
#'
#' diversity.compare(x = pdata$CUAL, group = pdata$LNGS, R = 100,
#'                   base = exp(1), na.omit = TRUE)
#'
#' diversity.compare(x = pdata$ANGB, group = pdata$LNGS, R = 100,
#'                   base = exp(1), na.omit = TRUE)
#'
diversity.compare <- function(x, group, R = 1000, base = exp(1),
                              na.omit = TRUE,
                              p.adjust.method = c("bonferroni", "holm"),
                              ci.conf = 0.95,
                              ci.type = c("perc", "bca"),
                              q = seq(0, 3, 0.1),
                              parallel = c("no", "multicore", "snow"),
                              ncpus = 1L,
                              cl = NULL) {

  p.adjust.method <- match.arg(p.adjust.method)
  ci.type <- match.arg(ci.type)
  parallel <- match.arg(parallel)

  stopifnot(length(x) == length(group))
  stopifnot(is.factor(x))
  stopifnot(is.factor(group))
  stopifnot(nlevels(group) >= 2)

  x <- droplevels(x)
  group <- droplevels(group)

  groups <- levels(group)

  # Diversity indices -----
  message("Computing diversity indices.")

  div_indices_overall <- diversity.calc(x, base = base, na.omit = na.omit)
  div_indices_overall <- c(group = "Overall", div_indices_overall)

  div_indices_gwise <-
    lapply(groups, function(g) {
      diversity.calc(x[group == g], base = base, na.omit = na.omit)
    })
  names(div_indices_gwise) <- groups

  div_indices <-
    dplyr::bind_rows(c(Overall = list(div_indices_overall),
                       div_indices_gwise), .id = "group")

  # Global permutation tests ----
  message("Performing global permutation tests.")

  fun_list <-
    list(margalef_index = list(fun = margalef_index, args = list()),
         menhinick_index = list(fun = menhinick_index, args = list()),
         berger_parker = list(fun = berger_parker, args = list()),
         berger_parker_reciprocal = list(fun = berger_parker_reciprocal,
                                         args = list()),
         simpson = list(fun = simpson, args = list()),
         gini_simpson = list(fun = gini_simpson, args = list()),
         simpson_max = list(fun = simpson_max, args = list()),
         simpson_relative = list(fun = simpson_relative, args = list()),
         shannon = list(fun = shannon, args = list(base = 2)),
         shannon_max = list(fun = shannon_max, args = list(base = 2)),
         shannon_relative = list(fun = shannon_relative,
                                 args = list(base = 2)),
         shannon_ens = list(fun = shannon_ens, args = list(base = 2)),
         heip_evenness = list(fun = heip_evenness, args = list()),
         mcintosh_diversity = list(fun = mcintosh_diversity, args = list()),
         mcintosh_evenness = list(fun = mcintosh_evenness, args = list()),
         smith_wilson = list(fun = smith_wilson, args = list()),
         brillouin_index = list(fun = brillouin_index, args = list()))

  global_perm_results <-
    lapply(fun_list, function(z) {
      do.call(perm.test.global,
              c(list(x = x, group = group, R = R,
                     fun = z$fun),
                z$args))
    })
  global_perm_results <-
    lapply(global_perm_results, function(x) {
      c(x[[1]], x[[3]])
    })
  global_perm_results <- data.frame(global_perm_results)
  global_perm_results <- cbind(Measure = c("Test statistic", "p-value"),
                               global_perm_results)

  # Pairwise permutation tests ----
  if (length(groups) > 2) {
    message("Performing pairwise permutation tests.")

    pairwise_perm_results <-
      lapply(fun_list, function(z) {
        do.call(perm.test.pairwise,
                c(list(x = x, group = group, R = R,
                       fun = z$fun),
                  z$args, list(p.adjust.method = p.adjust.method),
                  list(parallel = parallel,
                       ncpus = ncpus,
                       cl = cl)))
      })

    pairwise_perm_cld <-
      lapply(pairwise_perm_results, function(x) {

        adj_p <- setNames(x$adj.p.value,
                          gsub(" vs ", "-", x$Comparison))
        multcompLetters(adj_p)$Letters
      })
    pairwise_perm_cld <- data.frame(pairwise_perm_cld)
    pairwise_perm_cld <- cbind(Group = rownames(pairwise_perm_cld),
                               pairwise_perm_cld)
    rownames(pairwise_perm_cld) <- NULL

    pairwise_perm_results <-
      lapply(pairwise_perm_results, function(x) {
        setNames(x$adj.p.value, x$Comparison)
      })
    pairwise_perm_results <- data.frame(pairwise_perm_results)
    pairwise_perm_results <-
      cbind(Comparison = rownames(pairwise_perm_results),
            pairwise_perm_results)
    rownames(pairwise_perm_results) <- NULL

  } else {
    message('Skipping pairwise permutation tests as "nlevels(group) = 2".')
  }

  # Bootstrap CIs ----
  message('Computing bootstrap confidence intervals.')
  bootstrap_ci_results <-
    lapply(groups, function(g) {

      lapply(fun_list, function(z) {
        do.call(bootstrap.ci,
                c(list(x = x[group == g], R = R,
                       conf = ci.conf, type = ci.type,
                       fun = z$fun),
                  z$args,
                  list(parallel = parallel,
                       ncpus = ncpus,
                       cl = cl)))
      })

    })
  names(bootstrap_ci_results) <- groups
  bootstrap_ci_results <-
    lapply(bootstrap_ci_results, function(g_boot) {
      data.frame(lapply(g_boot, function(x) {
        x[[ci.type]]
      }))
    })
  bootstrap_ci_results <- dplyr::bind_rows(bootstrap_ci_results,
                                           .id = "Group-CI")
  bootstrap_ci_results$`Group-CI` <-
    paste(bootstrap_ci_results$`Group-CI`,
          gsub("[\\.0-9]+", "", rownames(bootstrap_ci_results)), sep = ": ")
  rownames(bootstrap_ci_results) <- NULL


  # Diversity Profiles ----
  message('Generating diversity profiles.')
  diversity_profile_results <- list(
    hill = diversity.profile(x = x, group = group, q = q,
                             ci.conf = ci.conf,
                             R = R, parameter = "hill",
                             ci.type = ci.type,
                             parallel = parallel,
                             ncpus = ncpus,
                             cl = cl),
    renyi = diversity.profile(x = x, group = group, q = q,
                              ci.conf = ci.conf,
                              R = R, parameter = "renyi",
                              ci.type = ci.type,
                              parallel = parallel,
                              ncpus = ncpus,
                              cl = cl),
    tsallis = diversity.profile(x = x, group = group, q = q,
                                ci.conf = ci.conf,
                                R = R, parameter = "tsallis",
                                ci.type = ci.type,
                                parallel = parallel,
                                ncpus = ncpus,
                                cl = cl)
  )

  out <- list(`Diversity Indices` = div_indices,
              `Global Test` = global_perm_results,
              `Pairwise Test` = list(`p-value` = pairwise_perm_results,
                                     `cld` = pairwise_perm_cld),
              `Bootstrap CIs` = bootstrap_ci_results,
              `Diversity profiles` = diversity_profile_results)

  return(out)

}





