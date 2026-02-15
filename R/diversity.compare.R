
#' Compare Diversity Measures
#'
#' @inheritParams diversity.calc
#' @inheritParams permutation_tests
#' @inheritParams diversity.profile
#' @param global.test logical. If \code{TRUE} performs the global permutation
#'   tests for the diversity measures. Default is \code{TRUE}.
#' @param pairwise.test logical. If \code{TRUE} performs the pairwise
#'   permutation tests for the diversity measures. Default is \code{TRUE}.
#' @param bootstrap.ci logical. If \code{TRUE} computes the bootstrap confidence
#'   intervals for the diversity measures. Default is \code{TRUE}.
#' @param diversity.profile logical. If \code{TRUE} diversity profiles. Default
#'   is \code{TRUE}.
#' @returns A list with the following elements. \describe{ \item{Diversity
#'   Indices}{A data frame of the different diversity indices computed for each
#'   group.}
#'   \item{Global Test}{A data frame of results of global permutation test
#'   including the test statistic (weighted sum of squares between group summary
#' indices) and the p value for the different diversity indices.}
#'   \item{Pairwise Test}{A list of the following data frames. \describe{
#'   \item{p-value}{A data frame of p values for each between
#'   group comparison for different diversity measures.}
#'   \item{cld}{A data frame of compact letter displays of significant
#'   differences among groups for different diversity measures.} } }
#'   \item{Bootstrap CIs}{A data frame of lower and upper bootstrap confidence
#'   intervals computed for each group in different diversity measures.}
#'   \item{Diversity profiles}{A list of data frames of Hill, Renyi and Tsallis
#'   diversity profiles computed for each group.} }
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
                              global.test = TRUE,
                              pairwise.test = TRUE,
                              bootstrap.ci = TRUE,
                              diversity.profile = TRUE,
                              p.adjust.method = c("bonferroni", "holm"),
                              ci.conf = 0.95,
                              ci.type = c("perc", "bca"),
                              q = seq(0, 3, 0.1),
                              parallel = c("no", "multicore", "snow"),
                              ncpus = 1L,
                              cl = NULL) {

  if (pairwise.test) {
    p.adjust.method <- match.arg(p.adjust.method)
  }
  if (bootstrap.ci) {
    ci.type <- match.arg(ci.type)
  }
  if (pairwise.test | bootstrap.ci | diversity.profile) {
    parallel <- match.arg(parallel)
  }

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

  if (global.test | pairwise.test | bootstrap.ci) {
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
         smith_wilson = list(fun = smith_wilson, args = list(warn = FALSE)),
         brillouin_index = list(fun = brillouin_index, args = list()))
  }

  # Global permutation tests ----
  if (global.test) {

    message("Performing global permutation tests.")

    global_perm_results <-
      lapply(fun_list, function(z) {
        tryCatch(
          {
        do.call(perm.test.global,
                list(x = x, group = group, R = R,
                     fun = z$fun,
                     fun.args = z$args))
          },
        error = function(e) {
          return(list(
            test_stat = NA_real_,
            observed_values = NA,
            p_value = NA_real_,
            error = conditionMessage(e)
          ))
        })
      })

    isgpermerror <- sapply(global_perm_results, length) == 4
    if (any(isgpermerror)) {
      global_perm_msgs <-
        sapply(global_perm_results[which(isgpermerror)], function(x) {
        x[[4]]
      })
    } else {
      global_perm_msgs <- NULL
    }

    message(paste(names(global_perm_msgs), global_perm_msgs,
                  sep = ":\n", collapse = "\n"))

    global_perm_results <-
      lapply(global_perm_results, function(x) {
        c(x[[1]], x[[3]])
      })
    global_perm_results <- data.frame(global_perm_results)
    global_perm_results <- cbind(Measure = c("Test statistic", "p-value"),
                                 global_perm_results)

    attr(global_perm_results, "messages") <- global_perm_msgs

  }

  # Pairwise permutation tests ----
  if (pairwise.test) {

    if (length(groups) > 2) {
      message("Performing pairwise permutation tests.")

      warnings_list <- vector("list", length(fun_list))
      names(warnings_list) <- names(fun_list)

      pairwise_perm_results <-
        lapply(seq_along(fun_list), function(i) {

          z <- fun_list[[i]]

          warnings_log <- character()

          # Run pairwise permutation test
          res <- tryCatch(
            withCallingHandlers(
              {
                do.call(
                  perm.test.pairwise,
                  c(list(x = x, group = group, R = R,
                         fun = z$fun, fun.args = z$args),
                    list(p.adjust.method = p.adjust.method),
                    list(parallel = parallel,
                         ncpus = ncpus,
                         cl = cl))
                )
              },
              warning = function(w) {
                warnings_log <<- c(warnings_log, conditionMessage(w))
                invokeRestart("muffleWarning")
              }
            ),
            error = function(e) {
              message("Error: ", conditionMessage(e))
              return(NULL)
            }
          )
          warnings_list[[i]] <<- warnings_log

          return(res)

        })

      names(pairwise_perm_results) <- names(fun_list)

      pairwise_perm_cld <- NA

      pairwise_perm_cld <-
        lapply(pairwise_perm_results, function(x) {

          adj_p <- setNames(x$adj.p.value,
                            gsub(" vs ", "-", x$Comparison))

          # Assume no significant difference (p = 1) for if p-value is NA
          # Ensures the groups share a letter and not crash
          if (any(is.na(adj_p))) {
            adj_p[is.na(adj_p)] <- 1
          }

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
  }

  # Bootstrap CIs ----
  if (bootstrap.ci) {

    message('Computing bootstrap confidence intervals.')
    bootstrap_ci_results <-
      lapply(groups, function(g) {

        lapply(fun_list, function(z) {
          do.call(avial::bootstrap.ci,
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
  }

  # Diversity Profiles ----
  if (diversity.profile) {

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

  }


  out <- list(`Diversity Indices` = div_indices,
              `Global Test` = if (global.test) {
                global_perm_results
              } else {
                NULL
              },
              `Pairwise Test` = if (pairwise.test) {
                list(`p-value` = pairwise_perm_results,
                     `cld` = pairwise_perm_cld)
              } else {
                NULL
              },
              `Bootstrap CIs` = if (bootstrap.ci) {
                bootstrap_ci_results
              } else {
                NULL
              },
              `Diversity profiles` = if (diversity.profile) {
                diversity_profile_results
              } else {
                NULL
              })

  return(out)

}





