#' Summary Statistics of Qualitative and Quantitative trait data
#'
#' @param data The data as a data frame object. The data frame should possess
#'   columns specifying the trait and group.
#' @param group Name of column specifying the group as a character string.
#' @param trait Name of column specifying the trait as a character string. The
#'   trait column should be of type \code{"numeric"} for quantitative traits and
#'   of type \code{"factor"} for qualitative traits.
#' @param out.format The output format.
#'
#' @returns A \code{tibble} data.frame with the summary statistics.
#'
#' @importFrom dplyr across all_of everything group_by mutate
#' @importFrom dplyr summarise n_distinct
#' @importFrom tidyr pivot_longer
#' @importFrom moments agostino.test anscombe.test
#' @export
#'
#' @examples
#' library(agridat)
#' library(dplyr)
#'
#' soydata <- australia.soybean
#'
#' soydata$year <- as.factor(soydata$year)
#'
#' quant_traits <- c("yield", "height", "lodging",
#'                   "size", "protein", "oil")
#' set.seed(123)
#' soydata <-
#'   soydata %>%
#'   mutate(
#'     across(
#'       .cols = all_of(quant_traits),
#'       .fns = ~factor(cut(.x, breaks = quantile(.x, na.rm = TRUE),
#'                          include.lowest = TRUE),
#'                      labels = sample(1:9, size = 4)),
#'       .names = "{.col}_score"
#'     )
#'   )
#'
#' qual_traits <- c("yield_score", "height_score", "lodging_score",
#'                  "size_score", "protein_score", "oil_score")
#'
#' summary_quant(data = soydata, trait = quant_traits)
#' summary_qual(data = soydata, trait = qual_traits)
#'
#' summary_quant(data = soydata, group = "loc", trait = quant_traits)
#' summary_qual(data = soydata, group = "loc", trait = qual_traits)
#'
#' summary_quant(data = soydata, group = c("loc", "year"), trait = quant_traits)
#' summary_qual(data = soydata, group = c("loc", "year"), trait = qual_traits)
#'
summary_quant <- function(data, group = NULL, trait,
                          out.format = c("long", "wide"),
                          min.n.skew = 8,
                          min.unique.skew = 8,
                          min.n.kurt = 8,
                          min.unique.kurt = 5) {

  # Check if data.frame
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object.')
  }

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble.\nCoercing to data frame.')
    data <- as.data.frame(data)
  }

  # check if trait columns present in data
  if (FALSE %in% (trait %in% colnames(data))) {
    stop(paste('The following column(s) specified as trait columns are not present in "data":\n',
               paste(trait[!(trait %in% colnames(data))], collapse = ", "),
               sep = ""))
  }

  # check if trait columns are of type numeric/integer
  inttrtcols <- unlist(lapply(data[, trait],
                              function(x) FALSE %in% (is.vector(x, mode = "integer") | is.vector(x, mode = "numeric"))))
  if (TRUE %in% inttrtcols) {
    stop(paste('The following trait column(s) in "data" are not of type numeric:\n',
               paste(names(inttrtcols[inttrtcols]), collapse = ", ")))
  }

  if (!is.null(group)) {
    # check if group columns present in data
    if (FALSE %in% (group %in% colnames(data))) {
      stop(paste('The following column(s) specified as group columns are not present in "data":\n',
                 paste(group[!(group %in% colnames(data))], collapse = ", "),
                 sep = ""))
    }

    # check if group columns are of type factor
    fctgpcols <- unlist(lapply(data[, group],
                               function(x) FALSE %in% is.factor(x)))
    if (TRUE %in% fctgpcols) {
      stop(paste('The following group column(s) in "data" are not of type factor:\n',
                 paste(names(fctgpcols[fctgpcols]), collapse = ", ")))
    }
  }

  # Check thresholds
  thresholds <- c(
    min.n.skew, min.unique.skew,
    min.n.kurt, min.unique.kurt
  )

  if (any(!is.finite(thresholds)) ||
      any(thresholds < 1) ||
      any(thresholds != floor(thresholds))) {
    stop('All test thresholds must be positive integers.')
  }

  # Check output format argument
  out.format = match.arg(out.format)

  # Convert to long form
  long_data <-
    tidyr::pivot_longer(data = data[, c(group, trait)],
                        cols = dplyr::all_of(trait),
                        names_to = "Trait",
                        values_to = ".quant_value")

  group_vars <- c(group, "Trait")

  grouped_data <-
    dplyr::group_by(long_data,
                    dplyr::across(dplyr::all_of(group_vars)))

  out_long <-

    summarise(.data = grouped_data,,
              count = sum(is.valid(.quant_value)),
              invalid.count = sum(!is.valid(.quant_value)),
              mean = {
                x <- .quant_value[is.valid(.quant_value)]
                if (length(x) == 0L) {
                  NA_real_
                } else {
                  mean(x)
                }
              },
              min = {
                x <- .quant_value[is.valid(.quant_value)]
                if (length(x) == 0L) {
                  NA_real_
                } else {
                  min(x)
                }
              },
              max = {
                x <- .quant_value[is.valid(.quant_value)]
                if (length(x) == 0L) {
                  NA_real_
                } else {
                  max(x)
                }
              },
              sd = {
                x <- .quant_value[is.valid(.quant_value)]
                if (length(x) < 2L) {
                  NA_real_
                } else {
                  sd(x)
                }
              },
              se = {
                x <- .quant_value[is.valid(.quant_value)]
                if (length(x) < 2L) {
                  NA_real_
                } else {
                  sd(x, na.rm = TRUE) / sqrt(length(x))
                }
              },
              summary.messages = {
                n <- sum(is.valid(.quant_value))
                if (n == 0L) {
                  "Mean, min, max, SD and SE are NA: no valid observations."
                } else if (n == 1L) {
                  "SD and SE are NA: fewer than two valid observations."
                } else {
                  "OK"
                }
              },
              skew = safe_test(x = .quant_value[is.valid(.quant_value)],
                               stat_name = "skewness",
                               min_n = min.n.skew,
                               min_unique = min.unique.skew,
                               alternative = "two.sided"),
              kurt = safe_test(x = .quant_value[is.valid(.quant_value)],
                               stat_name = "kurtosis",
                               min_n = min.n.kurt,
                               min_unique = min.unique.kurt,
                               alternative = "two.sided"),

              .groups = "drop"
    )

  out_long <- tidyr::unnest_wider(out_long,
                                  c(skew, kurt),
                                  names_sep = "_")

  out_long <- dplyr::rename(out_long,
                            skew = skew_stat,
                            skew.pvalue = skew_pvalue,
                            skew.messages = skew_message,
                            kurt = kurt_stat,
                            kurt.pvalue = kurt_pvalue,
                            kurt.messages = kurt_message)

  if (out.format == "long") {
    return(out_long)
  }

  value_cols <- setdiff(names(out_long), c(group, "Trait"))

  out_wide <- tidyr::pivot_wider(out_long,
    id_cols = dplyr::all_of(group),
    names_from = Trait,
    values_from = dplyr::all_of(value_cols),
    names_glue = "{Trait}_#_{.value}")

  out_wide

}


is.valid <- function(x, check.na = TRUE,
                     check.nan = TRUE, check.finite = TRUE) {
  valid <- rep(TRUE, length(x))

  if (check.finite)
    valid <- valid & is.finite(x)

  if (check.na)
    valid <- valid & !is.na(x)

  if (check.nan)
    valid <- valid & !is.nan(x)

  valid
}


safe_test <- function(x, stat_name = c("skewness", "kurtosis"),
                      min_n = 8, min_unique = 3,
                      alternative = c("two.sided", "less", "greater")) {

  stat_name <- match.arg(stat_name)
  alternative <- match.arg(alternative)

  x <- x[is.valid(x)]
  n <- sum(is.valid(x))
  n_unique <- dplyr::n_distinct(x)

  reasons <- character(0)

  if (length(x) == 0L) {
    reasons <- c(reasons, "Skewness is NA: no valid observations.")
  }

  if (n < min_n) {
    reasons <- c(reasons, sprintf(
      "%d finite observations (minimum required is %d)",
      n, min_n
    ))
  }

  if (n_unique < min_unique) {
    reasons <- c(reasons, sprintf(
      "%d distinct finite values (minimum required is %d)",
      n_unique, min_unique
    ))
  }

  if (length(reasons) > 0) {
    return(tibble::tibble(
      stat = NA_real_,
      pvalue = NA_real_,
      message = paste(reasons, collapse = " ")
    ))
  }

 # Compute stats
  stat <-
    switch(stat_name,
           skewness = moments::skewness(x),
           kurtosis = moments::kurtosis(x))

 # Perform tests
  warnings <- character()
  error_message <- NULL

  test_out <- tryCatch(
    withCallingHandlers(
      switch(stat_name,
             skewness = moments::agostino.test(x, alternative = alternative),
             kurtosis = moments::anscombe.test(x, alternative = alternative)),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error_message <<- conditionMessage(e)
      NULL
    }
  )

  # If the test errors, both outputs are NA.
  if (!is.null(error_message)) {
    messages <- c(
      paste0("Test error: ", error_message),
      if (length(warnings)) {
        paste0("Warning: ", unique(warnings))
      }
    )

    return(tibble::tibble(
      stat = stat,
      pvalue = NA_real_,
      message = paste(messages, collapse = " ")
    ))
  }

  # Fetch warnings
  messages <- character()

  if (length(warnings)) {
    messages <- c(
      messages,
      paste0("Warning: ", unique(warnings))
    )
  }

  # Fetch stats
  pvalue <- test_out$p.value

  if (length(pvalue) != 1L ||
      !is.numeric(pvalue) ||
      !is.finite(pvalue) ||
      pvalue < 0 ||
      pvalue > 1) {

    pvalue <- NA_real_

    messages <- c(
      messages,
      "P-value is NA: Failed to compute, non-scalar, non-finite, or outside [0, 1]."
    )
  } else {
    pvalue <- unname(pvalue)
  }

  if (!length(messages)) {
    messages <- NA
  }

  tibble::tibble(stat = stat,
                 pvalue = pvalue,
                 message = paste(messages, collapse = " "))

}

#' @rdname summary_quant
#' @export
summary_qual <- function(data, group = NULL, trait,
                         out.format = c("long", "wide")) {

  # Check if data.frame
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object.')
  }

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble.\nCoercing to data frame.')
    data <- as.data.frame(data)
  }

  # check if trait columns present in data
  if (FALSE %in% (trait %in% colnames(data))) {
    stop(paste('The following column(s) specified as trait columns are not present in "data":\n',
               paste(trait[!(trait %in% colnames(data))], collapse = ", "),
               sep = ""))
  }

  # check if trait columns are of type factor
  fcttrtcols <- unlist(lapply(data[, trait],
                                function(x) FALSE %in% is.factor(x)))
  if (TRUE %in% fcttrtcols) {
    stop(paste('The following trait column(s) in "data" are not of type factor:\n',
               paste(names(fcttrtcols[fcttrtcols]), collapse = ", ")))
  }

  if (!is.null(group)) {
    # check if group columns present in data
    if (FALSE %in% (group %in% colnames(data))) {
      stop(paste('The following column(s) specified as group columns are not present in "data":\n',
                 paste(group[!(group %in% colnames(data))], collapse = ", "),
                 sep = ""))
    }

    # check if group columns are of type factor
    fctgpcols <- unlist(lapply(data[, group],
                                  function(x) FALSE %in% is.factor(x)))
    if (TRUE %in% fctgpcols) {
      stop(paste('The following group column(s) in "data" are not of type factor:\n',
                 paste(names(fctgpcols[fctgpcols]), collapse = ", ")))
    }
  }

  # Check output format argument
  out.format = match.arg(out.format)

  if (!is.null(group)) {
    data = group_by(.data = data, !!!syms(group))
  }

  out_wide <-
    summarise(.data = data,
              across(
                .cols = all_of(trait),
                .fns = list(
                  count = ~sum(!is.na(.x)),
                  miss.count = ~sum(is.na(.x)),
                  freq = ~freq_string(.x)
                ),
                .names = "{col}_#_{fn}"
              )
    )

  cols_raw <- colnames(out_wide)
  if (!is.null(group)) {
    cols_raw <- setdiff(cols_raw, group)
  }

  if (out.format == "long") {
    out_long <-
      pivot_longer(data = out_wide,
                   cols = all_of(cols_raw),
                   names_to = c("Trait", ".value"),
                   names_sep = "_#_")

    return(out_long)
  } else {
    return(out_wide)
  }

}



freq_string <- function(x) {
  freq <- unlist(table(x))
  out <- paste0(names(freq), " (", freq, ")")
  out <- paste(out, collapse = ", ")
  return(out)
}
