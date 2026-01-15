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
#' @importFrom dplyr across all_of everything group_by mutate summarise
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
                  mean = ~mean(.x, na.rm = TRUE),
                  min = ~min(.x, na.rm = TRUE),
                  max = ~max(.x, na.rm = TRUE),
                  sd = ~sd(.x, na.rm = TRUE),
                  se = ~sd(.x, na.rm = TRUE) /
                    sqrt(length(.x[!is.na(.x)])),
                  skew = ~ifelse(n_distinct(.x) == 1, NA,
                                 moments::agostino.test(.x,
                                                        alternative = "two.sided")$statistic["skew"]),
                  skew.pvalue = ~ifelse(n_distinct(.x) == 1, NA,
                                        moments::agostino.test(.x,
                                                               alternative = "two.sided")$p.value),
                  kurt = ~ifelse(n_distinct(.x) == 1, NA,
                                 moments::anscombe.test(.x,
                                                        alternative = "two.sided")$statistic["kurt"]),
                  kurt.pvalue = ~ifelse(n_distinct(.x) == 1, NA,
                                        moments::anscombe.test(.x,
                                                               alternative = "two.sided")$p.value)
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
