#' Prepare Input Files for \code{PowerCore}
#'
#' Prepare input files for \code{PowerCore}, a program applying the advanced M
#' strategy with a heuristic search for establishing core sets.
#'
#' @inheritParams prep_mstrat_input
#'
#' @export
#'
#' @examples
#'
#' library(EvaluateCore)
#'
#' data(cassava_EC)
#' data <- cassava_EC
#'
#' quant <- c("NMSR", "TTRN", "TFWSR", "TTSW", "TTPW", "AVPW",
#'            "ARSR", "SRDM")
#' qual <- c("CUAL", "LNGS", "LFRT", "LBTEF", "CBTR", "NMLB",
#'           "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
#'           "PSTR")
#'
#' # Prepare genotype column
#' data$Accession <- rownames(data)
#' rownames(data) <- NULL
#' data$Accession <- as.factor(data$Accession)
#'
#' # Convert qualitative data as factors
#' data[, qual] <- lapply(data[, qual],
#'                        function(x) factor(as.factor(x)))
#'
#' sel <- c("TMe-2906", "TMe-3412", "TMe-1374", "TMe-768", "TMe-14",
#'          "TMe-3284", "TMe-937", "TMe-1859", "TMe-3265", "TMe-1739",
#'          "TMe-972", "TMe-769", "TMe-3243", "TMe-3719", "TMe-1095",
#'          "TMe-893", "TMe-1262", "TMe-2083", "TMe-376", "TMe-3633",
#'          "TMe-1738", "TMe-2428", "TMe-259", "TMe-3457", "TMe-1406",
#'          "TMe-977", "TMe-3006", "TMe-925", "TMe-3671", "TMe-2779",
#'          "TMe-1293", "TMe-268", "TMe-266", "TMe-3562", "TMe-801")
#'
#' prep_powercore_input(data = data, genotype = "Accession",
#'                      qualitative = qual, quantitative = quant,
#'                      center = TRUE, scale = TRUE,
#'                      always.selected = sel,
#'                      file.name = "PowerCore_input",
#'                      folder.path = tempdir())
#'
prep_powercore_input <- function(data, genotype,
                                 qualitative, quantitative,
                                 center = TRUE, scale = TRUE,
                                 always.selected = NULL,
                                 file.name = "PowerCore_input",
                                 folder.path = getwd()) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Checks ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Check if data.frame
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object.')
  }

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble.\nCoercing to data frame.')
    data <- as.data.frame(data)
  }

  if (missing(quantitative)) {
    quantitative <- NULL
  }

  if (missing(qualitative)) {
    qualitative <- NULL
  }

  # check if 'quantitative' columns are present in 'data'
  if (!is.null(quantitative)) {
    if (FALSE %in% (quantitative %in% colnames(data)))  {
      stop('The following column(s) specified in "quantitative" are not ',
           'present in "data":\n',
           paste(quantitative[!(quantitative %in% colnames(data))],
                 collapse = ", "),
           sep = "")
    }
  }

  # check if 'qualitative' columns are present in 'data'
  if (!is.null(qualitative)) {
    if (FALSE %in% (qualitative %in% colnames(data)))  {
      stop('The following column(s) specified in "qualitative" are not ',
           'present in "data":\n',
           paste(qualitative[!(qualitative %in% colnames(data))],
                 collapse = ", "),
           sep = "")
    }
  }

  # check if overlap exists between 'quantitative' and 'qualitative'
  if ((!is.null(quantitative)) & (!is.null(qualitative))) {
    if (length(intersect(quantitative, qualitative)) != 0) {
      stop('The following column(s) is/are specified in both "quantitative" ',
           'and "qualitative":\n',
           paste(intersect(quantitative, qualitative),
                 collapse = ", "),
           sep = "")
    }
  }

  # Check if 'genotype' column present in data
  if (!(genotype %in% colnames(data))) {
    stop(paste('Column ', genotype,
               ' specified as the "genotype" column is not present in "data".',
               sep = ""))
  }

  # check if 'genotype' column is of type factor
  if (!is.factor(data[, genotype])) {
    stop('"genotype" column in "data" should be of type factor.')
  }

  # Check if ~ or % is present in colnames
  col_mrkr_check <- grepl("~|%", c(genotype, quantitative, qualitative))
  names(col_mrkr_check) <- c(genotype, quantitative, qualitative)
  if (any(col_mrkr_check)) {
    stop(' The following column names specified as either "genotype" and/or ',
         '"qualitative" and/or "quantitative" have "~" and "%" characters.\n',
         paste(names(col_mrkr_check[col_mrkr_check]), collapse = ", "))
  }

  # Check if ~ or % is present in genotype
  gen_mrkr_check <- grepl("~|%", data[, genotype])
  if (any(gen_mrkr_check)) {
    stop(' The following genotype names in ', genotype,
         ' column have "~" and "%" characters.\n',
         paste(data[gen_mrkr_check, genotype], collapse = ", "))
  }

  # check if 'quantitative' columns are of type numeric/integer
  if (!is.null(quantitative)) {
    intquantcols <-
      unlist(lapply(data[, quantitative],
                    function(x) FALSE %in% (is.vector(x, mode = "integer") |
                                              is.vector(x, mode = "numeric"))))
    if (TRUE %in% intquantcols) {
      stop('The following "quantitative" column(s) in "data" are not of ',
           'type numeric:\n',
           paste(names(intquantcols[intquantcols]), collapse = ", "))
    }
  }

  # check if 'qualitative' columns are of type factor
  if (!is.null(qualitative)) {
    intqualcols <- unlist(lapply(data[, qualitative],
                                 function(x) is.factor(x)))
    if (FALSE %in% intqualcols) {
      stop('The following "qualitative" column(s) in "data" are not of ',
           'type factor:\n',
           paste(names(intqualcols[!intqualcols]), collapse = ", "))
    }
  }

  traits <- c(qualitative, quantitative)

  # Check always.selected
  if (!is.null(always.selected)) {
    if (FALSE %in% (always.selected %in% data[, genotype]))  {
      stop('The following genotype(s) specified in "always.selected" are ',
           'not present in the "',
           genotype,
           '" column: \n',
           paste(always.selected[!(always.selected %in% data[, genotype])],
                 collapse = ", "),
           sep = "")
    }
  }

  # Check if target file path exists
  if(!file.exists(folder.path)) {
    stop('The path specified as "folder.path" does not exist.')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare data file ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  data_out <- data[, c(genotype, traits)]

  # Scale and center the quantitative data (Z scaling)
  data_out[, quantitative] <-
    apply(data_out[, quantitative], 2,
          function(x) scale(x, center = center, scale = scale))

  # Mark selected accessions
  if (!is.null(always.selected)) {
    data_out[, genotype] <- as.character(data_out[, genotype])

    sel_ind <- data_out[, genotype] %in% always.selected
    data_out[sel_ind, genotype] <- paste("~", data_out[sel_ind, genotype],
                                         sep = "")
  }

  # Mark genotype column
  colnames(data_out)[colnames(data_out) == genotype] <-
    paste("%", genotype, sep = "")

  # Mark quantitiative traits
  colnames(data_out)[colnames(data_out) %in% quantitative] <-
    paste("~",
          colnames(data_out)[colnames(data_out) %in% quantitative],
          sep = "")

  target <- file.path(folder.path,
                      paste(file.name, ".csv", sep = ""))

  write.csv(data_out, file = target, na = "",
            row.names = FALSE)

  message(paste("PowerCore output file created at", target))

}
