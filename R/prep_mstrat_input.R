#' Prepare Input Files for \code{MStrat}
#'
#' Prepare input files for \code{MStrat}, a software for building germplasm core
#' collections by maximizing allelic or phenotypic richness
#' \insertCite{schoen_Conservation_1993a,gouesnard_MSTRAT_2001,gouesnard_MStrat_2002}{avial}.
#'
#' @param data The data as a data frame object. The data frame should possess
#'   columns with the genotype names and multiple quantitative and/or
#'   qualitative trait/variable data.
#' @param genotype Name of column with the genotype names as a character string.
#' @param qualitative Name of columns with the qualitative traits as a character
#'   vector.
#' @param quantitative Name of columns with the quantitative traits as a
#'   character vector.
#' @param active Name of traits/variables to be declared as active.
#' @param target Name of traits/variables to be declared as target.
#' @inheritParams base::scale
#' @param weights.qualitative An vector of weight to be applied on a qualitative
#'   traits. Should be \code{NULL} or a numeric vector of the same length as the
#'   number of qualitative traits. If \code{NULL}, the default weight of 1 is
#'   given.
#' @param weights.quantitative An vector of weight to be applied on a
#'   quantitative traits. Should be \code{NULL} or a numeric vector of the same
#'   length as the number of quantitative traits. If \code{NULL}, the default
#'   weight of 1 is given.
#' @param nclass.quantitative The number of classes into which each quantitative
#'   trait data have to be divided into. Should be \code{NULL} or a integer
#'   vector of the same length as the number of quantitative traits. If
#'   \code{NULL}, the default of 5 is applied. MStat limits the maximum number
#'   of classes to 1000.
#' @param always.selected A character vector with names of individuals in the
#'   \code{genotype} that should always be selected in the core collection. The
#'   maximum length accepted by \code{MStrat} is 500.
#' @param file.name A character string of name of file where the data will be
#'   saved.
#' @param folder.path The path to folder where the input files are to be saved.
#'
#' @importFrom readr format_delim
#' @importFrom dplyr mutate row_number
#' @importFrom utils write.table
#' @importFrom Rdpack reprompt
#' @importFrom stringi stri_replace_all_regex
#' @importFrom utils write.table
#' @export
#'
#' @references
#'
#' \insertAllCited{}
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
#' active = c("LNGS", "LFRT", "LBTEF", "CBTR", "NMLB",
#'            "ANGB", "CUAL9M", "LVC9M", "TNPR9M",
#'            "TTRN", "TFWSR", "TTSW", "TTPW", "AVPW")
#' target = c("NMSR", "TTRN", "ARSR", "SRDM",
#'            "CUAL", "LNGS", "TNPR9M",
#'            "PL9M", "STRP", "STRC",
#'            "PSTR")
#'
#' sel <- c("TMe-2906", "TMe-3412", "TMe-1374", "TMe-768", "TMe-14",
#'          "TMe-3284", "TMe-937", "TMe-1859", "TMe-3265", "TMe-1739",
#'          "TMe-972", "TMe-769", "TMe-3243", "TMe-3719", "TMe-1095",
#'          "TMe-893", "TMe-1262", "TMe-2083", "TMe-376", "TMe-3633",
#'          "TMe-1738", "TMe-2428", "TMe-259", "TMe-3457", "TMe-1406",
#'          "TMe-977", "TMe-3006", "TMe-925", "TMe-3671", "TMe-2779",
#'          "TMe-1293", "TMe-268", "TMe-266", "TMe-3562", "TMe-801")
#'
#' prep_mstrat_input(data = data, genotype = "Accession",
#'                   qualitative = qual, quantitative = quant,
#'                   active = active, target = target,
#'                   center = TRUE, scale = TRUE,
#'                   weights.qualitative = NULL,
#'                   weights.quantitative = NULL,
#'                   nclass.quantitative = NULL, always.selected = sel,
#'                   file.name = "MStrat_input",
#'                   folder.path = tempdir())
#'
prep_mstrat_input <- function(data, genotype,
                              qualitative, quantitative, #inactive,
                              active, target,
                              center = TRUE, scale = TRUE,
                              weights.qualitative = NULL,
                              weights.quantitative = NULL,
                              nclass.quantitative = NULL,
                              always.selected = NULL,
                              file.name = "MStrat_input",
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

  if (nrow(data) < 30) {
    stop('"data" should include atleast 30 accessions/genotypes.')
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

    nclass_check <- unlist(lapply(data[, qualitative],
                                  function(x) {
                                    length(levels(x)) > 1000
                                  }))
    if (TRUE %in% nclass_check) {
      stop('The following "qualitative" column(s) in "data" have more than ',
           '1000 levels/classes:\n',
           paste(names(nclass_check[!nclass_check]), collapse = ", "))
    }
    rm(nclass_check)
  }

  traits <- c(qualitative, quantitative)

  # check if number of variables is >500
  if (ncol(data) >500) {
    stop('The number of variables exceeds 500.')
  }

  # check if 'active' traits/markers are present in 'traits'
  if (FALSE %in% (active %in% traits))  {
    stop(paste('The following column(s) specified in "active" not present in',
               ' "qualitative" or "quantitative":\n',
               paste(active[!(active %in% traits)],
                     collapse = ", "),
               sep = ""))
  }

  # check if 'target' traits/markers are present in 'traits'
  if (FALSE %in% (target %in% traits))  {
    stop(paste('The following column(s) specified in "target" not present in',
               '"qualitative" or "quantitative":\n',
               paste(target[!(target %in% traits)],
                     collapse = ", "),
               sep = ""))
  }

  # Check qualitative weights
  if (!is.null(weights.qualitative)) {
    if (!is.numeric(weights.qualitative) ||
        length(weights.qualitative) != length(qualitative)) {
      stop('"weights.qualitative" should be a numeric vector with same length',
           'as "qualitative".')
    }
  }

  # Check quantitative weights
  if (!is.null(weights.quantitative)) {
    if (!is.numeric(weights.quantitative) ||
        length(weights.quantitative) != length(quantitative)) {
      stop('"weights.quantitative" should be a numeric vector with same length',
           'as "quantitative".')
    }
  }

  # Check number of classes for quantitative
  if (!is.null(nclass.quantitative)) {
    if (!is.integer(nclass.quantitative) ||
        length(nclass.quantitative) != length(quantitative)) {
      stop('"nclass.quantitative" should be an integer vector with same length',
           'as "quantitative".')
    }

    nclass_check <- unlist(lapply(nclass.quantitative,
                                  function(x) {
                                    x < 2 | x > 1000
                                  }))
    names(nclass_check) <- quantitative
    if (TRUE %in% nclass_check) {
      stop('The number of levels/classes specified for the following ',
           '"quantitative" column(s) in "data" with the "nclass.quantitative" ',
           'argument are < 2 or > 1000:\n',
           paste(names(nclass_check[nclass_check]), collapse = ", "))
    }
  }

  # Check for 9999 data
  qual_9999_check <- unlist(lapply(data[, qualitative],
                                   function(x) {
                                     x <- as.integer(x)
                                     any(x == 9999)
                                   }))
  quant_9999_check <- unlist(lapply(data[, quantitative],
                                    function(x) {
                                      any(x == 9999)
                                    }))

  if (any(qual_9999_check)) {
    stop('The following "qualitative" column(s) in "data" have a level ',
         'equal to 9999.',
         '\nThis is the code for missing data in MStrat.\n',
         paste(names(qual_9999_check[qual_9999_check]), collapse = ", "))
  }

  if (any(quant_9999_check)) {
    stop('The following "quantitative" column(s) in "data" have a value ',
         'equal to 9999.',
         '\nThis is the code for missing data in MStrat.\n',
         paste(names(quant_9999_check[quant_9999_check]), collapse = ", "))
  }


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

  # Check if always.selected is > 500
  # MStrat accepts only kernel files with 500 lines.
  if (!is.null(always.selected)) {
    if (length(always.selected) > 500) {
      stop('"Length of always.selected is > 500.')
    }
  }

  # Check if target file path exists
  if(!file.exists(folder.path)) {
    stop('The path specified as "folder.path" does not exist.')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data preprocess ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Fix column names

  fix_names <- function(x) {
    x <- stringi::stri_replace_all_regex(str = make.names(x),
                                         pattern = "\\.+",
                                         replacement = "_")
    x <- stringi::stri_replace_all_regex(str = x,
                                         pattern = "^_|_$",
                                         replacement = "")
    return(x)
  }

  colnames(data) <- fix_names(colnames(data))

  if (!is.null(qualitative)) {
    qualitative <- fix_names(qualitative)
  }

  if (!is.null(quantitative)) {
    quantitative <- fix_names(quantitative)
  }

  if (!is.null(active)) {
    active <- fix_names(active)
  }

  if (!is.null(target)) {
    target <- fix_names(target)
  }

  traits <- c(qualitative, quantitative)

  if (!is.null(genotype)) {
    genotype <- fix_names(genotype)
  }


  inactive <- setdiff(colnames(data), c(genotype, traits))

  inactive <- c(genotype, inactive)

  # Fix inactive
  data[, inactive] <-
    lapply(data[, inactive],
           function(x) {
             stringi::stri_replace_all_regex(str = x,
                                             pattern = "\\s+",
                                             replacement = "_")
           })

  # Scale and center the quantitative data (Z scaling)
  data[, quantitative] <-
    apply(data[, quantitative], 2,
          function(x) scale(x, center = center, scale = scale))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare data file ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  data_out <- data[, c(qualitative, quantitative, inactive)]
  rownames(data_out) <- NULL

  # Bring always.selected to top
  # (MStrat considers only always.selected with code upto 500)
  data_out[, genotype] <- as.factor(data_out[, genotype])
  if (!is.null(always.selected)) {
    genotype_levels <- levels(data_out[, genotype])
    genotype_levels_new <- c(sort(always.selected),
                             sort(setdiff(genotype_levels, always.selected)))
    data_out[, genotype] <- factor(data_out[, genotype],
                                   levels = genotype_levels_new)
  }

  # Convert genotype column to code
  data_out$code <- as.numeric(data_out[, genotype])

  # Sort/order by code [Not required]
  data_out <-  data_out[order(data_out$code), ]

  # Get genotype counts as count column
  # data_out <- dplyr::add_count(x = data_out,
  #                              .data[[genotype]],
  #                              name = "count")

  data_out <- dplyr::mutate(.data = data_out,
                            .by = .data[[genotype]],
                            count = dplyr::row_number(.data[[genotype]]))


  # convert qualitative to numeric
  data_out[, qualitative] <- lapply(data_out[, qualitative],
                                    function(x) as.numeric(x))

  # Convert missing data to 9999
  data_out[, traits][is.na(data_out[, traits])] <- 9999

  # Rearrange columns
  data_out <- data_out[, c("code", "count",
                           qualitative, quantitative, inactive)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare variable file ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  vars <- setdiff(colnames(data_out), c("code", "count"))

  var_df <- data.frame(matrix(ncol = 6, nrow = length(vars),
                              dimnames = list(NULL, c("variable_name",
                                                      "data_type",
                                                      "active",
                                                      "target",
                                                      "weight",
                                                      "nclass"))))
  # first column : name of variable
  var_df$variable_name <- vars

  # second column : 1 (inactive), 2 (qualitative data), 3 (quantitative data)
  var_df$data_type <- ifelse(var_df$variable_name %in% qualitative, 2,
                             ifelse(var_df$variable_name %in% quantitative, 3,
                                    1))
  # third column : 1 (active or marker), 0 (no active variable)
  var_df$active <- ifelse(var_df$variable_name %in% active, 1, 0)

  # fourth column : 1 (target variable), 0 (no target variable)
  var_df$target <- ifelse(var_df$variable_name %in% target, 1, 0)

  # fifth column : (weight)
  if (!is.null(weights.qualitative)) {
    var_df[var_df$variable_name %in% qualitative, ]$weight <-
      weights.qualitative
  }
  if (!is.null(weights.quantitative)) {
    var_df[var_df$variable_name %in% quantitative, ]$weight <-
      weights.quantitative
  }
  var_df$weight <- ifelse(is.na(var_df$weight), 1, var_df$weight)

  # sixth column : number of classes
  if (!is.null(nclass.quantitative)) {
    var_df[var_df$variable_name %in% quantitative, ]$nclass <-
      nclass.quantitative
  }
  var_df$nclass <- ifelse(is.na(var_df$nclass), 5, var_df$nclass)

  var_out <-
    paste("code 0", "individu 0",
          readr::format_delim(x = var_df, delim = " ", col_names = FALSE),
          sep = "\n")
  # cat(var_out)

  var_out <- gsub("\\n$", "", var_out)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare kernel file ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  kernel_out <- data_out[, c("code", genotype)]
  kernel_out$presence <- 0

  if (!is.null(always.selected)) {
    kernel_out[kernel_out[, genotype] %in% always.selected, ]$presence <- 1
  }

  kernel_out[, genotype] <- NULL

  # # Truncate to include only always.selected
  # # as MStrat accepts only first 500
  # if (any(kernel_out$presence == 1)) {
  #   if (nrow(data_out) > 500) {
  #     kernel_out <- kernel_out[kernel_out$presence == 1, ]
  #   }
  # }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Export the input files ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  file.name <- gsub(pattern = "\\s", replacement = "_", x = file.name)

  fname_dat <- paste(file.name, "data.dat", sep = "_")
  fname_var <- paste(file.name, "variable.var", sep = "_")
  fname_ker <- paste(file.name, "kernel.ker", sep = "_")

  # data file
  write.table(x = data_out[1:(nrow(data_out)-1), ],
              file = file.path(folder.path, fname_dat), sep = " ",
              na = "9999", row.names = FALSE, col.names = FALSE,
              quote = FALSE,
              append = FALSE)
  write.table(x = data_out[nrow(data_out), ],
              file = file.path(folder.path, fname_dat), sep = " ",
              na = "9999", row.names = FALSE, col.names = FALSE,
              quote = FALSE,
              append = TRUE, eol = "")
  # variable file
  writeLines(text = var_out,
             con = file.path(folder.path, fname_var))
  # kernel file
  write.table(x = kernel_out,
              file = file.path(folder.path, fname_ker),
              row.names = FALSE, col.names = FALSE)

  message(paste("The following MStrat input files created at", folder.path),
          ":\n", paste(c(fname_dat, fname_var, fname_ker), collapse = "\n"))

}
