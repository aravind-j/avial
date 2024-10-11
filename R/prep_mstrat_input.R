# test weights > 1


prep_mstrat_input <- function(data, genotype,
                              qualitative, quantitative, #inactive,
                              active, target,
                              weights.qualitative = NULL,
                              weights.quantitative = NULL,
                              center = TRUE, scale = TRUE) {

  # an optional vector of ‘prior weights’ to be used in the fitting process. Should be NULL or a numeric vector.

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
      stop(paste('The following column(s) specified in "quantitative" not present in "data":\n',
                 paste(quantitative[!(quantitative %in% colnames(data))],
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if 'qualitative' columns are present in 'data'
  if (!is.null(qualitative)) {
    if (FALSE %in% (qualitative %in% colnames(data)))  {
      stop(paste('The following column(s) specified in "qualitative" not present in "data":\n',
                 paste(qualitative[!(qualitative %in% colnames(data))],
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if overlap exists between 'quantitative' and 'qualitative'
  if ((!is.null(quantitative)) & (!is.null(qualitative))) {
    if (length(intersect(quantitative, qualitative)) != 0) {
      stop(paste('The following column(s) is/are specified in both "quantitative" and "qualitative":\n',
                 paste(intersect(quantitative, qualitative),
                       collapse = ", "),
                 sep = ""))
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
    intquantcols <- unlist(lapply(data[, quantitative],
                                  function(x) FALSE %in% (is.vector(x, mode = "integer") | is.vector(x, mode = "numeric"))))
    if (TRUE %in% intquantcols) {
      stop(paste('The following "quantitative" column(s) in "data" are not of type numeric:\n',
                 paste(names(intquantcols[intquantcols]), collapse = ", ")))
    }
  }

  # check if 'qualitative' columns are of type factor
  if (!is.null(qualitative)) {
    intqualcols <- unlist(lapply(data[, qualitative],
                                 function(x) is.factor(x)))
    if (FALSE %in% intqualcols) {
      stop(paste('The following "qualitative" column(s) in "data" are not of type factor:\n',
                 paste(names(intqualcols[!intqualcols]), collapse = ", ")))
    }
  }

  traits <- c(qualitative, quantitative)

  # check if 'active' traits/markers are present in 'traits'
  if (FALSE %in% (active %in% traits))  {
    stop(paste('The following column(s) specified in "active" not present in',
               ' "qualitative" or quantitative:\n',
               paste(active[!(active %in% traits)],
                     collapse = ", "),
               sep = ""))
  }

  # check if 'target' traits/markers are present in 'traits'
  if (FALSE %in% (target %in% traits))  {
    stop(paste('The following column(s) specified in "target" not present in',
               '"qualitative" or quantitative:\n',
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

  # Check for 9999 data

  qual_9999_check <- apply(data[, qualitative], 2,
                           function(x) {
                             x <- as.numeric(x)
                             any(x == 9999)
                           })
  quant_9999_check <- apply(data[, quantitative], 2,
                            function(x) {
                              any(x == 9999)
                            })

  if (any(qual_9999_check)) {
    stop(paste('The following "qualitative" column(s) in "data" have a level equal to 9999.',
               '\nThis is the code for missing data in MStrat.\n',
               paste(names(qual_9999_check[qual_9999_check]), collapse = ", ")))
  }

  if (any(quant_9999_check)) {
    stop(paste('The following "quantitative" column(s) in "data" have a value equal to 9999.',
               '\nThis is the code for missing data in MStrat.\n',
               paste(names(quant_9999_check[quant_9999_check]), collapse = ", ")))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data preprocess ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  inactive <- setdiff(colnames(data), c(genotype, traits))

  # Scale and center the quantitative data (Z scaling)
  data[, quantitative] <-
    apply(data[, quantitative], 2,
          function(x) scale(x, center = center, scale = scale))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare data file ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  inactive <- c(genotype, inactive)

  # Spacer between fields is blank(space). All data should not have space.
  #   - inactive, genotype: convert all space to "" or "-".
  #   - qualitative: Convert factor to numeric.
  #   - quantitative: OK.

  # Missing data hase the code 9999. Check for 9999 values

  data_out <- data[, c(qualitative, quantitative, inactive)]
  rownames(data_out) <- NULL

  # Convert genotype column to code
  data_out$code <- as.numeric(as.factor(data_out[, genotype]))

  # Get genotype counts as count column
  data_out <- dplyr::add_count(x = data_out,
                               .data[["Accession"]],
                               name = "count")


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
    var_df[var_df$variable_name %in% qualitative, ]$weight <- weights.qualitative
  }
  if (!is.null(weights.quantitative)) {
    var_df[var_df$variable_name %in% quantitative, ]$weight <- weights.quantitative
  }
  var_df$weight <- ifelse(is.na(var_df$weight), 1, var_df$weight)

  # sixth column : number of classes

  var_df$nclass <- ifelse(is.na(var_df$nclass), 5, var_df$nclass)

  c("code")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare kernel file ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

}
