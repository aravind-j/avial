#' Parse Output Files from \code{MStrat}
#'
#' @param data.file The path to the \code{.dat} input file used for generating
#'   the output files from \code{MStrat}.
#' @param genotype Name of column/variable with the genotype names as a
#'   character string.
#' @param variable.file The path to the \code{.var} input file used for
#'   generating the output files from \code{MStrat}.
#' @param kernel.file The path to the \code{.ker} input file used for generating
#'   the output files from \code{MStrat}.
#' @param redundancy.output The path to the redundancy output file generated by
#'   \code{MStrat}.
#' @param core.output The path to the core output file generated by
#'   \code{MStrat}.
#'
#' @return A list with the following components: \item{\code{MStrat Core
#'   Output}}{The raw output of core sets constructed by \code{MStrat}.}
#'   \item{\code{MStrat Core Optimised}}{The optimised core set output from
#'   \code{MStrat}.} \item{\code{MStrat Redundancy Output}}{The raw output of
#'   redundancy estimation by \code{MStrat}} \item{\code{MStrat Redundancy
#'   Plot}}{A list of plots of redundancy estimation.}
#'
#' @import ggplot2
#' @importFrom dplyr all_of summarise
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.delim
#' @export
#'
#' @seealso \code{\link[avial]{prep_mstrat_input}}
#'
#' @examplesIf interactive()
#'
#' parse_mstrat_out(data.file = "MStrat_input_data.dat",
#'                  genotype = "Accession",
#'                  variable.file = "MStrat_input_variable.var",
#'                  kernel.file = "MStrat_input_kernel.ker",
#'                  redundancy.output = "MStrat - Redundancy.out",
#'                  core.output = "MStrat - Core.out")
#'
parse_mstrat_out <- function(data.file,
                             genotype = NULL,
                             variable.file,
                             kernel.file,
                             redundancy.output = NULL,
                             core.output = NULL) {
  # Checks ----

  # Load the MStrat input files ----

  data_out <- read.delim(file = data.file, header = FALSE,
                         sep = " ", quote = "",
                         na.strings = 9999, stringsAsFactors = FALSE)

  var_out <- read.delim(file = variable.file, header = FALSE,
                        sep = " ", quote = "",
                        stringsAsFactors = FALSE, skip = 2)
  colnames(data_out) <- c("code", "count", var_out$V1)

  if (!(is.null(kernel.file))) {
    ker_out <- read.delim(file = kernel.file, header = FALSE,
                          sep = " ", quote = "",
                          stringsAsFactors = FALSE)
    colnames(ker_out) <- c("code", "presence")
  }

  core_parsed <- NULL
  core_out <- NULL
  red_parsed <- NULL
  outg <- NULL

  # Load and parse the MStrat core output files ----

  if (!(is.null(core.output))) {
    core_parsed <- read.delim(file = core.output, header = TRUE,
                              sep = "", quote = "",
                              skip = 3, stringsAsFactors = FALSE, strip.white = TRUE)

    # table(core_parsed[, 4])

    # Subset the core
    ind_max <- max(core_parsed[, 4])

    core_out <- core_parsed[core_parsed[, 4] == ind_max, ]

    # Check if kernel accessions are in core
    if (!(is.null(kernel.file))) {
      ker_ind <- ker_out[ker_out$presence == 1, ]$code %in% core_out$Noaccess
      if (!all(ker_ind)) {
        warning("Accessions specified in kernel file are missing in core output.")
      }
    }

    if (!(is.null(genotype))) {
      core_out <-  merge.data.frame(core_out, data_out[, c("code", genotype)],
                                    by.x = "Noaccess", by.y = "code",
                                    all.x = TRUE)
      core_out <- unique(core_out)
    }
  }

  # Load and parse the MStrat redundancy output files ----

  if (!(is.null(redundancy.output))) {

    red_parsed <- read.delim(file = redundancy.output, header = TRUE,
                             sep = "", quote = "",
                             skip = 2, stringsAsFactors = FALSE,
                             strip.white = TRUE)

    red_parsed_long <- red_parsed

    red_parsed_long$method <- as.factor(red_parsed_long$method)
    levels(red_parsed_long$method) <- c("M Method", "Random")

    red_parsed_long <-
      tidyr::pivot_longer(data = red_parsed_long,
                          cols = c("scoreactiv", "scoretarget"),
                          names_to = "Type", values_to = "Value")

    red_parsed_long$Type <- as.factor(red_parsed_long$Type)
    levels(red_parsed_long$Type) <- c("Active", "Target")

    red_parsed_long_avg <-
      summarise(.data = red_parsed_long,
                .by = all_of(c("method", "coresize", "Type", "Value")),
                mean = mean(Value, na.rm = TRUE))

    redg1 <-
      ggplot(data = red_parsed_long_avg,
             aes(x = coresize, y = Value,
                 group = method, colour = method)) +
      geom_point(alpha = 0.8) +
      xlab("Size") +
      ylab("Score") +
      labs(colour = NULL) +
      facet_wrap(~ Type) +
      labs(title = "Average") +
      theme_bw()

    redg2 <-
      ggplot(data = red_parsed_long,
             aes(x = coresize, y = Value,
                 group = method, colour = method)) +
      geom_point(alpha = 0.8) +
      xlab("Size") +
      ylab("Score") +
      labs(colour = NULL) +
      facet_wrap(~ Type) +
      labs(title = "Cloud") +
      theme_bw()

    # outg <-   redg1 / redg2 + plot_layout(guides = "collect")

  }

  out <- list(`MStrat Core Output` = core_parsed,
              `MStrat Core Optimised` = core_out,
              `MStrat Redundancy Output` = red_parsed,
              `MStrat Redundancy Plot` = list(Active = redg1, Target = redg2))

  return(out)

}
