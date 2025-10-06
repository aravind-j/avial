
#' Plot Frequency Distribution and Density Plots
#'
#' @param data The data as a data frame object. The data frame should possess
#'   columns specifying the trait (and genotypes if \code{highlight.genotype.* =
#'   TRUE}).
#' @param trait Name of column specifying the trait as a character string.
#' @param genotype Name of column specifying the group as a character string.
#'   Required only when \code{highlight.genotype.* =  TRUE}.
#' @param hist logical. If \code{TRUE}, the histogram is plotted. Default is
#'   \code{TRUE}.
#' @param hist.col The histograme colour.
#' @param hist.border logical. If \code{TRUE}, histogram border is also plotted.
#'   Default is \code{TRUE}.
#' @param hist.border.col The histogram border colour.
#' @param hist.alpha Alpha transparency for the histogram.
#' @param bw.adjust Multiplicative bin width adjustment. Default is 0.5 which
#'   means use half of the default bandwidth.
#' @param density logical. If \code{TRUE}, the kernel density is plotted.
#'   Default is \code{TRUE}.
#' @param density.col The kernel density colour.
#' @param density.fill The kernel density fill colour.
#' @param density.alpha Alpha transparency for the kernel density
#' @param normal.curve logical. If \code{TRUE}, a normal curve is plotted.
#'   Default is \code{TRUE}.
#' @param normal.curve.col The colour of the normal curve.
#' @param normal.curve.linetype Linetype for the normal curve. See
#'   \code{\link[ggplot2]{aes_linetype_size_shape}}.
#' @param highlight.mean logical. If \code{TRUE}, the mean value is highlighted
#'   as a vertical line. Default is \code{TRUE}.
#' @param highlight.mean.col The colour of the vertical line representing mean.
#' @param show.counts If \code{TRUE}, group wise counts are plotted as a text
#'   annotation. Default is \code{TRUE}.
#' @param count.text.col The colour of the count text annotation.
#' @param count.text.size The size of the count text annotation.
#' @param highlight.genotype.vline logical. If \code{TRUE}, the mean values of
#'   genotypes specified in \code{highlights} are plotted as vertical lines.
#' @param highlight.genotype.pointrange logical. If \code{TRUE}, the mean ±
#'   stand error values of genotypes specified in \code{highlights} are plotted
#'   as a separate pointrange plot.
#' @param highlights The genotypes to be highlighted as a character vector.
#' @param highlight.col The colour(s) to be used to highlight genotypes
#'   specified in \code{highlights} in the plot as a character vector. Must be
#'   valid colour values in R (named colours, hexadecimal representation, index
#'   of colours [\code{1:8}] in default R \code{palette()} etc.).
#' @param standardize.xrange logical. If \code{TRUE}, the original plot and the
#'   pointrange plot x axis ranges are standardized. Default is \code{TRUE}.
#'
#' @returns Either the frequency distribution plot as a histogram or kernel
#'   density \code{ggplot2} object, or a list containing a \code{ggplot2}
#'   pointrange plot and the histogram/kernel density plot.
#'
#' @importFrom dplyr n summarise
#' @importFrom stats sd
#' @export
#'
#' @examples
#' library(agridat)
#' library(ggplot2)
#' library(patchwork)
#'
#' soydata <- australia.soybean
#' soydata$gen <- as.character(soydata$gen)
#' checks <- c("G01", "G05")
#'
#' check_cols <- c("#B2182B", "#2166AC")
#'
#'
#' # Frequency distribution as histogram
#' freq_hist1 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = TRUE,
#'                     hist.col = "lemonchiffon",
#'                     density = FALSE,
#'                     normal.curve = FALSE, highlight.mean = FALSE,
#'                     show.counts = FALSE)
#' freq_hist1
#'
#' # Frequency distribution as histogram with normal curve
#' freq_hist2 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = TRUE,
#'                     hist.col = "lemonchiffon",
#'                     density = FALSE,
#'                     normal.curve = TRUE, normal.curve.col = "blue",
#'                     highlight.mean = FALSE,
#'                     show.counts = FALSE)
#' freq_hist2
#'
#' # Frequency distribution as histogram with mean highlighted
#' freq_hist3 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = TRUE,
#'                     hist.col = "lemonchiffon",
#'                     density = FALSE, normal.curve = FALSE,
#'                     highlight.mean = TRUE, highlight.mean.col = "red",
#'                     show.counts = FALSE)
#' freq_hist3
#'
#' # Frequency distribution as histogram with count value
#' freq_hist4 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = TRUE,
#'                     hist.col = "lemonchiffon",
#'                     density = FALSE, normal.curve = FALSE,
#'                     highlight.mean = FALSE,
#'                     show.counts = TRUE, count.text.col = "red")
#' freq_hist4
#'
#' # Frequency distribution as histogram with check values
#' # highlighted as vertical lines
#' freq_hist5 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = TRUE,
#'                     hist.col = "lemonchiffon",
#'                     density = FALSE, normal.curve = FALSE,
#'                     highlight.mean = FALSE, show.counts = FALSE,
#'                     genotype = "gen",
#'                     highlight.genotype.vline = TRUE, highlights = checks,
#'                     highlight.col = check_cols)
#' freq_hist5
#'
#' # Frequency distribution as histogram with check values
#' # highlighted as a separate pointrange plot
#' freq_hist6 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = TRUE,
#'                     hist.col = "lemonchiffon",
#'                     density = FALSE, normal.curve = FALSE,
#'                     highlight.mean = FALSE, show.counts = FALSE,
#'                     genotype = "gen",
#'                     highlight.genotype.vline = TRUE,
#'                     highlight.genotype.pointrange = TRUE,
#'                     highlights = checks,
#'                     highlight.col = check_cols)
#' freq_hist6[[1]] <-
#'   freq_hist6[[1]] +
#'   theme(axis.ticks.x = element_blank(),
#'         axis.text.x = element_blank())
#' wrap_plots(freq_hist6[[1]], plot_spacer(), freq_hist6[[2]],
#'            ncol = 1, heights = c(1, -0.5, 4))
#'
#' # Frequency distribution as kernel density plot
#' freq_dens1 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = FALSE,
#'                     density = TRUE,
#'                     density.fill = "lemonchiffon",
#'                     normal.curve = FALSE, highlight.mean = FALSE,
#'                     show.counts = FALSE)
#' freq_dens1
#'
#' # Frequency distribution as kernel density plot with mean highlighted
#' freq_dens2 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = FALSE,
#'                     density = TRUE,
#'                     density.fill = "lemonchiffon",
#'                     normal.curve = FALSE,
#'                     highlight.mean = TRUE, highlight.mean.col = "red",
#'                     show.counts = FALSE)
#' freq_dens2
#'
#' # Frequency distribution as kernel density plot with count value
#' freq_dens3 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = FALSE,
#'                     density = TRUE,
#'                     density.fill = "lemonchiffon",
#'                     normal.curve = FALSE,
#'                     highlight.mean = FALSE,
#'                     show.counts = TRUE, count.text.col = "red")
#' freq_dens3
#'
#' # Frequency distribution as kernel density plot with check values
#' # highlighted as vertical lines
#' freq_dens4 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = FALSE,
#'                     density = TRUE,
#'                     density.fill = "lemonchiffon",
#'                     normal.curve = FALSE,
#'                     highlight.mean = FALSE, show.counts = FALSE,
#'                     genotype = "gen",
#'                     highlight.genotype.vline = TRUE, highlights = checks,
#'                     highlight.col = check_cols)
#' freq_dens4
#'
#' # Frequency distribution as kernel density plot with check values
#' # highlighted as a separate pointrange plot
#' freq_dens5 <-
#'   freq_distribution(data = soydata, trait = "lodging",
#'                     hist = FALSE,
#'                     density = TRUE,
#'                     density.fill = "lemonchiffon",
#'                     normal.curve = FALSE,
#'                     highlight.mean = FALSE, show.counts = FALSE,
#'                     genotype = "gen",
#'                     highlight.genotype.vline = TRUE,
#'                     highlight.genotype.pointrange = TRUE,
#'                     highlights = checks,
#'                     highlight.col = check_cols)
#' freq_dens5[[1]] <-
#'   freq_dens5[[1]] +
#'   theme(axis.ticks.x = element_blank(),
#'         axis.text.x = element_blank())
#' wrap_plots(freq_dens5[[1]], plot_spacer(), freq_dens5[[2]],
#'            ncol = 1, heights = c(1, -0.5, 4))
#'
#'
freq_distribution <- function(data, trait,
                              genotype = NULL,
                              hist = TRUE,
                              hist.col = "gray45",
                              hist.border = TRUE,
                              hist.border.col = "black",
                              hist.alpha = 0.8,
                              bw.adjust = 0.5,
                              density = TRUE,
                              density.col = "black",
                              density.fill = "gray45",
                              density.alpha = 0.1,
                              normal.curve = TRUE,
                              normal.curve.col = "black",
                              normal.curve.linetype = "solid",
                              highlight.mean = TRUE,
                              highlight.mean.col = "black",
                              show.counts = TRUE,
                              count.text.col = "black",
                              count.text.size = 3,
                              highlight.genotype.vline = FALSE,
                              highlight.genotype.pointrange = FALSE,
                              highlights = NULL,
                              highlight.col = highlight.mean.col,
                              standardize.xrange = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Checks ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Check if data.frame
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object.')
  }

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble.\nCoercing to data frame.')
    data <- as.data.frame(data)
  }

  # Check if trait column present in data
  if (!(trait %in% colnames(data))) {
    stop(paste('Column ', trait,
               ' specified as the trait column is not present in "data".',
               sep = ""))
  }

  # Check if trait column is of type numeric
  if (!is.numeric(data[, trait])) {
    stop(paste('Column ', trait,
               ' in "data" is not of type "numeric".',
               sep = ""))
  }

  # Check if genotype column present in data
  if (highlight.genotype.vline == TRUE ||
      highlight.genotype.pointrange == TRUE) {
    if (!(genotype %in% colnames(data))) {
      stop(paste('Column ', genotype,
                 ' specified as the genotype column is not present in "data".',
                 sep = ""))
    }

    # Check if genotype column is of type character
    if (!is.character(data[, genotype])) {
      stop(paste('Column ', genotype,
                 ' in "data" is not of type "character".',
                 sep = ""))
    }

    if (!any(highlights %in% data[, genotype])) {
      highlights_missing <- highlights[!(highlights %in% data[, genotype])]
      stop('The following genotypes specified in highlights ',
           'are missing from ', genotype, ' column in "data".\n',
           paste(highlights_missing, collapse = ", ")
      )
    }

    if (length(highlight.col) != 1) {
      if (length(highlight.col) != length(checks)) {
        stop('"highlights" and "highlight.col" are of unequal lengths.')
      }
    }
  }

  # Compute the binwidth
  bw <- binw(data[, trait], "sturges")

  # Summary data.frame
  if (highlight.mean == TRUE | show.counts == TRUE) {
    data_summ <-
      summarise(.data = data,
                # count = n(),
                count = sum(!is.na(.data[[trait]])),
                mean = mean(.data[[trait]], na.rm = TRUE),
                # se = plotrix::std.error(.data[[trait]], na.rm = TRUE),
                se = sd(.data[[trait]], na.rm = TRUE) /
                  sqrt(length(.data[[trait]]
                              [!is.na(.data[[trait]])])))
  }

  if (hist.border) {
    hist.border.col <- hist.border.col
  } else {
    hist.border.col <- "transparent"
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  outg <- ggplot(data, aes(x = .data[[trait]]))

  ## Add histogram ----

  if (hist == TRUE) {

    outg <- outg +
      geom_histogram(alpha = hist.alpha, binwidth = bw * bw.adjust,
                     position = "identity", show.legend = FALSE,
                     fill = hist.col,
                     colour = hist.border.col)

  }

  ## Add density ----
  if (density == TRUE) {
    outg <- outg +
      geom_density(mapping = aes(y = after_stat(count) * bw * bw.adjust),
                   alpha = density.alpha, show.legend = FALSE,
                   fill = density.fill,
                   colour = density.col)
  }

  data <- as.data.frame(data)

  ## Add normal curve ----
  if (normal.curve == TRUE) {

    if (!all(is.na(data[, trait]))) {
      outg <- outg +
        stat_function(data = data, aes(x = .data[[trait]]),
                      fun = dnorm_ggplot,
                      args = list(mean = mean(data[, trait], na.rm = TRUE),
                                  sd = sd(data[, trait], na.rm = TRUE),
                                  n = length(data[, trait]),
                                  bw = bw * bw.adjust),
                      linetype = normal.curve.linetype,
                      colour = normal.curve.col)
    }
  }


  ## Add mean ----
  if (highlight.mean == TRUE) {
    outg <- outg +
      geom_vline(data = data_summ[data_summ$count != 0, ],
                 aes(xintercept = mean),
                 linetype = "dashed", show.legend = TRUE,
                 colour = highlight.mean.col)
  }

  ## Show counts ----
  if (show.counts == TRUE) {

    vjust_custom <- 1.5

    outg <- outg +
      geom_text(data = data_summ,
                aes(x = Inf, y = Inf,
                    vjust = vjust_custom, hjust = 1.5,
                    label = paste("n =", count)),
                size = count.text.size, show.legend = TRUE,
                colour = count.text.col)
  }

  ## Highlight checks ----
  if (highlight.genotype.vline == TRUE ||
      highlight.genotype.pointrange == TRUE) {
    data_summ_highlights <-
      summarise(.data = data[data[, genotype] %in% highlights, ],
                .by = all_of(c(genotype)),
                # count = n(),
                count = sum(!is.na(.data[[trait]])),
                mean = mean(.data[[trait]], na.rm = TRUE),
                # se = plotrix::std.error(.data[[trait]], na.rm = TRUE),
                se = sd(.data[[trait]], na.rm = TRUE) /
                  sqrt(length(.data[[trait]]
                              [!is.na(.data[[trait]])])))
  }

  if (highlight.genotype.vline == TRUE) {
    outg <-
      outg +
      geom_vline(data = data_summ_highlights[data_summ_highlights$count != 0, ],
                 aes(xintercept = mean, colour = .data[[genotype]]),
                 linetype = "dashed", show.legend = TRUE) +
      scale_colour_manual(values = highlight.col)
  }

  outg <- outg +
    xlab(trait) +
    ylab("Count") +
    theme_bw()

  if (highlight.genotype.pointrange == TRUE) {
    data_summ_highlights$lower <-
      data_summ_highlights$mean - data_summ_highlights$se
    data_summ_highlights$upper <-
      data_summ_highlights$mean + data_summ_highlights$se

    outg_h <-
      ggplot(data_summ_highlights, aes(y = .data[[genotype]], x = mean)) +
      geom_pointrange(aes(xmin = lower, xmax = upper),
                      colour = highlight.col)
    outg_h <-
      outg_h +
      ylab("") +
      xlab("") +
      theme_bw()

    if (standardize.xrange == TRUE) {
      xrange <- c(layer_scales(outg_h)$x$range$range,
                  layer_scales(outg)$x$range$range)

      outg_h <- outg_h + xlim(c(min(xrange), max(xrange)))
      outg <- outg + xlim(c(min(xrange), max(xrange)))
    }

    outg <- list(outg_h, outg)
  }

  return(outg)

}
