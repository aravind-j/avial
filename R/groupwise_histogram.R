#' Plot Group-wise Histograms
#'
#' @param data The data as a data frame object. The data frame should possess
#'   columns specifying the group and trait.
#' @param group Name of column specifying the group as a character string.
#' @param trait Name of column specifying the trait as a character string.
#' @param background.hist logical. If \code{TRUE}, the background data histogram
#'   is plotted. Default is \code{TRUE}.
#' @param background.hist.alpha Alpha transparency for the background data
#'   histogram.
#' @param background.density logical. If \code{TRUE}, the background data kernel
#'   density is plotted. Default is \code{TRUE}.
#' @param background.density.alpha Alpha transparency for the background data
#'   kernel density.
#' @param hist logical. If \code{TRUE}, the group-wise histogram is plotted.
#'   Default is \code{TRUE}.
#' @param hist.alpha Alpha transparency for the group-wise histogram.
#' @param hist.border logical. If \code{TRUE}, histogram border is also plotted.
#'   Default is \code{TRUE}.
#' @param hist.position Histogram position adjustment. Either "identity" or
#'   "stack".
#' @param bw.adjust Multiplicative bin width adjustment. Default is 0.5 which
#'   means use half of the default bandwidth.
#' @param density logical. If \code{TRUE}, the group-wise kernel density is
#'   plotted. Default is \code{TRUE}.
#' @param density.alpha Alpha transparency for the group-wise kernel density
#' @param normal.curve logical. If \code{TRUE}, a normal curve is plotted.
#'   Default is \code{TRUE}.
#' @param normal.curve.linetype Linetype for the normal curve. See
#'   \code{\link[ggplot2]{aes_linetype_size_shape}}.
#' @param highlight.mean logical. If \code{TRUE}, the mean value is highlighted
#'   as a vertical line. Default is \code{TRUE}.
#' @param show.counts logical. If \code{TRUE}, group wise counts are plotted as
#'   a text annotation. Default is \code{TRUE}.
#' @param count.text.size The size of the count text annotation.
#' @param subset The method for subsetting the plots according to the argument
#'   \code{"group"}. Either \code{"facet"} for getting an plot using faceting in
#'   \code{ggplot2} or \code{"list"} for getting a list of plots.
#' @param ncol Number of columns when \code{subset = "facet"}.
#' @param nrow Number of rows when \code{subset = "facet"}.
#'
#' @return The group-wise histogram as a \code{ggplot2} plot grob or as a list
#'   of \code{ggplot2} plot grobs.
#'
#' @import ggplot2
#' @importFrom dplyr all_of arrange summarise n
#' @importFrom scales hue_pal
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' library(agridat)
#' library(ggplot2)
#' library(patchwork)
#'
#' soydata <- australia.soybean
#'
#' clrs <- c("#B2182B", "#2166AC", "#009E53", "#E69F00")
#' clrs_dark <- colorspace::darken(clrs, amount = 0.2)
#'
#' # Group-wise histogram ----
#' outg_hist <-
#'   groupwise_histogram(data = soydata, group = "loc", trait = "lodging",
#'                       background.hist = FALSE,
#'                       background.density = FALSE,
#'                       hist.alpha = 0.5,
#'                       density = FALSE,
#'                       subset = "none")
#' outg_hist
#'
#' outg_hist +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' # Group-wise histogram - stacked ----
#' outg_hist_stack <-
#'   groupwise_histogram(data = soydata, group = "loc", trait = "lodging",
#'                       background.hist = FALSE,
#'                       background.density = FALSE,
#'                       hist.position = "stack",
#'                       density = FALSE,
#'                       normal.curve = FALSE,
#'                       subset = "none")
#' outg_hist_stack
#'
#' outg_hist_stack +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' # Group-wise histogram with facet ----
#' outg_hist_facet <-
#'   groupwise_histogram(data = soydata, group = "loc", trait = "lodging",
#'                       background.hist = FALSE,
#'                       background.density = FALSE,
#'                       hist.alpha = 0.5,
#'                       density = FALSE,
#'                       subset = "facet")
#' outg_hist_facet
#'
#' outg_hist_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' # Group-wise histogram as list ----
#' outg_hist_list <-
#'   groupwise_histogram(data = soydata, group = "loc", trait = "lodging",
#'                       background.hist = FALSE,
#'                       background.density = FALSE,
#'                       hist.alpha = 0.5,
#'                       density = FALSE,
#'                       subset = "list")
#'
#' wrap_plots(outg_hist_list, nrow = 2, guides = "collect")
#'
#' outg_hist_list <-
#'   lapply(seq_along(outg_hist_list), function(i) {
#'     outg_hist_list[[i]] +
#'       scale_fill_manual(values = clrs[i]) +
#'       scale_colour_manual(values = clrs_dark[i])
#'   })
#'
#' wrap_plots(outg_hist_list, nrow = 2, guides = "collect")
#'
#' # Group-wise density ----
#' outg_density <-
#'   groupwise_histogram(data = soydata, group = "loc", trait = "lodging",
#'                       background.hist = FALSE,
#'                       background.density = TRUE,
#'                       hist = FALSE,
#'                       density = TRUE,
#'                       normal.curve = FALSE,
#'                       subset = "none")
#' outg_density
#'
#' outg_density +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' # Group-wise density with facet ----
#' outg_density_facet <-
#'   groupwise_histogram(data = soydata, group = "loc", trait = "lodging",
#'                       background.hist = FALSE,
#'                       background.density = TRUE,
#'                       hist = FALSE,
#'                       density = TRUE,
#'                       normal.curve = FALSE,
#'                       subset = "facet")
#' outg_density_facet
#'
#' outg_density_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' # Group-wise density as list ----
#' outg_density_list <-
#'   groupwise_histogram(data = soydata, group = "loc", trait = "lodging",
#'                       background.hist = FALSE,
#'                       background.density = TRUE,
#'                       hist = FALSE,
#'                       density = TRUE,
#'                       normal.curve = FALSE,
#'                       subset = "list")
#'
#' wrap_plots(outg_density_list, nrow = 2, guides = "collect")
#'
#' outg_density_list <-
#'   lapply(seq_along(outg_density_list), function(i) {
#'     outg_density_list[[i]] +
#'       scale_fill_manual(values = clrs[i]) +
#'       scale_colour_manual(values = clrs_dark[i])
#'   })
#'
#' wrap_plots(outg_density_list, nrow = 2, guides = "collect")
#'
#' # Group-wise density + histogram ----
#' outg_density_hist <-
#'   groupwise_histogram(data = soydata, group = "loc", trait = "lodging",
#'                       background.hist = FALSE,
#'                       background.density = FALSE,
#'                       hist = TRUE,
#'                       hist.alpha = 0.3,
#'                       density = TRUE,
#'                       normal.curve = FALSE,
#'                       subset = "none")
#' outg_density_hist
#'
#' outg_density_hist +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' # Group-wise density + histogram with facet ----
#' outg_density_hist_facet <-
#'   groupwise_histogram(data = soydata, group = "loc", trait = "lodging",
#'                       background.hist = FALSE,
#'                       background.density = FALSE,
#'                       hist = TRUE,
#'                       hist.alpha = 0.3,
#'                       density = TRUE,
#'                       normal.curve = FALSE,
#'                       subset = "facet")
#' outg_density_hist_facet
#'
#' outg_density_hist_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' # Group-wise density + histogram as list ----
#' outg_density_hist_list <-
#'   groupwise_histogram(data = soydata, group = "loc", trait = "lodging",
#'                       background.hist = FALSE,
#'                       background.density = FALSE,
#'                       hist = TRUE,
#'                       hist.alpha = 0.3,
#'                       density = TRUE,
#'                       normal.curve = FALSE,
#'                       subset = "list")
#'
#' wrap_plots(outg_density_hist_list, nrow = 2, guides = "collect")
#'
#' outg_density_hist_list <-
#'   lapply(seq_along(outg_density_hist_list), function(i) {
#'     outg_density_hist_list[[i]] +
#'       scale_fill_manual(values = clrs[i]) +
#'       scale_colour_manual(values = clrs_dark[i])
#'   })
#'
#' wrap_plots(outg_density_hist_list, nrow = 2, guides = "collect")
#'
groupwise_histogram <- function(data, group, trait,
                                background.hist = TRUE,
                                background.hist.alpha = 0.25,
                                background.density = TRUE,
                                background.density.alpha = 0.1,
                                hist = TRUE,
                                hist.border = TRUE,
                                hist.position = c("identity", "stack"),
                                hist.alpha = 0.8,
                                bw.adjust = 0.5,
                                density = TRUE,
                                density.alpha = 0.1,
                                normal.curve = TRUE,
                                normal.curve.linetype = "solid",
                                highlight.mean = TRUE,
                                show.counts = TRUE,
                                count.text.size = 3,
                                subset = c("facet", "list", "none"),
                                ncol = NULL, nrow = NULL) {

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

  # Check if group column present in data
  if (!(group %in% colnames(data))) {
    stop(paste('Column ', group,
               ' specified as the group column is not present in "data".',
               sep = ""))
  }

  # Check if trait column present in data
  if (!(trait %in% colnames(data))) {
    stop(paste('Column ', trait,
               ' specified as the trait column is not present in "data".',
               sep = ""))
  }

  # Check if group column is of type factor
  if (class(data[, group]) != "factor") {
    stop(paste('Column ', group,
               ' in "data" is not of type "factor".',
               sep = ""))
  }

  # Check if trait column is of type numeric
  if (class(data[, trait]) != "numeric") {
    stop(paste('Column ', trait,
               ' in "data" is not of type "numeric".',
               sep = ""))
  }

  # if (facet == FALSE) {
  #   background.hist = FALSE
  # }

  if (is.null(hist.position)) {
    hist.position <- "identity"
  }
  hist.position <- match.arg(hist.position)

  # Compute the binwidth
  bw <- binw(data[, trait], "sturges")


  p <- levels(data[, group])
  # p <- sort(levels(data[, group]))
  # data[, group] <- factor(data[, group], p)

  # Prepare aesthetics according to hist.border
  if (hist.border == TRUE) {
    hist_aes <- aes(fill = .data[[group]],
                    colour = .data[[group]])
  } else {
    hist_aes <- aes(fill = .data[[group]])
  }

  # Summary data.frame
  if (highlight.mean == TRUE | show.counts == TRUE) {
    data_summ <-
      summarise(.data = data,
                .by = all_of(c(group)),
                count = n(),
                mean = mean(.data[[trait]], na.rm = TRUE),
                # se = plotrix::std.error(.data[[trait]], na.rm = TRUE),
                se = sd(.data[[trait]], na.rm = TRUE) /
                  sqrt(length(.data[[trait]]
                              [!is.na(.data[[trait]])])))
    data_summ <- dplyr::arrange(.data = data_summ, .by = .data[[group]])
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Single plot or facet ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (subset != "list") {

    outg <- ggplot(data, aes(x = .data[[trait]]))

    ## Add background histogram ----
    if (background.hist == TRUE) {

      outg <- outg +
        geom_histogram(data = data[, setdiff(colnames(data), group)],
                       fill = "black",
                       alpha = background.hist.alpha,
                       binwidth = bw * bw.adjust)

    }

    ## Add background density ----
    if (background.density == TRUE) {

      outg <- outg +
        geom_density(data = data[, setdiff(colnames(data), group)],
                     mapping = aes(y = after_stat(count) * bw * bw.adjust),
                     fill = "black",
                     alpha = background.density.alpha)

    }

    ## Add group-wise histogram ----
    if (hist == TRUE) {

      outg <- outg +
        geom_histogram(mapping = hist_aes,
                       alpha = hist.alpha, binwidth = bw * bw.adjust,
                       position = hist.position)

    }

    ## Add group-wise density ----
    if (density == TRUE) {

      outg <- outg +
        geom_density(mapping = modifyList(aes(y = after_stat(count) * bw * bw.adjust),
                                          hist_aes),
                     alpha = density.alpha)

    }

    ## Add facet ----
    if (subset == "facet") {
      outg <- outg +
        facet_wrap(~ .data[[group]], scales = "fixed",
                   nrow = nrow, ncol = ncol)

    }

    ## Add normal curve ----
    if (normal.curve == TRUE) {
      for (i in seq_along(p))  {
        df <- data[data[, group] == p[i], ]
        outg <- outg +
          stat_function(data = df, aes(x = .data[[trait]],
                                       colour = .data[[group]]),
                        fun = dnorm_ggplot,
                        args = list(mean = mean(df[, trait], na.rm = TRUE),
                                    sd = sd(df[, trait], na.rm = TRUE),
                                    n = length(df[, trait]),
                                    bw = bw * bw.adjust),
                        linetype = normal.curve.linetype)
      }
    }

    ## Add mean ----
    if (highlight.mean == TRUE) {

      for (i in seq_along(p))  {
        outg <- outg +
          geom_vline(data = data_summ, aes(xintercept = mean,
                                           colour = .data[[group]]),
                     linetype = "dashed")
      }

    }

    ## Show counts ----
    if (show.counts == TRUE) {

      vjust_custom <- 1.5

      if (subset == "none") {
        vjust_custom <- ((seq_along(p) - 1) * 2) + (count.text.size)
      }

      for (i in seq_along(p))  {
        outg <- outg +
          geom_text(data = data_summ, aes(x = Inf, y = Inf,
                                          vjust = vjust_custom, hjust = 1.5,
                                          colour = .data[[group]],
                                          label = paste("n =", count)),
                    size = count.text.size)
      }

    }

    return(outg +
             xlab(trait) +
             ylab("Count") +
             theme_bw())


  } else { # subset ==  list

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # List output ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    colhex <- scales::hue_pal()(length(p))


    gpdata_list <- lapply(seq_along(p), function(i) {
      data[data[, group] == p[i], ]
    })
    names(gpdata_list) <- p

    gpdata_summ_list <- lapply(seq_along(p), function(i) {
      data_summ[data_summ[, group] == p[i], ]
    })
    names(gpdata_summ_list) <- p

    outg_list <- lapply(seq_along(p), function(i) {
      ggplot(gpdata_list[[i]], aes(x = .data[[trait]])) +
        facet_wrap(~ .data[[group]])
    })
    names(outg_list) <- p

    ## Add background histogram ----

    if (background.hist == TRUE) {

      outg_list <- lapply(seq_along(p), function(i) {
        outg_list[[i]] +
          geom_histogram(data = data[, setdiff(colnames(data), group)],
                         alpha = background.hist.alpha,
                         binwidth = bw * bw.adjust)
      })

    }

    ## Add background density ----
    if (background.density == TRUE) {

      outg_list <- lapply(seq_along(p), function(i) {
        outg_list[[i]] +
          geom_density(data = data[, setdiff(colnames(data), group)],
                       mapping = aes(y = after_stat(count) * bw * bw.adjust),
                       fill = "black",
                       alpha = background.density.alpha)
      })

    }

    ## Add group-wise histogram ----
    if (hist == TRUE) {

      outg_list <- lapply(seq_along(p), function(i) {
        colourval <- ifelse(hist.border == TRUE, colhex[i], NULL)

        outg_list[[i]] +
          geom_histogram(mapping = hist_aes,
                         alpha = hist.alpha, binwidth = bw * bw.adjust,
                         position = hist.position) +
          scale_fill_manual(values = colhex[i]) +
          scale_colour_manual(values = colourval)
      })

    }

    ## Add group-wise density ----
    if (density == TRUE) {

      outg_list <- lapply(seq_along(p), function(i) {
        colourval <- ifelse(hist.border == TRUE, colhex[i], NULL)

        remove_scales(outg_list[[i]], scales = c("fill", "colour")) +
          geom_density(mapping = modifyList(aes(y = after_stat(count) * bw * bw.adjust),
                                            hist_aes),
                       alpha = density.alpha) +
          scale_fill_manual(values = colhex[i]) +
          scale_colour_manual(values = colourval)
      })

    }

    ## Add normal curve ----
    if (normal.curve == TRUE) {

      outg_list <- lapply(seq_along(p), function(i) {
        outg_list[[i]] <- remove_scales(outg_list[[i]], scales = "colour") +
          stat_function(aes(x = .data[[trait]],
                            colour = .data[[group]]),
                        fun = dnorm_ggplot,
                        args = list(mean = mean(gpdata_list[[i]][, trait],
                                                na.rm = TRUE),
                                    sd = sd(gpdata_list[[i]][, trait],
                                            na.rm = TRUE),
                                    n = length(gpdata_list[[i]][, trait]),
                                    bw = bw * bw.adjust),
                        linetype = normal.curve.linetype) +
          scale_colour_manual(values = colhex[i])
      })

    }

    ## Add mean ----
    if (highlight.mean == TRUE) {

      outg_list <- lapply(seq_along(p), function(i) {
        outg_list[[i]] <- remove_scales(outg_list[[i]], scales = "colour") +
          geom_vline(data = gpdata_summ_list[[i]],
                     aes(colour = .data[[group]], xintercept = mean),
                     linetype = "dashed") +
          scale_colour_manual(values = colhex[i])
      })

    }

    ## Show counts ----
    if (show.counts == TRUE) {

      vjust_custom <- 1.5

      outg_list <- lapply(seq_along(p), function(i) {
        outg_list[[i]] <- remove_scales(outg_list[[i]], scales = "colour") +
          geom_text(data =  gpdata_summ_list[[i]],
                    aes(colour = .data[[group]], label = paste("n = ", count)),
                    x = Inf, y = Inf,
                    vjust = vjust_custom, hjust = 1.5,
                    show.legend = TRUE) +
          scale_colour_manual(values = colhex[i])
      })

    }

    ## Standardize limits ----
    xrange <- unlist(lapply(outg_list, function(x) {
      layer_scales(x)$x$range$range
    }))
    yrange <- unlist(lapply(outg_list, function(x) {
      layer_scales(x)$y$range$range
    }))

    outg_list <- lapply(seq_along(p), function(i) {
      outg_list[[i]] <- outg_list[[i]] +
        xlim(c(min(xrange), max(xrange))) +
        ylim(c(min(yrange), max(yrange)))
    })

    ## Final theme ----
    outg_list <- lapply(seq_along(p), function(i) {
      outg_list[[i]] <- outg_list[[i]] +
        xlab(trait) +
        ylab("Count") +
        theme_bw()
    })

    return(outg_list)

  }

}
