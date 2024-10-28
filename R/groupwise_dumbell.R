#' Plot Group-wise Dumbell Plots
#'
#' @inheritParams groupwise_histogram
#' @param genotype Name of column specifying the genotype as a character string.
#' @param diff.sort The order for sorting the genotypes for plotting. Either
#'   \code{"ascending"}, \code{"descending"} or \code{"none"}.
#' @param segment logical. If \code{TRUE}, the dumbell segment is plotted.
#'   Default is \code{TRUE}.
#' @param segment.size The size of the dumbell segment.
#' @param segment.colour The colour of the dumbell segment.
#' @param segment.alpha Alpha transparency for the dumbell segment.
#' @param point.size The size of the points.
#' @param point.alpha Alpha transparency for the points.
#' @param error.bar logical. If \code{TRUE}, the error bars depicting standard
#'   errors are plotted. Default is \code{TRUE}.
#' @param error.bar.width The width of the error bars.
#'
#' @return The group-wise dumbell plot as a \code{ggplot2} plot grob or as a
#'   list of \code{ggplot2} plot grobs.
#'
#' @importFrom dplyr all_of n summarise
#' @importFrom scales hue_pal
#' @importFrom stats sd reorder
#' @export
#'
#' @examples
#' library(agridat)
#' library(ggplot2)
#' library(patchwork)
#'
#' soydata <- australia.soybean
#' # soydata[soydata$loc == "Nambour", ]$lodging <- NA
#'
#' checks <- c("G01", "G05")
#'
#' checkdata <- soydata[soydata$gen %in% checks, ]
#'
#' clrs <- c("#B2182B", "#2166AC", "#009E53", "#E69F00")
#' clrs_dark <- colorspace::darken(clrs, amount = 0.2)
#'
#' # Group-wise dumbell plot with error bar
#' outg_dumbell1 <-
#'   groupwise_dumbell(data = checkdata, group = "loc",
#'                     trait = "lodging", genotype = "gen",
#'                     subset = "none", diff.sort = "descending")
#' outg_dumbell1
#'
#' outg_dumbell1 +
#'   scale_colour_manual(values = clrs)
#'
#' # Group-wise dumbell plot without error bar
#' outg_dumbell2 <-
#'   groupwise_dumbell(data = checkdata, group = "loc",
#'                     trait = "lodging", genotype = "gen",
#'                     subset = "none", diff.sort = "descending",
#'                     error.bar = FALSE)
#' outg_dumbell2
#'
#' outg_dumbell2 +
#'   scale_colour_manual(values = clrs)
#'
#' # Group-wise points with error bar as facets
#' outg_facet <-
#'   groupwise_dumbell(data = checkdata, group = "loc",
#'                     trait = "lodging", genotype = "gen",
#'                     subset = "facet")
#' outg_facet
#'
#' outg_facet +
#'   scale_colour_manual(values = clrs)
#'
#' # Group-wise points with error bar as list
#' outg_list <-
#'   groupwise_dumbell(data = checkdata, group = "loc",
#'                     trait = "lodging", genotype = "gen",
#'                     subset = "list")
#'
#' wrap_plots(outg_list, nrow = 2, guides = "collect")
#'
#' outg_list <-
#'   lapply(seq_along(outg_list), function(i) {
#'     outg_list[[i]] +
#'       scale_colour_manual(values = clrs[i])
#'   })
#'
#' wrap_plots(outg_list, nrow = 2, guides = "collect")
#'
groupwise_dumbell <- function(data, group, trait, genotype,
                              subset = c("facet", "list", "none"),
                              diff.sort = c("none", "ascending", "descending"),
                              segment = TRUE,
                              segment.size = 3,
                              segment.colour = "gray",
                              segment.alpha = 0.5,
                              point.size = 3,
                              point.alpha = 0.8,
                              error.bar = TRUE,
                              error.bar.width = 0.1,
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

  # Check if genotype column present in data
  if (!(genotype %in% colnames(data))) {
    stop(paste('Column ', genotype,
               ' specified as the genotype column is not present in "data".',
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

  # Check if genotype column is of type factor
  if (class(data[, genotype]) != "factor") {
    stop(paste('Column ', genotype,
               ' in "data" is not of type "factor".',
               sep = ""))
  }

  diff.sort <- match.arg(diff.sort)

  p <- levels(data[, group])
  # p <- sort(levels(data[, group]))
  # data[, group] <- factor(data[, group], p)

  # Generate data summary group and genotype-wise
  data_summ <-
    summarise(.data = data,
              .by = all_of(c(group, genotype)),
              count = n(),
              mean = mean(.data[[trait]], na.rm = TRUE),
              # se = plotrix::std.error(.data[[trait]], na.rm = TRUE),
              se = sd(.data[[trait]], na.rm = TRUE) /
                sqrt(length(.data[[trait]]
                            [!is.na(.data[[trait]])])))

  if (subset == "none" & diff.sort != "none") {
    summ_diffrange <-
      summarise(.data = data_summ,
                .by = all_of(c(genotype)),
                diff = diff(range(mean)))

    data_summ <- merge.data.frame(data_summ, summ_diffrange,
                                  by = genotype, all.x = TRUE)
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Single plot or facet ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (subset != "list") {

    if (subset == "none") {

      if (diff.sort != "none") {

        if (diff.sort == "ascending") {
          outg <-
            ggplot(data = data_summ,
                   aes(y = reorder(.data[[genotype]], diff)))
        }

        if (diff.sort == "descending") {
          outg <-
            ggplot(data = data_summ,
                   aes(y = reorder(.data[[genotype]], rev(diff))))
        }

      } else {

        outg <-
          ggplot(data = data_summ, aes(y = .data[[genotype]]))

      }

      ## Dumbell segment ----
      if (segment == TRUE) {
        outg <- outg +
          geom_line(aes(x = mean, y = .data[[genotype]],
                        group = .data[[genotype]]),
                    colour = segment.colour, size = segment.size,
                    alpha = segment.alpha)
      }

    }

    # Facet ----
    if (subset == "facet") {

      outg <-
        ggplot(data = data_summ, aes(y = .data[[genotype]]))

      outg <- outg +
        facet_wrap(~ .data[[group]], scales = "fixed",
                   nrow = nrow, ncol = ncol)

    }

    ## Error bar ----
    if (error.bar == TRUE) {
      outg <- outg +
        geom_errorbar(aes(y = .data[[genotype]],
                          xmin = mean - se,
                          xmax = mean + se,
                          colour = .data[[group]]),
                      width = error.bar.width)
    }

    ## Point ----
    outg <- outg +
      geom_point(aes(x = mean, colour = .data[[group]]),
                 size = point.size, alpha = point.alpha)

    return(outg +
             xlab(trait) +
             ylab(genotype) +
             theme_bw())

  } else {  # subset ==  list

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # List output ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    colhex <- scales::hue_pal()(length(p))


    data_summ_list <- lapply(seq_along(p), function(i) {
      data_summ[data_summ[, group] == p[i], ]
    })
    names(data_summ_list) <- p

    outg_list <- lapply(seq_along(p), function(i) {
      ggplot(data_summ_list[[i]], aes(y = .data[[genotype]])) +
        facet_wrap(~ .data[[group]])
    })
    names(outg_list) <- p

    ## Error bar ----
    if (error.bar == TRUE) {
      outg_list <- lapply(seq_along(p), function(i) {
        outg_list[[i]] +
          geom_errorbar(aes(y = .data[[genotype]],
                            xmin = mean - se,
                            xmax = mean + se,
                            colour = .data[[group]]),
                        width = error.bar.width) +
          scale_colour_manual(values = colhex[i])
      })
    }

    ## Point ----
    outg_list <- lapply(seq_along(p), function(i) {
      outg_list[[i]] +
        geom_point(aes(x = mean, colour = .data[[group]]),
                   size = point.size, alpha = point.alpha)
    })

    ## Standardize limits ----
    xrange <- unlist(lapply(outg_list, function(x) {
      layer_scales(x)$x$range$range
    }))
    yrange <- unlist(lapply(outg_list, function(x) {
      layer_scales(x)$y$range$range
    }))

    xrange <- setdiff(xrange, c(Inf, -Inf))
    yrange <- setdiff(yrange, c(Inf, -Inf))

    outg_list <- lapply(seq_along(p), function(i) {
      outg_list[[i]] <- outg_list[[i]] +
        xlim(c(min(xrange), max(xrange))) +
        ylim(c(min(yrange), max(yrange)))
    })

    ## Final theme ----
    outg_list <- lapply(seq_along(p), function(i) {
      outg_list[[i]] <- outg_list[[i]] +
        xlab(trait) +
        ylab(genotype) +
        theme_bw()
    })

    return(outg_list)

  }

}

