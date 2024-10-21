#' Plot Group-wise Bar Plots
#'
#' @inheritParams groupwise_histogram
#' @param bar.border logical. If \code{TRUE}, bar border is also plotted.
#'   Default is \code{TRUE}.
#' @param bar.alpha Alpha transparency for the group-wise bar plot.
#' @param by The factor according to which the bars have to be grouped. Either
#'   \code{"group"} or \code{"trait"}.
#' @param relative.freq logical. If \code{TRUE}, the relative frequency or
#'   proportion is plotted instead of counts. Default is \code{FALSE}.
#' @param na.rm logical. If \code{TRUE}, the \code{NA} factor levels are
#'   excluded from the plot. Default is \code{TRUE}.
#' @param include.overall logical. If \code{TRUE}, the overall or total data is
#'   also plotted. Default is \code{TRUE}.
#' @param background.bar logical. If \code{TRUE}, the overall data is plotted as
#'   a background bar plot when \code{by = "group"}, \code{include.overall =
#'   TRUE}, and \code{position = "dodge"}. Default is \code{TRUE}.
#' @param background.bar.alpha Alpha transparency for the background bar plot.
#' @param position Bar position adjustment. Either "dodge" or "stack".
#'
#' @return The group-wise bar plot as a \code{ggplot2} plot grob or as a list of
#'   \code{ggplot2} plot grobs.
#'
#' @import ggplot2
#' @importFrom dplyr all_of arrange mutate n summarise
#' @importFrom scales hue_pal
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
#' clrs <- c("#B2182B", "#2166AC", "#009E53", "#E69F00", "gray25")
#' clrs_dark <- colorspace::darken(clrs, amount = 0.2)
#'
#' soydata$lodging <- cut(soydata$lodging,
#'                        breaks = quantile(soydata$lodging),
#'                        include.lowest = TRUE)
#' levels(soydata$lodging) <- 1:4
#'
#'
#' # Group-wise side-by-side bar plot with counts ----
#'
#' outg_group_dodge_count <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 subset = "none", position = "dodge")
#'
#' outg_group_dodge_count
#'
#' outg_group_dodge_count +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_group_dodge_count_facet <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 subset = "facet", position = "dodge")
#'
#' outg_group_dodge_count_facet
#'
#' outg_group_dodge_count_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_group_dodge_count_list <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 subset = "list", position = "dodge")
#'
#' wrap_plots(outg_group_dodge_count_list, nrow = 2, guides = "collect")
#'
#' outg_group_dodge_count_list <-
#'   lapply(seq_along(outg_group_dodge_count_list), function(i) {
#'     outg_group_dodge_count_list[[i]] +
#'       scale_fill_manual(values = clrs[i]) +
#'       scale_colour_manual(values = clrs_dark[i])
#'   })
#'
#' wrap_plots(outg_group_dodge_count_list, nrow = 2, guides = "collect")
#'
#' # Group-wise side-by-side bar plot with relative frequencies ----
#'
#' outg_group_dodge_rfreq <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 relative.freq = TRUE,
#'                 subset = "none", position = "dodge")
#'
#' outg_group_dodge_rfreq
#'
#' outg_group_dodge_rfreq +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_group_dodge_rfreq_facet <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 relative.freq = TRUE,
#'                 subset = "facet", position = "dodge")
#'
#' outg_group_dodge_rfreq_facet
#'
#' outg_group_dodge_rfreq_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_group_dodge_rfreq_list <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 relative.freq = TRUE,
#'                 subset = "list", position = "dodge")
#'
#' wrap_plots(outg_group_dodge_rfreq_list, nrow = 2, guides = "collect")
#'
#' outg_group_dodge_rfreq_list <-
#'   lapply(seq_along(outg_group_dodge_rfreq_list), function(i) {
#'     outg_group_dodge_rfreq_list[[i]] +
#'       scale_fill_manual(values = clrs[i]) +
#'       scale_colour_manual(values = clrs_dark[i])
#'   })
#'
#' wrap_plots(outg_group_dodge_rfreq_list, nrow = 2, guides = "collect")
#'
#' # Group-wise stacked bar plot with counts ----
#'
#' outg_group_stack_count <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 subset = "none", position = "stack")
#'
#' outg_group_stack_count
#'
#' outg_group_stack_count +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_group_stack_count_facet <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 subset = "facet", position = "stack")
#'
#' outg_group_stack_count_facet
#'
#' outg_group_stack_count_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_group_stack_count_list <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 subset = "list", position = "stack")
#'
#' wrap_plots(outg_group_stack_count_list, nrow = 2, guides = "collect")
#'
#' outg_group_stack_count_list <-
#'   lapply(seq_along(outg_group_stack_count_list), function(i) {
#'     outg_group_stack_count_list[[i]] +
#'       scale_fill_manual(values = clrs) +
#'       scale_colour_manual(values = clrs_dark)
#'   })
#'
#' wrap_plots(outg_group_stack_count_list, nrow = 2, guides = "collect")
#'
#' # Group-wise stacked bar plot with relative frequencies ----
#'
#' outg_group_stack_rfreq <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 relative.freq = TRUE,
#'                 subset = "none", position = "stack")
#'
#' outg_group_stack_rfreq
#'
#' outg_group_stack_rfreq +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_group_stack_rfreq_facet <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 relative.freq = TRUE,
#'                 subset = "facet", position = "stack")
#'
#' outg_group_stack_rfreq_facet
#'
#' outg_group_stack_rfreq_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_group_stack_rfreq_list <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "group",
#'                 relative.freq = TRUE,
#'                 subset = "list", position = "stack")
#'
#' wrap_plots(outg_group_stack_rfreq_list, nrow = 2, guides = "collect")
#'
#' outg_group_stack_rfreq_list <-
#'   lapply(seq_along(outg_group_stack_rfreq_list), function(i) {
#'     outg_group_stack_rfreq_list[[i]] +
#'       scale_fill_manual(values = clrs) +
#'       scale_colour_manual(values = clrs_dark)
#'   })
#'
#' wrap_plots(outg_group_stack_count_list, nrow = 2, guides = "collect")
#'
#' # Trait-wise side-by-side bar plot with counts ----
#'
#' # Trait-
#' #   _trait_
#' #   by = "trait"
#'
#' # Trait-wise side-by-side bar plot with counts ----
#'
#' outg_trait_dodge_count <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 subset = "none", position = "dodge")
#'
#' outg_trait_dodge_count
#'
#' outg_trait_dodge_count +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_trait_dodge_count_facet <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 subset = "facet", position = "dodge")
#'
#' outg_trait_dodge_count_facet
#'
#' outg_trait_dodge_count_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_trait_dodge_count_list <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 subset = "list", position = "dodge")
#'
#' wrap_plots(outg_trait_dodge_count_list, nrow = 2, guides = "collect")
#'
#' outg_trait_dodge_count_list <-
#'   lapply(seq_along(outg_trait_dodge_count_list), function(i) {
#'     outg_trait_dodge_count_list[[i]] +
#'       scale_fill_manual(values = clrs[i]) +
#'       scale_colour_manual(values = clrs_dark[i])
#'   })
#'
#' wrap_plots(outg_trait_dodge_count_list, nrow = 2, guides = "collect")
#'
#' # Trait-wise side-by-side bar plot with relative frequencies ----
#'
#' outg_trait_dodge_rfreq <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 relative.freq = TRUE,
#'                 subset = "none", position = "dodge")
#'
#' outg_trait_dodge_rfreq
#'
#' outg_trait_dodge_rfreq +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_trait_dodge_rfreq_facet <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 relative.freq = TRUE,
#'                 subset = "facet", position = "dodge")
#'
#' outg_trait_dodge_rfreq_facet
#'
#' outg_trait_dodge_rfreq_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_trait_dodge_rfreq_list <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 relative.freq = TRUE,
#'                 subset = "list", position = "dodge")
#'
#' wrap_plots(outg_trait_dodge_rfreq_list, nrow = 2, guides = "collect")
#'
#' outg_trait_dodge_rfreq_list <-
#'   lapply(seq_along(outg_trait_dodge_rfreq_list), function(i) {
#'     outg_trait_dodge_rfreq_list[[i]] +
#'       scale_fill_manual(values = clrs[i]) +
#'       scale_colour_manual(values = clrs_dark[i])
#'   })
#'
#' wrap_plots(outg_trait_dodge_rfreq_list, nrow = 2, guides = "collect")
#'
#' # Trait-wise stacked bar plot with counts ----
#'
#' outg_trait_stack_count <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 subset = "none", position = "stack")
#'
#' outg_trait_stack_count
#'
#' outg_trait_stack_count +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_trait_stack_count_facet <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 subset = "facet", position = "stack")
#'
#' outg_trait_stack_count_facet
#'
#' outg_trait_stack_count_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_trait_stack_count_list <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 subset = "list", position = "stack")
#'
#' wrap_plots(outg_trait_stack_count_list, nrow = 2, guides = "collect")
#'
#' outg_trait_stack_count_list <-
#'   lapply(seq_along(outg_trait_stack_count_list), function(i) {
#'     outg_trait_stack_count_list[[i]] +
#'       scale_fill_manual(values = clrs) +
#'       scale_colour_manual(values = clrs_dark)
#'   })
#'
#' wrap_plots(outg_trait_stack_count_list, nrow = 2, guides = "collect")
#'
#' # Trait-wise stacked bar plot with relative frequencies ----
#'
#' outg_trait_stack_rfreq <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 relative.freq = TRUE,
#'                 subset = "none", position = "stack")
#'
#' outg_trait_stack_rfreq
#'
#' outg_trait_stack_rfreq +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_trait_stack_rfreq_facet <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 relative.freq = TRUE,
#'                 subset = "facet", position = "stack")
#'
#' outg_trait_stack_rfreq_facet
#'
#' outg_trait_stack_rfreq_facet +
#'   scale_fill_manual(values = clrs) +
#'   scale_colour_manual(values = clrs_dark)
#'
#' outg_trait_stack_rfreq_list <-
#'   groupwise_bar(data = soydata, group = "loc", trait = "lodging",
#'                 bar.border = TRUE, by = "trait",
#'                 relative.freq = TRUE,
#'                 subset = "list", position = "stack")
#'
#' wrap_plots(outg_trait_stack_rfreq_list, nrow = 2, guides = "collect")
#'
#' outg_trait_stack_rfreq_list <-
#'   lapply(seq_along(outg_trait_stack_rfreq_list), function(i) {
#'     outg_trait_stack_rfreq_list[[i]] +
#'       scale_fill_manual(values = clrs) +
#'       scale_colour_manual(values = clrs_dark)
#'   })
#'
#' wrap_plots(outg_trait_stack_count_list, nrow = 2, guides = "collect")
#'
groupwise_bar <- function(data, group, trait,
                          bar.border = TRUE,
                          bar.alpha = 0.8,
                          by = c("group", "trait"),
                          relative.freq = FALSE,
                          subset = c("facet", "none"),
                          na.rm = TRUE,
                          include.overall = TRUE,
                          background.bar = TRUE,
                          background.bar.alpha = 0.25,
                          show.counts = TRUE,
                          count.text.size = 3,
                          position = c("dodge", "stack"),
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

  # Check if trait column is of type factor
  if (class(data[, trait]) != "factor") {
    stop(paste('Column ', trait,
               ' in "data" is not of type "factor".',
               sep = ""))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data prep ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  p <- levels(data[, group])

  ## Add overall data ----
  if (include.overall == TRUE) {
    data_total <- data
    nlevels <- length(p)
    levels(data_total[, group]) <- rep("Total", nlevels)

    if (!(by == "group" & position == "dodge" & background.bar == TRUE &
          subset == "none")) {
      data <- rbind(data, data_total)
    }

    p <- levels(data[, group])
  }

  ## Remove na data ----
  if (na.rm) {
    na_ind <- is.na(data[, trait])
    data <- data[!na_ind, ]
  }

  ## Summary data.frame ----
  if (show.counts == TRUE) {

    total_ind <- data[, trait] != "Total"
    data_summ <- summarise(.data = data[total_ind, ],
                           .by = all_of(c(group)),
                           count = n())
    data_summ <- dplyr::arrange(.data = data_summ, .by = .data[[group]])

    if (subset != "none" & by == "trait") {
      data_summ2 <-
        summarise(.data = data[total_ind, ],
                  .by = all_of(c(group, trait)),
                  count = n())
    }

    if (by == "group" & position == "stack") {
      data <- mutate(data, .by = all_of(c(group)),
                     {{group}} := paste0(.data[[group]], ' (n = ', n(), ')'))
      data[, group] <- as.factor(data[, group])

      p <- levels(data[, group])

    }

  }

  ## Prepare aesthetics according to by and bar.border ----
  if (by == "group") {
    gp_aes <- aes(x = .data[[trait]], fill = .data[[group]])
    # Prepare aesthetics according to bar.border
    if (bar.border == TRUE) {
      gp_aes <- modifyList(gp_aes, aes(colour = .data[[group]]))
    }
  }
  if (by == "trait") {
    gp_aes <- aes(x = .data[[group]], fill = .data[[trait]])
    # Prepare aesthetics according to bar.border
    if (bar.border == TRUE) {
      gp_aes <- modifyList(gp_aes, aes(colour = .data[[trait]]))
    }
  }

  ## Adjust aes according to by and position for relative freq ----
  if (relative.freq == TRUE) {

    if (by == "group") {

      if (position == "dodge") {
        gp_aes <- modifyList(gp_aes, aes(y = after_stat(prop),
                                         group = .data[[group]]))
      }
      if (position == "stack") {
        gp_aes <- modifyList(gp_aes, aes(#y = after_stat(prop),
          group = .data[[group]]))
      }

    }

    if (by == "trait") {

      if (position == "dodge") {
        gp_aes <- modifyList(gp_aes, aes(y = after_stat(prop),
                                         group = .data[[trait]]))
      }
      if (position == "stack") {
        gp_aes <- modifyList(gp_aes, aes(#y = after_stat(prop),
          group = .data[[trait]]))
      }

    }

    if (relative.freq && position == "stack") {
      position = "fill"
    }

  }

  if (by == "group" & position == "dodge" & background.bar == TRUE &
      subset == "none") {

    if (relative.freq == TRUE) {
      gp_aes_bg <- aes(x = .data[[trait]], y = after_stat(prop),
                       group = .data[[group]])
    } else {
      gp_aes_bg <- aes(x = .data[[trait]])
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Single plot or facet ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (subset != "list") {

    outg <-  ggplot(data = data)

    if (by == "group" & position == "dodge" & background.bar == TRUE &
        subset == "none") {

      outg <- outg +
        geom_bar(data = data_total, mapping = gp_aes_bg,
                 position = position, alpha = 0.1, fill = "black") +
        geom_bar(mapping = gp_aes, position = position, alpha = bar.alpha)

    } else {

      outg <- outg +
        geom_bar(mapping = gp_aes, position = position, alpha = bar.alpha)

    }

    if (subset == "facet") {

      if (by == "group") {
        if (position == "dodge") {
          outg <- outg +
            facet_wrap(~ .data[[group]], scales = "fixed",
                       nrow = nrow, ncol = ncol)
        }
        if (position == "stack" | position == "fill") {
          outg <- outg +
            facet_wrap(~ .data[[trait]], scales = "free_x",
                       nrow = nrow, ncol = ncol)
        }
      }
      if (by == "trait") {
        if (position == "dodge") {
          outg <- outg +
            facet_wrap(~ .data[[trait]], scales = "fixed",
                       nrow = nrow, ncol = ncol)
        }
        if (position == "stack" | position == "fill") {
          outg <- outg +
            facet_wrap(~ .data[[group]], scales = "free_x",
                       nrow = nrow, ncol = ncol)
        }
      }

    }

    ## Show counts ----
    if (show.counts == TRUE) {

      if (by == "group") {

        if (position == "dodge") {

          vjust_custom <- 1.5

          if (subset == "none") {
            vjust_custom <- (seq_along(p) * 2) + (count.text.size)
          }

          outg <-
            outg +
            geom_text(data = data_summ, aes(x = Inf, y = Inf,
                                            vjust = vjust_custom, hjust = 1.5,
                                            colour = .data[[group]],
                                            label = paste("n =", count)),
                      size = count.text.size) +
            # expand_limits(x = c(1, length(levels(data[, trait])) + 1)) +
            scale_x_discrete(expand = expansion(add = c(0.6, 1)))

        }

      }

      if (by == "trait") {
        # yscale_min <- layer_scales(outg)$y$range$range[1]
        # yscale_max <- layer_scales(outg)$y$range$range[2]
        # yscale_max <- yscale_max + (0.1 * yscale_max)

        if (subset == "none") {

          outg <-
            outg +
            geom_text(data = data_summ, aes(x = .data[[group]], y = Inf,
                                            vjust = 1.5,
                                            #hjust = 1.5,
                                            # colour = .data[[group]],
                                            label = paste("n =", count)),
                      size = count.text.size) +
            # expand_limits(y = c(0, yscale_max)) +
            scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
        }

        if (subset == "facet") {

          if (position == "dodge") {
            outg <-
              outg +
              geom_text(data = data_summ2, aes(x = .data[[group]], y = Inf,
                                               vjust = 1.5,
                                               #hjust = 1.5,
                                               colour = .data[[trait]],
                                               label = paste("n =", count)),
                        size = count.text.size) +
              # expand_limits(y = c(0, yscale_max)) +
              scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
          }

          if (position == "stack" | position == "fill") {
            outg <-
              outg +
              geom_text(data = data_summ, aes(x = .data[[group]], y = Inf,
                                               vjust = 1.5,
                                               #hjust = 1.5,
                                               # colour = .data[[trait]],
                                               label = paste("n =", count)),
                        size = count.text.size) +
              # expand_limits(y = c(0, yscale_max)) +
              scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
          }

        }
      }

    }

    return(outg +
             xlab(trait) +
             ylab(ifelse(relative.freq, "Proportion", "Count")) +
             theme_bw())

  } else { # subset ==  list

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # List output ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (by == "group") {
      if (position == "dodge") {

        p <- levels(data[, group])

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

          ggplot(data = gpdata_list[[i]]) +
            geom_bar(mapping = gp_aes,
                     position = position, alpha = bar.alpha) +
            scale_fill_manual(values = colhex[i]) +
            scale_colour_manual(values = colhex[i])

        })
        names(outg_list) <- p

        outg_list <- lapply(seq_along(p), function(i) {
          outg_list[[i]] +
            facet_wrap(~ .data[[group]], scales = "fixed",
                       nrow = nrow, ncol = ncol)
        })

      }
      if (position == "stack" | position == "fill") {

        p <- levels(data[, trait])

        # colhex <- scales::hue_pal()(length(p))

        gpdata_list <- lapply(seq_along(p), function(i) {
          data[data[, trait] == p[i], ]
        })
        names(gpdata_list) <- p

        outg_list <- lapply(seq_along(p), function(i) {

          ggplot(data = gpdata_list[[i]]) +
            geom_bar(mapping = gp_aes,
                     position = position, alpha = bar.alpha)

        })
        names(outg_list) <- p

        outg_list <- lapply(seq_along(p), function(i) {
          outg_list[[i]] +
            facet_wrap(~ .data[[trait]], scales = "free_x",
                       nrow = nrow, ncol = ncol)
        })

      }
    }
    if (by == "trait") {
      if (position == "dodge") {

        p <- levels(data[, trait])

        colhex <- scales::hue_pal()(length(p))

        gpdata_list <- lapply(seq_along(p), function(i) {
          data[data[, trait] == p[i], ]
        })
        names(gpdata_list) <- p

        # gpdata_summ_list <- lapply(seq_along(p), function(i) {
        #   data_summ[data_summ[, group] == p[i], ]
        # })
        # names(gpdata_summ_list) <- p

        gpdata_summ2_list <- lapply(seq_along(p), function(i) {
          data_summ2[data_summ2[, trait] == p[i], ]
        })
        names(gpdata_summ2_list) <- p

        outg_list <- lapply(seq_along(p), function(i) {

          ggplot(data = gpdata_list[[i]]) +
            geom_bar(mapping = gp_aes,
                     position = position, alpha = bar.alpha) +
            scale_fill_manual(values = colhex[i]) +
            scale_colour_manual(values = colhex[i])

        })
        names(outg_list) <- p

        outg_list <- lapply(seq_along(p), function(i) {
          outg_list[[i]] +
            facet_wrap(~ .data[[trait]], scales = "fixed",
                       nrow = nrow, ncol = ncol)
        })

      }
      if (position == "stack" | position == "fill") {

        p <- levels(data[, group])

        colhex <- scales::hue_pal()(length(p))

        gpdata_list <- lapply(seq_along(p), function(i) {
          data[data[, group] == p[i], ]
        })
        names(gpdata_list) <- p

        gpdata_summ_list <- lapply(seq_along(p), function(i) {
          data_summ[data_summ[, group] == p[i], ]
        })
        names(gpdata_summ_list) <- p

        # gpdata_summ2_list <- lapply(seq_along(p), function(i) {
        #   data_summ2[data_summ2[, trait] == p[i], ]
        # })
        # names(gpdata_summ2_list) <- p

        outg_list <- lapply(seq_along(p), function(i) {

          ggplot(data = gpdata_list[[i]]) +
            geom_bar(mapping = gp_aes,
                     position = position, alpha = bar.alpha)

        })
        names(outg_list) <- p

        outg_list <- lapply(seq_along(p), function(i) {
          outg_list[[i]] +
            facet_wrap(~ .data[[group]], scales = "free_x",
                       nrow = nrow, ncol = ncol)
        })
      }
    }

    ## Show counts ----
    if (show.counts == TRUE) {

      if (by == "group") {

        if (position == "dodge") {

          vjust_custom <- 1.5

          if (subset == "none") {
            vjust_custom <- (seq_along(p) * 2) + (count.text.size)
          }

          outg_list <- lapply(seq_along(p), function(i) {
            outg_list[[i]] +
              geom_text(data = gpdata_summ_list[[i]],
                        aes(x = Inf, y = Inf,
                            vjust = vjust_custom, hjust = 1.5,
                            colour = .data[[group]],
                            label = paste("n =", count)),
                        size = count.text.size) +
              # expand_limits(x = c(1, length(levels(data[, trait])) + 1)) +
              scale_x_discrete(expand = expansion(add = c(0.6, 1)))
          })

        }
      }

      if (by == "trait") {


        # yscale_min <- layer_scales(outg)$y$range$range[1]
        # yscale_max <- layer_scales(outg)$y$range$range[2]
        # yscale_max <- yscale_max + (0.1 * yscale_max)

        if (position == "dodge") {

          outg_list <- lapply(seq_along(p), function(i) {
            outg_list[[i]] +
              geom_text(data = gpdata_summ2_list[[i]],
                        aes(x = .data[[group]], y = Inf,
                            vjust = 1.5,
                            #hjust = 1.5,
                            colour = .data[[trait]],
                            label = paste("n =", count)),
                        size = count.text.size) +
              # expand_limits(y = c(0, yscale_max)) +
              scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
          })

        }

        if (position == "stack" | position == "fill") {

          outg_list <- lapply(seq_along(p), function(i) {
            outg_list[[i]] +
              geom_text(data = gpdata_summ_list[[i]],
                        aes(x = .data[[group]], y = Inf,
                            vjust = 1.5,
                            #hjust = 1.5,
                            # colour = .data[[trait]],
                            label = paste("n =", count)),
                        size = count.text.size) +
              # expand_limits(y = c(0, yscale_max)) +
              scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
          })

        }

      }
    }

    ## Standardize limits ----
    yrange <- unlist(lapply(outg_list, function(x) {
      layer_scales(x)$y$range$range
    }))

    yexpand <- unlist(lapply(outg_list, function(x) {
      layer_scales(x)$y$expand[3]
    }))

    ### Remove y scale ----
    outg_list <- lapply(seq_along(p), function(i) {

      remove_scales(outg_list[[i]], scales = "y")

    })

    if (is.null(yexpand)) {

      outg_list <- lapply(seq_along(p), function(i) {
        outg_list[[i]] <- outg_list[[i]] +
          scale_y_continuous(limits = c(min(yrange), max(yrange)))

      })

    } else {

      yexpand <- max(yexpand)

      outg_list <- lapply(seq_along(p), function(i) {
        outg_list[[i]] <- outg_list[[i]] +
          scale_y_continuous(limits = c(min(yrange), max(yrange)),
                             expand = expansion(mult = c(0.05, yexpand)))

      })

    }

    ## Final theme ----
    outg_list <- lapply(seq_along(p), function(i) {
      outg_list[[i]] <- outg_list[[i]] +
        xlab(trait) +
        ylab(ifelse(relative.freq, "Proportion", "Count")) +
        theme_bw()
    })

    return(outg_list)


  }

}
