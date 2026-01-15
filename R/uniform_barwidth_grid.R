
#' Plot a List of \code{ggplot2} Barplots as a Grid with with Uniform Width of
#' the Bars
#'
#' @param plot.list The list of bar plots as \code{ggplot2} objects.
#' @param level.count The count of levels/bars in each plot.
#' @param nrow Number of rows of the grid.
#' @param ncol Number of columns of the grid.
#'
#' @returns The plot grid as a \code{patchwork} object.
#'
#' @importFrom patchwork wrap_plots plot_spacer
#' @export
#'
#' @examples
#' # Load ggplot2
#' library(ggplot2)
#' library(patchwork)
#'
#' # Barplot 1 ----
#' data1 <- data.frame(
#'   category = c("A", "B", "C"),
#'   value = c(10, 15, 8)
#' )
#'
#' plot1 <- ggplot(data1, aes(x = category, y = value, fill = category)) +
#'   geom_bar(stat = "identity") +
#'   ggtitle("Plot 1: 3 bars") +
#'   theme_minimal()
#'
#' # Barplot 2 ----
#' data2 <- data.frame(
#'   category = c("W", "X", "Y", "Z", "V"),
#'   value = c(12, 9, 20, 7, 14)
#' )
#'
#' plot2 <- ggplot(data2, aes(x = category, y = value, fill = category)) +
#'   geom_bar(stat = "identity") +
#'   ggtitle("Plot 2: 5 bars") +
#'   theme_minimal()
#'
#' # Barplot 3 ----
#' data3 <- data.frame(
#'   category = c("P", "Q", "R", "S"),
#'   value = c(5, 18, 11, 9)
#' )
#'
#' plot3 <- ggplot(data3, aes(x = category, y = value, fill = category)) +
#'   geom_bar(stat = "identity") +
#'   ggtitle("Plot 3: 4 bars") +
#'   theme_minimal()
#'
#' # Plot originals with patchwork
#' wrap_plots(plot1, plot2, plot3, nrow = 2)
#'
#' # Plot to get uniform bar widths
#' uniform_barwidth_grid(list(plot1, plot2, plot3),
#'                       level.count = c(3, 5, 4), nrow = 2, ncol = 2)
#'
uniform_barwidth_grid <- function(plot.list, level.count, nrow, ncol) {

  # Check if the plot list fits the grid
  if (length(level.count) > nrow * ncol) {
    stop("Length of level.count is too long for the specified grid (nrow * ncol).")
  }

  if (nrow == 1) {

    # Relative widths
    rel_width <- level.count
    wrap_out <- wrap_plots(plot.list, nrow = nrow, ncol = ncol,
                          widths = rel_width)

  }

  if (nrow > 1) {

    # Row-wise list of relative widths
    rel_width <- split(level.count, 1:nrow)

    # Row widths
    rows_width <- unlist(lapply(rel_width, sum))

    # Max row width
    rows_width_max <- max(rows_width)

    # Width of spacers
    pad_width <- rows_width_max - rows_width

    # Split plot list by row
    plot_list_split <- split(plot.list, 1:nrow)

    # Wrap each row
    plotgrid_rows <- vector("list", nrow)

    for (i in 1:nrow) {
      if (pad_width[i] > 0) {
        plotgrid_rows[[i]] <-
          wrap_plots(plot_list_split[[i]], plot_spacer(),
                     widths = c(rel_width[[i]], pad_width[[i]]),
                     nrow = 1)
      } else {
        plotgrid_rows[[i]] <-
          wrap_plots(plot_list_split[[i]],
                     widths = rel_width[[i]], nrow = 1)
      }
    }

    wrap_out <- wrap_plots(plotgrid_rows, ncol = 1)

  }

  return(wrap_out)


}







