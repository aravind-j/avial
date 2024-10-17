#' Remove scales from \code{ggplot} objects
#'
#' Useful for avoiding the warning \code{Scale for * is already present. Adding
#' another scale for *, which will replace the existing scale.}
#'
#' @param g A \code{ggplot} object.
#' @param scales The scales to be removed as a character vector.
#'
#' @return A \code{ggplot} object without the scales specified.
#' @export
#'
remove_scales <- function(g, scales) {

  if (length(g$scales$scales) > 0) {
    g_scale_ind <- unlist(lapply(g$scales$scales, function(x) {
      x$aesthetics %in% scales
    }))

    if (any(g_scale_ind)) {
      g$scales$scales[which(g_scale_ind)] <- NULL
    }

  }
  return(g)
}
