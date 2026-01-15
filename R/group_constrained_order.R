
#' Ordering by keeping defined groups together
#'
#' This function modifies the order of a vector so that specified groups of
#' elements remain together. This is useful for keeping specific dendrogram
#' leaves together. The location of each group block is determined by the
#' earliest member of that group in original vector. For elements not involved
#' in any group, their relative order is unchanged.
#'
#' @param x The original vector of elements in order.
#' @param groups The groups of elements that are to be kept together as a list
#'   of vectors. The groups must be disjoint.
#' @param group_order The group internal order should follow the one in
#'   \code{"groups"} or the \code{"original"} order.
#'
#' @returns A vector of the original elements where the specified groups of
#'   elements are kept together.
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' # data
#' mat <- matrix(rnorm(8 * 4), nrow = 8)
#' rownames(mat) <- LETTERS[1:8]
#'
#' # hierarchical clustering
#' hc <- hclust(dist(mat))
#' plot(hc, main = "Original dendrogram")
#'
#' # groups of leaves to keep together (may not be adjacent in dendrogram)
#' groups <- list(
#'   g1 = c("B", "F"),
#'   g2 = c("C", "D", "E")
#' )
#'
#' # Dendrogram original order
#' orig_order <- hc$labels[hc$order]
#' orig_order
#'
#' # Get group constrained order
#' new_order <-
#'   group_constrained_order(x = orig_order,
#'                           groups = groups,
#'                           group_order = "original")
#'
#' new_order
#'
#' # Reorder dendrogram labels
#' hc2 <- hc
#' hc2$order <- match(new_order, hc$labels)
#'
#' plot(hc2, main = "Dendrogram with grouped leaves")
#'
group_constrained_order <- function(x,
                                    groups,
                                    group_order = c("groups", "original")) {

  group_order <- match.arg(group_order)

  ## Checks ----

  stopifnot(is.character(x))
  stopifnot(is.list(groups))
  stopifnot(all(vapply(groups, is.character, logical(1))))

  all_group_membs <- unlist(groups, use.names = FALSE)

  # group members must exist in x
  missing_membs <- setdiff(all_group_membs, x)
  if (length(missing_membs) > 0) {
    stop(
      'The following group members in "groups" are not present in "x":\n',
      paste(missing_membs, collapse = ", ")
    )
  }

  # groups must be disjoint
  dup_membs <- all_group_membs[duplicated(all_group_membs)]
  if (length(dup_membs) > 0) {
    stop(
      'The following elements appear in more than one group in "groups":\n',
      paste(unique(dup_membs), collapse = ", ")
    )
  }

  ## Position lookup ----

  pos <- seq_along(x)
  names(pos) <- x

  ## Build group blocks ----

  group_blocks <- lapply(groups, function(g) {
    if (group_order == "original") {
      g[order(pos[g])]
    } else {
      g
    }
  })

  group_start_pos <- vapply(group_blocks, function(g) {
    min(pos[g])
  }, numeric(1))

  ## Ungrouped elements ----


  ungrouped <- setdiff(x, all_group_membs)

  ungrouped_blocks <- as.list(ungrouped)
  ungrouped_start_pos <- pos[ungrouped]

  ## Combine and order blocks ----

  all_blocks <- c(group_blocks, ungrouped_blocks)
  all_start_pos <- c(group_start_pos, ungrouped_start_pos)

  ordered_blocks <- all_blocks[order(all_start_pos)]
  unlist(ordered_blocks, use.names = FALSE)
}



