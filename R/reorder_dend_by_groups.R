
#' Reorder a dendrogram to keep specified groups of tips adjacent
#'
#' This function reorders the tips (leaves) of a dendrogram so that elements
#' belonging to predefined groups appear together. This is useful in
#' hierarchical clustering visualization where it is desired to have clusters or
#' groups to appear contiguously.
#'
#' @param dend An object of class \code{dendrogram}.
#' @param groups_list A list of character vectors. Each vector contains the tip
#'   labels that should be kept adjacent. e.g., list(g1=c("a","b"),
#'   g2=c("c","d")).
#'
#' @importFrom dendextend rotate
#' @export
#'
#' @return A reordered \code{dendrogram} object.
#'
#' @examples
#' set.seed(42)
#'
#' mat <- matrix(rnorm(12*3), nrow = 12)
#' rownames(mat) <- paste0("T", 1:12)
#'
#' # Hierarchical clustering
#' hc <- hclust(dist(mat))
#' dend <- as.dendrogram(hc)
#'
#' # Plot
#' plot(dend, main = "Original dendrogram")
#'
#'
#' # Groups that align with the dendrogram structure
#' #'
#' groups_preserved <- list(
#'   GroupA = c("T1", "T2", "T3"),
#'   GroupB = c("T4", "T5", "T6"),
#'   GroupC = c("T7", "T8")
#' )
#'
#' dend_preserved <- reorder_dend_by_groups(dend, groups_preserved)
#'
#' plot(dend_preserved, main = "Groups preserved (no warning)")
#'
#' # Define groups that do NOT align with dendrogram
#' # These tips are far apart in the dendrogram
#' groups_split <- list(
#'   GroupX = c("T1", "T5", "T9"),
#'   GroupY = c("T2", "T6", "T10")
#' )
#'
#' dend_split <- reorder_dend_by_groups(dend, groups_split)
#'
#' # Plot
#' plot(dend_split, main = "Groups split (warning expected)")
#'
reorder_dend_by_groups <- function(dend, groups_list) {

  tip_labels <- labels(dend)

  # Checks ----

  # Check each group has at least 2 tips
  for (gname in names(groups_list)) {
    if (length(groups_list[[gname]]) < 2) {
      stop(sprintf("Group '%s' has less than 2 tips. Skipping this group.",
                   gname))
      groups_list[[gname]] <- NULL
    }
  }

  # Check for overlapping groups
  all_tips_in_groups <- unlist(groups_list)
  if (any(duplicated(all_tips_in_groups))) {
    stop("Groups have overlapping tips. Please ensure groups are disjoint.")
  }

  # Check that all group elements exist in dendrogram
  for (gname in names(groups_list)) {
    missing_tips <- setdiff(groups_list[[gname]], tip_labels)
    if (length(missing_tips) > 0) {
      stop(sprintf("Group '%s' contains tips not in dendrogram: %s",
                   gname, paste(missing_tips, collapse = ", ")))
    }
  }


  # Ideal target order ----
  tip_labels_new <-
    group_constrained_order(x = tip_labels, groups = groups_list)

  # Rotate the dendrogram to match the target as closely as possible ----
  # rotate() will ignore orders that break the hierarchy/topology
  dend_new <- dendextend::rotate(x = dend, order = tip_labels_new)

  # Post Execution Checks ----

  if (!identical(labels(dend_new), tip_labels_new)) {
    split_groups <- list()
    for (gname in names(groups_list)) {
      group_tips <- groups_list[[gname]]
      positions <- which(labels(dend_new) %in% group_tips)
      # Check if positions are contiguous
      if (length(positions) > 1 && any(diff(positions) != 1)) {
        split_groups[[gname]] <- group_tips
      }
    }
    if (length(split_groups) > 0) {
      split_str <- sapply(split_groups, function(x) paste(x, collapse = ", "))
      warning(sprintf(
        "The following groups could not be kept together due to dendrogram topology constraints:\n%s",
        paste(split_str, collapse = "\n")
      ))
    }
  }

  return(dend_new)

}
