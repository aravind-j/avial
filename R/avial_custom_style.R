#' Custom tidyverse style guide
#'
#' Creates a modified version of [styler::tidyverse_style()] with custom
#' line-breaking and argument-packing rules.
#'
#' This style guide retains the standard tidyverse spacing, indentation,
#' token, and line-breaking conventions except for the following
#' modifications:
#'
#' \itemize{
#'   \item Function arguments and vector elements are packed onto lines
#'     subject to an 80-character width limit.
#'   \item Function calls do not introduce a line break immediately after
#'     the opening parenthesis or immediately before the closing parenthesis.
#'   \item Arguments that would otherwise be formatted one per line are
#'     grouped onto the same line when they fit within the width limit.
#'   \item The right-hand side of an assignment is moved to a new line when
#'     it is a function call.
#'   \item The base `function()` syntax is retained; the shorthand lambda
#'     syntax is not introduced.
#' }
#'
#' The resulting style guide can be supplied to styler functions through
#' the `transformers` argument or configured for the styler RStudio Addin.
#'
#' @returns
#' A styler style-guide object, i.e. a named list of transformer functions
#' suitable for use with [styler::style_text()], [styler::style_file()],
#' [styler::style_dir()], and related styler functions.
#'
#' @importFrom dplyr lead
#' @importFrom styler is_function_call tidyverse_style
#' @importFrom utils getFromNamespace
#'
#' @examples
#' if (requireNamespace("styler", quietly = TRUE)) {
#'   style <- avial_custom_style()
#'
#'   styler::style_text(
#'     "result <- some_function(first_argument = value_one,
#'                              second_argument = value_two)",
#'     transformers = style
#'   )
#' }
#'
#' @export
avial_custom_style <- function() {
  td <- styler::tidyverse_style()

  # Remove the default styler transformers that force multi-line breaks
  td$line_break$set_line_break_after_opening_if_call_is_multi_line <- NULL
  td$line_break$set_line_break_before_closing_call <- NULL

  td$line_break$keep_call_delimiters_together <- function(pd) {
    if (!styler::is_function_call(pd) && !is_subset_expr(pd)) {
      return(pd)
    }

    opening <- which(pd$token %in% c("'('", "'['", "LBB"))
    closing <- which(pd$token %in% c("')'", "']'"))

    # Set lag_newlines to 0 immediately after opening delimiters
    if (length(opening)) {
      pd$lag_newlines[opening + 1L] <- 0L
    }

    # Set lag_newlines to 0 for closing delimiters themselves
    if (length(closing)) {
      pd$lag_newlines[closing] <- 0L
    }

    pd
  }

  td$line_break$pack_call_arguments <- pack_call_arguments_mod
  td$line_break$set_line_break_after_assignment <-
    set_line_break_after_assignment_mod

  td
}

is_subset_expr <- utils::getFromNamespace("is_subset_expr", "styler")

# options(styler.addins_style_transformer = "avial_custom_style")

#' Pack function-call arguments and vector elements to the configured width.
#'
#' Internal transformer used by [avial_custom_style()].
#'
#' @param pd A styler parse-data object.
#'
#' @returns A modified styler parse-data object.
#'
#' @noRd
pack_call_arguments_mod <- function(pd) {
  if (!styler::is_function_call(pd) && !is_subset_expr(pd)) {
    return(pd)
  }

  if (nrow(pd) < 4L || any(pd$token == "COMMENT")) {
    return(pd)
  }

  opening_idx <- which(pd$token %in% c("'('", "'['", "LBB"))[1L]
  closing_idx <- min(which(pd$token %in% c("')'", "']'")))

  if (is.na(opening_idx) || !length(closing_idx)) {
    return(pd)
  }

  # Reset all internal newlines inside the call delimiters
  if (closing_idx > opening_idx + 1L) {
    pd$lag_newlines[(opening_idx + 1L):closing_idx] <- 0L
  }

  # Calculate depth to isolate top-level commas
  n <- nrow(pd)
  depth <- integer(n)
  current_depth <- 0L

  for (i in seq_len(n)) {
    if (pd$token[i] %in% c("'('", "'['", "LBB")) {
      current_depth <- current_depth + 1L
    }
    depth[i] <- current_depth
    if (pd$token[i] %in% c("')'", "']'")) {
      current_depth <- max(0L, current_depth - 1L)
    }
  }

  comma_idx <- which(pd$token == "','")
  # Filter commas matching the nesting depth of the call
  comma_idx <- comma_idx[depth[comma_idx] == depth[opening_idx]]

  # Argument start and end boundaries
  starts <- c(opening_idx + 1L, comma_idx + 1L)
  ends <- if (length(comma_idx)) {
    c(comma_idx - 1L, closing_idx - 1L)
  } else {
    closing_idx - 1L
  }
  keep <- starts <= ends
  starts <- starts[keep]
  ends <- ends[keep]

  if (!length(starts)) {
    return(pd)
  }

  # Calculate the width of tokens, accounting for inter-token spacing
  token_width <- function(from, to) {
    if (from > to) return(0L)
    idx <- seq.int(from, to)
    width <- sum(nchar(pd$text[idx], type = "width"))

    # Add spacing between tokens
    # (except after opening delimiters or before closing)
    if (length(idx) > 1L) {
      for (j in 2:length(idx)) {
        prev_tok <- pd$token[idx[j - 1L]]
        curr_tok <- pd$token[idx[j]]
        # Don't add space after opening delimiters or
        # before closing delimiters or commas
        if (!(prev_tok %in% c("'('", "'['", "LBB")) &&
            !(curr_tok %in% c("')'", "']'", "','")) &&
            pd$spaces[idx[j]] > 0L) {
          width <- width + 1L
        }
      }
    }
    width
  }

  # Calculate the prefix width
  # (everything up to and including the opening delimiter)
  prefix_width <- sum(nchar(pd$text[seq_len(opening_idx)], type = "width"))
  # Account for indentation on the first line
  current_width <- pd$indent[1L] + prefix_width

  for (i in seq_along(starts)) {
    arg_width <- token_width(starts[i], ends[i])
    sep_width <- if (i > 1L) 2L else 0L # ", " is 2 characters

    # Check if argument fits on current line
    if (current_width + sep_width + arg_width <= 80L) {
      pd$lag_newlines[starts[i]] <- 0L
      pd$spaces[starts[i]] <- if (i > 1L) 1L else 0L
      current_width <- current_width + sep_width + arg_width
    } else {
      # Move to new line: indent + argument
      pd$lag_newlines[starts[i]] <- 1L
      pd$spaces[starts[i]] <- 0L
      current_width <- pd$indent[1L] + 2L + arg_width
    }
  }

  # No line break before closing delimiter
  pd$lag_newlines[closing_idx] <- 0L
  pd
}

#' Move function-call right-hand sides after assignments to a new line.
#'
#' Internal transformer used by [avial_custom_style()].
#'
#' @param pd A styler parse-data object.
#'
#' @returns A modified styler parse-data object.
#'
#' @noRd
set_line_break_after_assignment_mod <- function(pd) {
  assignment <- which(pd$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))
  if (!length(assignment)) return(pd)

  for (idx in assignment) {
    if (idx >= nrow(pd)) next

    # Check if LHS width exceeds threshold
    lhs <- seq_len(idx)
    lhs_width <- sum(nchar(pd$text[lhs], type = "width"))

    if (lhs_width > 40L) {
      # Check if the RHS starts with a function call
      rhs_start <- idx + 1L
      if (rhs_start <= nrow(pd)) {
        # Skip whitespace/newlines to find the actual RHS content
        rhs_idx <- rhs_start
        while (rhs_idx <= nrow(pd) &&
               pd$token[rhs_idx] %in% c("NEWLINE", "NL")) {
          rhs_idx <- rhs_idx + 1L
        }

        # Look ahead to see if there's an opening paren (function call)
        has_call <- FALSE
        if (rhs_idx <= nrow(pd)) {
          # Check if current position has a NAME followed by '('
          for (j in rhs_idx:min(rhs_idx + 5L, nrow(pd))) {
            if (pd$token[j] == "NAME" && j < nrow(pd)) {
              # Check if next non-whitespace token is an opening paren
              k <- j + 1L
              while (k <= nrow(pd) && pd$token[k] %in% c("NEWLINE", "NL")) {
                k <- k + 1L
              }
              if (k <= nrow(pd) && pd$token[k] %in% c("'('", "'['")) {
                has_call <- TRUE
                break
              }
            }
          }
        }

        if (has_call) {
          pd$lag_newlines[rhs_start] <- 1L
        }
      }
    }
  }
  pd
}
