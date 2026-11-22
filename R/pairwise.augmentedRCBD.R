#' Perform Pairwise t Tests of Adjusted Means from \code{augmentedRCBD} Output
#'
#' \code{pairwise.augmentedRCBD} performs pairwise t tests of adjusted means from
#' an object of class \code{augmentedRCBD}.
#'
#' The default pairwise comparison in \code{\link[augmentedRCBD]{augmentedRCBD}}
#' employs \code{\link[emmeans]{pairs.emmGrid}} function from
#' \code{\link[emmeans]{emmeans}} which is very slow for large number of
#' comparisons.
#'
#' @param aug An object of class \code{augmentedRCBD}.
#' @param alpha Type I error probability (Significance level).
#' @param p.adjust The p value adjustment method. Must be one of
#'   \code{"none"}, \code{"tukey"}, \code{"sidak"}, \code{"holm"},
#'   \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"},
#'   \code{"BH"}, \code{"BY"}, or \code{"fdr"}.
#'
#' @returns A data frame of pairwise comparisons of treatments.
#'
#' @seealso \code{\link[stats]{p.adjust}}
#'
#' @importFrom augmentedRCBD augmentedRCBD
#' @importFrom stats p.adjust pt ptukey
#' @export
#'
#' @examples
#' library(augmentedRCBD)
#'
#' blk <- c(rep(1,7),rep(2,6),rep(3,7))
#' trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
#' y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
#'         70, 75, 74)
#' y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
#'         240, 268, 287, 226, 395, 450)
#' data <- data.frame(blk, trt, y1, y2)
#' # Convert block and treatment to factors
#' data$blk <- as.factor(data$blk)
#' data$trt <- as.factor(data$trt)
#' # Results for variable y1
#' out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE)
#' # Results for variable y2
#' out2 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
#'                       alpha = 0.05, group = TRUE, console = TRUE)
#'
#' # Pairwise t test without p value adjustment ----
#' pout1 <- pairwise.augmentedRCBD(out1, p.adjust = "none")
#' pout1

#'
#' pout2 <- pairwise.augmentedRCBD(out2, p.adjust = "none")
#' pout2
#'
#' # Pairwise t test with tukey adjustment ----
#' pout1_tukey <- pairwise.augmentedRCBD(out1, p.adjust = "tukey")
#' pout1_tukey
#'
#' pout2_tukey <- pairwise.augmentedRCBD(out2, p.adjust = "tukey")
#' pout2_tukey
#'
#' # Pairwise t test with holm p value adjustment ----
#' pout1_holm <- pairwise.augmentedRCBD(out1, p.adjust = "holm")
#' pout1_holm
#'
#' pout2_holm <- pairwise.augmentedRCBD(out2, p.adjust = "holm")
#' pout2_holm
#'
#' # Pairwise t test with bonferroni p value adjustment ----
#' pout1_bonferroni <- pairwise.augmentedRCBD(out1, p.adjust = "bonferroni")
#' pout1_bonferroni
#'
#' pout2_bonferroni <- pairwise.augmentedRCBD(out2, p.adjust = "bonferroni")
#' pout2_bonferroni
#'
pairwise.augmentedRCBD <-
  function(aug,
           alpha = NULL,
           p.adjust = c("none", "tukey", "sidak",
                        "holm", "hochberg", "hommel",
                        "bonferroni", "BH", "BY",
                        "fdr")) {

    p.adjust <- match.arg(p.adjust)

    # Validate alpha
    if (!is.null(alpha)) {
      if (length(alpha) != 1L || is.na(alpha) ||
          !(0 < alpha && alpha < 1)) {
        stop('"alpha" should be between 0 and 1.')
      }
    }

    # Extract data
    Means <- aug$Means
    ## Remove duplicated treatments from Means df
    Means <- Means[!duplicated(Means$Treatment), ]

    treatment <- Means$Treatment
    block <- Means$Block
    adjmeans <- Means$`Adjusted Means`

    ntr <- length(treatment)
    n.pairs <- as.double(ntr) * (ntr - 1) / 2

    if (ntr < 2L) {
      stop("At least two treatments are required.")
    }

    chks <- aug$Details$`Check treatments`

    is.check <- treatment %in% chks
    is.test <- !is.check

    treatment2 <- ifelse(is.check,
                         as.character(treatment),
                         paste0("(", treatment, ")"))

    df <- aug$`ANOVA, Block Adjusted`[[1]]["Residuals", "Df"]

    SE <- aug$`Std. Errors`
    SEcol <- "Std. Error of Diff."

    # Validate test-treatment block information
    if (any(is.na(block[is.test]))) {
      stop("Missing block information for a test treatment.")
    }

    # Extract standard errors
    se.diff <-
      c(different = SE["Two Test Treatments (Different Blocks)", SEcol],
        same = SE["Two Test Treatments (Same Block)", SEcol],
        test.control = SE["A Test Treatment and a Control Treatment", SEcol],
        control = SE["Control Treatment Means", SEcol])

    # Preallocate output
    estimate <- numeric(n.pairs)
    SE.out <- numeric(n.pairs)
    t.ratio <- numeric(n.pairs)
    contrast <- character(n.pairs)

    # Generate comparisons
    pos <- 0
    n <- ntr

    for (i in seq_len(ntr - 1L)) {

      j <- seq.int(i + 1L, ntr)
      nj <- length(j)

      idx <- pos + seq_len(nj)

      # Contrasts
      estimate[idx] <- adjmeans[i] - adjmeans[j]

      contrast[idx] <- paste0(
        treatment2[i], " - ", treatment2[j]
      )

      # Determine standard error
      if (is.test[i]) {

        if (all(is.test[j])) {

          same.block <-
            as.character(block[i]) == as.character(block[j])

          SE.out[idx] <-
            ifelse(same.block, se.diff["same"], se.diff["different"])

        } else {

          SE.out[idx] <- se.diff["test.control"]

        }

      } else {

        SE.out[idx] <-
          ifelse(is.check[j], se.diff["control"], se.diff["test.control"])

      }

      pos <- pos + nj

    }

    # Calculate t statistics
    t.ratio <- estimate / SE.out

    # Calculate p values
    ## Family-Wise Error Rate (FWER) Adjustments
    ## "tukey", "sidak", "holm", "hochberg", "hommel", "bonferroni"
    ## False Discovery Rate (FDR) Adjustments
    ## "BH"/"fdr", "BY"

    if (p.adjust == "tukey") {
      q.stat <- sqrt(2) * abs(t.ratio)

      p.value <- ptukey(q = q.stat, nmeans = ntr, df = df, lower.tail = FALSE)

    } else { #p.adjust != "tukey"

      ## p.adjust == "none"
      p.value <- 2 * pt(q = -abs(t.ratio), df = df)

      if (p.adjust %in% c("holm", "hochberg", "hommel",
                          "bonferroni", "BH", "BY", "fdr")) {

        p.value <- stats::p.adjust(p = p.value, method = p.adjust)

      }

      if (p.adjust == "sidak") {

        p.value <- -expm1(length(p.value) * log1p(-p.value))

      }

    }

    # Significance
    sig <- ifelse(p.value < 0.001, "***",
                  ifelse(p.value < 0.01, "**",
                         ifelse(p.value < 0.05, "*", "")))

    # Return result
    data.frame(contrast = contrast,
               estimate = estimate,
               SE = SE.out,
               df = rep(df, length(estimate)),
               t.ratio = t.ratio,
               p.value = p.value,
               sig = sig,
               stringsAsFactors = FALSE)
  }
