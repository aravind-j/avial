#' Perform Pairwise t Tests of Adjusted Means from \code{augmentedRCBD} Output
#'
#' \code{pairwise.augmentedRCBD} performs pairwise t tests of adjusted means from
#' an object of class \code{augmentedRCBD}.
#'
#' The default pairwise comparison in \code{\link[augmentedRCBD]{augmentedRCBD}}
#' employs \code{\link[emmeans]{pairs.emmGrid}} function from
#' \code{\link[emmeans]{emmeans}} which is very slow for large number of
#' comparisons. This function attempts to do the same faster with parallel
#' computing with the package \code{\link[parallel]{parallel-package}}.
#'
#' @param aug An object of class \code{augmentedRCBD}.
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}} for
#'   parallel evaluations.
#' @param p.adjust The p value adjustment method. Either \code{"none"},
#'   \code{"tukey"} or \code{"sidak"}.
#'
#' @returns A data frame of pairwise comparisons of treatments.
#'
#' @seealso \code{\link[parallel]{makeCluster}}
#'
#' @importFrom augmentedRCBD augmentedRCBD
#' @importFrom parallel clusterExport makeCluster parLapply stopCluster
#' @importFrom stats pt qtukey
#' @importFrom utils combn
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
#' # Make cluster
#' library(parallel)
#' ncores <- max(2, parallel::detectCores() - 2)
#'
#' # Pairwise t test without p value adjustment
#' cl <- makeCluster(getOption("cl.cores", ncores))
#' pout1 <- pairwise.augmentedRCBD(out1, cl = cl,
#'                                 p.adjust = "none")
#' stopCluster(cl)
#'
#' cl <- makeCluster(getOption("cl.cores", ncores))
#' pout2 <- pairwise.augmentedRCBD(out1, cl = cl,
#'                                 p.adjust = "none")
#' stopCluster(cl)
#'
#' # Pairwise t test with tukey adjustment
#' cl <- makeCluster(getOption("cl.cores", ncores))
#' pout1_tukey <- pairwise.augmentedRCBD(out1, cl = cl,
#'                                       p.adjust = "tukey")
#' stopCluster(cl)
#'
#' cl <- makeCluster(getOption("cl.cores", ncores))
#' pout2_tukey <- pairwise.augmentedRCBD(out1, cl = cl,
#'                                       p.adjust = "tukey")
#' stopCluster(cl)
#'
#' # Pairwise t test with sidak p value adjustment
#' cl <- makeCluster(getOption("cl.cores", ncores))
#' pout1_sidak <- pairwise.augmentedRCBD(out1, cl = cl,
#'                                       p.adjust = "sidak")
#' stopCluster(cl)
#'
#' cl <- makeCluster(getOption("cl.cores", ncores))
#' pout2_sidak <- pairwise.augmentedRCBD(out1, cl = cl,
#'                                       p.adjust = "sidak")
#' stopCluster(cl)
#'
pairwise.augmentedRCBD <- function(aug, cl = NULL,
                                   p.adjust = c("none", "tukey", "sidak")) {

  ntr <- aug$Details$`Number of treatments`
  n.pairs <- (ntr * (ntr - 1)) / 2

  chks <- aug$Details$`Check treatments`

  treatment <- aug$Means$Treatment
  block <- aug$Means$Block
  adjmeans <- aug$Means$`Adjusted Means`
  df <- aug$`ANOVA, Block Adjusted`[[1]]["Residuals", "Df"]

  treatment2 <- ifelse(treatment %in% chks,
                       treatment,
                       paste0("(", treatment, ")"))

  pairs_list <- combn(x = treatment, m = 2, simplify = FALSE)

  SE <- aug$`Std. Errors`

  SEcol <- "Std. Error of Diff."
  if (p.adjust == "tukey") {
    if (!is.na(colnames(SE)[3])) {
      SEcol <- colnames(SE)[3]
    } else {
      alpha <- gsub(pattern = "\\D", replacement = "", x = colnames(SE)[2])
      alpha <- as.numeric(alpha) / 100
      q0 <- qtukey(p = 1 - alpha, nmeans = length(treatment),
                   df = df)
      SE$THSD <- c((q0 * SE[1:3,]$`Std. Error of Diff.`)/sqrt(2), 0)
      SEcol <- "THSD"
    }
  }

  clusterExport(cl, c("pairs_list", "SE",
                      "treatment", "block", "adjmeans", "df",
                      "treatment2", "chks"),
                envir=environment())

  pairs_aug <- parLapply(X = pairs_list, fun = function(x) {
    ind1 <- which(treatment == x[1])
    ind2 <- which(treatment == x[2])
    contrast <- paste(treatment2[ind1], "-", treatment2[ind2])
    estimate <- adjmeans[ind1] - adjmeans[ind2]
    df <- df

    if (all(c(treatment[ind1], treatment[ind2]) %in%
            setdiff(treatment, chks))) {

      if (block[ind1] != block[ind2]) {
        SEdiff <-
          SE["Two Test Treatments (Different Blocks)",  SEcol]

      } else {
        SEdiff <-
          SE["Two Test Treatments (Same Block)",  SEcol]
      }

    } else {
      if (!all(c(treatment[ind1], treatment[ind2]) %in% chks)) {
        SEdiff <-
          SE["A Test Treatment and a Control Treatment",  SEcol]
      } else {
        SEdiff <-
          SE["Control Treatment Means",  SEcol]
      }
    }

    t.ratio <- estimate / SEdiff

    out <- data.frame(contrast, estimate, SE = SEdiff, df, t.ratio)

    return(out)
  }, cl = cl)

  # stopCluster(cl)

  pairs_aug <- dplyr::bind_rows(pairs_aug)
  pairs_aug$p.value <- 2 * pt(q = pairs_aug$t.ratio, df = pairs_aug$df)
  if (p.adjust == "sidak") {
    pairs_aug$p.value <-
      1 - (1 - pairs_aug$p.value) ^ length(pairs_aug$p.value)
  }
  pairs_aug$sig <- ifelse(pairs_aug$p.value < 0.001, "***",
                          ifelse(pairs_aug$p.value < 0.01, "**",
                                 ifelse(pairs_aug$p.value < 0.05, "*",
                                        "")))
  return(pairs_aug)

}
