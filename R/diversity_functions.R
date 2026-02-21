
#' Diversity Index Functions
#'
#' To be used by \code{\link[avial]{diversity.calc}}.
#'
#' @param x A factor vector of categories (e.g., species, traits). The frequency
#'   of each level is treated as the abundance of that category.
#' @param base The logarithm base to be used for computation of shannon family
#'   of diversity indices. Default is \code{exp(1)}.
#' @param warn logical. If \code{TRUE} shows the relevant warning. Default is
#'   \code{TRUE}.
#' @param q The order of the parametric index.
#'
#' @return The calculated diversity index value.
#'
#' @name diversity_functions
#' @rdname diversity_functions
#'
NULL

# Berger Parker indices ----

## Berger–Parker Dominance Index
#' @rdname diversity_functions
#' @export
berger_parker <- function(x) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  p <- tab / length(x)
  max(p)
}

## Inverse Berger–Parker Index
#' @rdname diversity_functions
#' @export
berger_parker_reciprocal <- function(x) {
  1 / berger_parker(x)
}


# Simpson indices ----

## Simpson's Index (d)
#' @rdname diversity_functions
#' @export
simpson <- function(x) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  p <- tab / length(x)
  sum(p ^ 2)
}

## Simpson's Index of Diversity / Gini-Simpson Index (D)
#' @rdname diversity_functions
#' @export
gini_simpson <- function(x) {
  1 - simpson(x)
}

## Maximum Simpson's Index
#' @rdname diversity_functions
#' @export
simpson_max <- function(x) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  1 - (1 / length(tab))
}

## Reciprocal Simpson's Index
#' @rdname diversity_functions
#' @export
simpson_reciprocal <- function(x) {
  1 / simpson(x)
}

## Relative Simpson's Index
#' @rdname diversity_functions
#' @export
simpson_relative <- function(x) {
  denom <- simpson_max(x)
  if (denom == 0) return(NA_real_)
  gini_simpson(x) / denom
}

## Simpson's evenness
#' @rdname diversity_functions
#' @export
simpson_evenness <- function(x) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  1 / (simpson(x) * length(tab))
}

# Shannon indices ----

## Shannon-Wiener Diversity Index (H)
#' @rdname diversity_functions
#' @export
shannon <- function(x, base = 2) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  p <- tab / length(x)
  -sum(p * log(p, base = base))
}

## Maximum Shannon-Weaver Diversity Index
#' @rdname diversity_functions
#' @export
shannon_max <- function(x, base = 2) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  log(length(tab), base = base)
}

## Relative Shannon-Weaver Diversity Index
#' @rdname diversity_functions
#' @export
shannon_relative <- function(x, base = 2) {
  shannon(x, base) / shannon_max(x, base)
}

## Effective number of species
#' @rdname diversity_functions
#' @export
shannon_ens <- function(x, base = 2) {
  base ^ shannon(x, base)
}

# McIntosh indices ----

## McIntosh Index
#' @rdname diversity_functions
#' @export
mcintosh_diversity <- function(x) {
  n <- tabulate(x)
  n <- n[n > 0]
  N <- sum(n)
  U <- sqrt(sum(n^2))
  (N - U) / (N - sqrt(N))
}

## McIntosh Evenness
#' @rdname diversity_functions
#' @export
mcintosh_evenness <- function(x) {
  n <- tabulate(x)
  n <- n[n > 0]
  N <- sum(n)
  U <- sqrt(sum(n^2))
  S <- length(n) #k
  (N - U) / (N - (N / sqrt(S)))
}

## Smith & Wilson's Evenness Index
# E_var
#' @rdname diversity_functions
#' @export
smith_wilson <- function(x, warn = TRUE) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  p <- tab / length(x) # relative abundances

  if(length(p) < 2) {
    if(warn) {
      warning("E_var undefined for <2 species or classes; returning NA.")
    }
    return(NA_real_)
  }

  ln_p <- log(p)
  sd_ln <- sd(ln_p)

  1 - (2 / pi) * atan(sd_ln)
}

# Heip's evenness
#' @rdname diversity_functions
#' @export
heip_evenness <- function(x) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  S <- length(tab)
  H <- shannon(x)
  if(S <= 1) return(NA_real_)
  (exp(H) - 1) / (S - 1)
}


# Margalef's richness index
#' @rdname diversity_functions
#' @export
margalef_index <- function(x) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  S <- length(tab)
  N <- length(x)
  if(N <= 1) return(NA_real_)
  (S - 1) / log(N)
}

# Menhinick's index
#' @rdname diversity_functions
#' @export
menhinick_index <- function(x) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  S <- length(tab)
  N <- length(x)
  S / sqrt(N)
}


# Brillouin index ----
#' @rdname diversity_functions
#' @export
brillouin_index <- function(x) {
  n <- tabulate(x)
  n <- n[n > 0]
  N <- sum(n)

  if (N <= 1) {
    return(NA_real_)
  }

  (lgamma(N + 1) - sum(lgamma(n + 1))) / N
}

# Parametric indices ----
# Hill number
#' @rdname diversity_functions
#' @export
hill_number <- function(x, q = 1) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  p <- tab / length(x)
  S <- length(p)

  if ((abs(q - 1) < 1e-8)) { # (q == 1) floating-point tolerance.
    exp(-sum(p * log(p, base = exp(1)))) # hill numbers are base invariant
  } else {
    (sum(p ^ q)) ^ (1 / (1 - q))
  }
}

# Rényi Entropy
#' @rdname diversity_functions
#' @export
renyi_entropy <- function(x, q = 1) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  p <- tab / length(x)

  if ((abs(q - 1) < 1e-8)) { # (q == 1) floating-point tolerance.
    -sum(p * log(p, base = exp(1))) # use natural log
  } else {
    log(sum(p ^ q), base = exp(1)) / (1 - q)
  }
}

# Tsallis Entropy
#' @rdname diversity_functions
#' @export
tsallis_entropy <- function(x, q = 1) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  p <- tab / length(x)

  if ((abs(q - 1) < 1e-8)) { # (q == 1) floating-point tolerance.
    -sum(p * log(p, base = exp(1))) # use natural log
  } else {
    (1 - sum(p ^ q)) / (q - 1)
  }
}

# Hill's evenness
#' @rdname diversity_functions
#' @export
hill_evenness <- function(x, q = 1) {
  tab <- tabulate(x)
  tab <- tab[tab > 0]
  S <- length(tab)
  D_q <- hill_number(x, q)
  D_q / S
}

