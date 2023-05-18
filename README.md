
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dcme

**D**ouble-**C**omplier **M**ediation **E**ffects with Instrumental
Variables

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of dcme from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nt-williams/dcme")
```

## Example

``` r
library(dcme)

n <- 1000
w_1 <- rbinom(n, 1, prob = 0.6)
w_2 <- rbinom(n, 1, prob = 0.3)
w <- cbind(w_1, w_2,
           rbinom(n, 1, prob = pmin(0.2 + (w_1 + w_2) / 3, 1)))

a <- rbinom(n, 1, prob = 0.5)
l <- rbinom(n, 1, plogis(rowMeans(-log(2) + w + 3*a - 1)))
z <- rbinom(n, 1, plogis(rowMeans(-log(1.1) * w) + 3*a - l))
m <- rbinom(n, 1, plogis(rowSums(-log(3) * w[,-3]) + 2*l + z - 1))
y <- rbinom(n, 1, plogis(rowSums(-log(5)*w) + z + m + .3))

colnames(w) <- paste("W", seq_len(ncol(w)), sep = "")
tmp <- as.data.frame(cbind(W = w, A = a, Z = z, L = l, M = m, Y = y))

dcme(tmp, paste0("W", 1:3), "A", "L", "Z", "M", "Y", "binomial", 1)
#> Loading required package: nnls
#>   Estimand    psi        95% CI
#> 1    TIIDE  0.137  0.053, 0.221
#> 2    TIIIE -0.014 -0.054, 0.026
#> 3      JFS  0.387  0.255, 0.519
#> 4     CIDE  0.353 -0.141, 0.069
#> 5     CIIE -0.036  0.097, 0.610
```
