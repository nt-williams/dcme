
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dcme

<!-- badges: start -->
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
w <- cbind(
  w_1, w_2,
  rbinom(n, 1, prob = pmin(0.2 + (w_1 + w_2) / 3, 1))
)

a <- rbinom(n, 1, prob = 0.5)
l <- rbinom(n, 1, plogis(rowMeans(-log(2) + w + 3*a - 1)))
z <- rbinom(n, 1, plogis(rowMeans(-log(1.1) * w) + 3*a - l))
m <- rbinom(n, 1, plogis(rowSums(-log(3) * w[,-3]) + 2*l + z - 1))
y <- rbinom(n, 1, plogis(rowSums(-log(5)*w) + z + m + .3))

colnames(w) <- paste("W", seq_len(ncol(w)), sep = "")
tmp <- as.data.frame(cbind(W = w, A = a, Z = z, L = l, M = m, Y = y))

dcme(tmp, paste0("W", 1:3), "A", "L", "Z", "M", "Y", "binomial", 1)
#> Loading required package: nnls
#> $TIIDE
#> $TIIDE$psi
#> [1] 0.1229145
#> 
#> $TIIDE$se
#> [1] 0.04345169
#> 
#> $TIIDE$ci
#> [1] 0.03775076 0.20807825
#> 
#> 
#> $TIIIE
#> $TIIIE$psi
#> [1] -0.007386946
#> 
#> $TIIIE$se
#> [1] 0.02061183
#> 
#> $TIIIE$ci
#> [1] -0.04778538  0.03301149
#> 
#> 
#> $JFS
#> $JFS$psi
#> [1] 0.300995
#> 
#> $JFS$se
#> [1] 0.07415356
#> 
#> $JFS$ci
#> [1] 0.1556567 0.4463333
#> 
#> 
#> $CIDE
#> $CIDE$psi
#> [1] 0.4083606
#> 
#> $CIDE$se
#> [1] 0.1888216
#> 
#> $CIDE$ci
#> [1] 0.0382771 0.7784441
#> 
#> 
#> $CIIE
#> $CIIE$psi
#> [1] -0.02454176
#> 
#> $CIIE$se
#> [1] 0.0702691
#> 
#> $CIIE$ci
#> [1] -0.1622667  0.1131832
```
