install.packages("devtools")

devtools::install_github("nt-williams/mlr3superlearner@Screener")
devtools::install_github("nt-williams/dcme")

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
