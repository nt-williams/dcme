g <- function(a, w) {
  a * 0.5 + (1 - a) * (1 - 0.5)
}

pz <- function(z, a, l, w) {
  p <- plogis(rowMeans(-log(1.1) * w) + 3*a - l)
  z * p + (1 - z) * (1 - p)
}

pl <- function(l, a, w) {
  p <- plogis(rowMeans(-log(2) + w + 3*a - 1))
  l * p + (1 - l) * (1 - p)
}

pm <- function(m, l, z, w) {
  p <- plogis(rowSums(-log(3) * w[,-3]) + 2*l + z - 1)
  m * p + (1 - m) * (1 - p)
}

my <- function(m, z, w) {
  plogis(rowSums(-log(5)*w) + z + m + .3)
}

gamma <- function(m, astar, w) {
  (pm(m, 1, 1, w) * pz(1, astar, 1, w) + pm(m, 1, 0, w) * pz(0, astar, 1, w)) * pl(1, astar, w) +
    (pm(m, 0, 1, w) * pz(1, astar, 0, w) + pm(m, 0, 0, w) * pz(0, astar, 0, w)) * pl(0, astar, w)
}

# E(Y|M,Z,W) -> E(Y|L=m,A=a',W)
# Integrate out M with respect to L being set to m, then integrate out Z with respect to A being set to a'
mu <- function(Lm, aprime, w) {
  (my(1, 1, w) * pm(1, Lm, 1, w) + my(0, 1, w) * pm(0, Lm, 1, w)) * pz(1, aprime, Lm, w) +
    (my(1, 0, w) * pm(1, Lm, 0, w) + my(0, 0, w) * pm(0, Lm, 0, w)) * pz(0, aprime, Lm, w)
}

gendata <- function(n, seed) {
  set.seed(seed)
  w_1 <- rbinom(n, 1, prob = 0.6)
  w_2 <- rbinom(n, 1, prob = 0.3)
  w <- cbind(
    w_1, w_2,
    rbinom(n, 1, prob = pmin(0.2 + (w_1 + w_2) / 3, 1))
  )

  a <- as.numeric(rbinom(n, 1, prob = g(1, w)))
  l <- rbinom(n, 1, pl(1, a, w))
  z <- rbinom(n, 1, pz(1, a, l, w))
  m <- rbinom(n, 1, pm(1, l, z, w))
  y <- rbinom(n, 1, my(m, z, w))

  colnames(w) <- paste("W", seq_len(ncol(w)), sep = "")
  as.data.frame(cbind(W = w, A = a, Z = z, L = l, M = m, Y = y))
}
