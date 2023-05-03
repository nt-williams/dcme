D_v <- function(data, npsem, aprime, astar, g, q, p, c, mu) {
  a <- npsem$get(data, "A")
  l <- npsem$get(data, "L")
  m <- npsem$get(data, "M")
  z <- npsem$get(data, "Z")
  y <- npsem$get(data, "Y")

  `P(a'|W)` <- g[, gl("g({aprime}|w)")]
  `P(a*|W)` <- g[, gl("g({astar}|w)")]
  `P(L|a',W)` <- l*p[, gl("p(L=1|{aprime},w)")] + (1 - l)*(1 - p[, gl("p(L=1|{aprime},w)")])

  `mu(L,a',W)` <- l * mu[, gl("mu(Y|1,{aprime},w)")] + (1 - l) * mu[, gl("mu(Y|0,{aprime},w)")]
  `mu(M,a',W)` <- m * mu[, gl("mu(Y|1,{aprime},w)")] + (1 - m) * mu[, gl("mu(Y|0,{aprime},w)")]
  `mu(0,a',W)` <- mu[, gl("mu(Y|0,{aprime},w)")]
  `mu(1,a',W)` <- mu[, gl("mu(Y|1,{aprime},w)")]

  `gamma(1|a*,W)` <- gamma(astar, c, p, q)
  `mu(0,a',W)gamma(0|a*,W)` <- `mu(0,a',W)` * (1 - `gamma(1|a*,W)`)
  `mu(1,a',W)gamma(1|a*,W)` <- `mu(1,a',W)` * `gamma(1|a*,W)`
  `gamma(L|a*,W)` <- l * `gamma(1|a*,W)` + (1 - l) * (1 - `gamma(1|a*,W)`)

  `v(a',a*)` <- mean(`mu(0,a',W)gamma(0|a*,W)`) + mean(`mu(1,a',W)gamma(1|a*,W)`)

  ((I(a == aprime)*`gamma(L|a*,W)`) / (`P(L|a',W)`*`P(a'|W)`)) * (y - `mu(L,a',W)`) +
    (I(a == astar)/`P(a*|W)`) * (`mu(M,a',W)` - (`mu(0,a',W)gamma(0|a*,W)` + `mu(1,a',W)gamma(1|a*,W)`)) +
    (`mu(0,a',W)gamma(0|a*,W)` + `mu(1,a',W)gamma(1|a*,W)`)# - `v(a',a*)`
}

# c(m|l,z,w)
# p(l|a,w)
# q(z|l,a,w)
gamma <- function(astar, c, p, q) {
  `q(Z=1|1,a*,w)` <- q[, gl("q(Z=1|1,{astar},w)")]
  `q(Z=1|0,a*,w)` <- q[, gl("q(Z=1|0,{astar},w)")]
  `p(L=1|a*,w)` <- p[, gl("p(L=1|{astar},w)")]

  gamm <- matrix(nrow = nrow(c), ncol = 2)
  colnames(gamm) <- c("gamma(M=1|a*,w)", "gamma(M=0|a*,w)")

  # L = 1
  (c[, "c(M=1|1,1,w)"] * `q(Z=1|1,a*,w)` + c[, "c(M=1|1,0,w)"] * (1 - `q(Z=1|1,a*,w)`)) * `p(L=1|a*,w)` +
    # L = 0
    (c[, "c(M=1|0,1,w)"] * `q(Z=1|0,a*,w)` + c[, "c(M=1|0,0,w)"] * (1 - `q(Z=1|0,a*,w)`)) * (1 - `p(L=1|a*,w)`)
}
