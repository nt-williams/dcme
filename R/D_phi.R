D_phi <- function(data, npsem, a, l, g, q, p, cc) {
  # g = g(a|w), q = q(z|a, w), p = p(l|z, a, w), and c = c(m|l, z, w)
  M <- npsem$get(data, "M")
  Z <- npsem$get(data, "Z")
  A <- npsem$get(data, "A")
  L <- npsem$get(data, "L")

  `P(Z=1|l,a,W)` <- q[, gl("q(Z=1|{l},{a},w)")]
  `P(l|W)` <- l*p[, "p(L=1|w)"] + (1 - l)*(1 - p[, "p(L=1|w)"])
  `P(a|l,W)` <- a*g[, gl("g(1|{l},w)")] + (1 - a)*(1 - g[, gl("g(1|{l},w)")])
  `P(M=1|l,Z=1,W)` <- cc[, gl("c(M=1|{l},1,w)")]

  # equation 5 in paper
  ((A == a)*(L == l)) / (`P(a|l,W)` * `P(l|W)`) * (M*Z - `P(M=1|l,Z=1,W)`*`P(Z=1|l,a,W)`) +
    `P(M=1|l,Z=1,W)`*`P(Z=1|l,a,W)`# - mean(`P(M=1|l,Z=1,W)`*`P(Z=1|l,a,W)`)
}
