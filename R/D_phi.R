D_phi <- function(data, npsem, a, l, g, q, p, cc) {
  # g = g(a|w), q = q(z|a, w), p = p(l|z, a, w), and c = c(m|l, z, w)
  M <- npsem$get(data, "M")
  Z <- npsem$get(data, "Z")
  A <- npsem$get(data, "A")
  L <- npsem$get(data, "L")

  `P(Z=1|l,a,W)` <- q[, gl("q(Z=1|{l},{a},w)")]
  `P(l|a,W)` <- l*p[, gl("p(L=1|{a},w)")] + (1 - l)*(1 - p[, gl("p(L=1|{a},w)")])
  `P(a|W)` <- a*g[, "g(1|w)"] + (1 - a)*g[, "g(0|w)"]
  `P(M=1|l,Z=1,W)` <- cc[, gl("c(M=1|{l},1,w)")]

  # equation 5 in paper
  ((A == a)*(L == l)) / (`P(a|W)` * `P(l|a,W)`) * (M*Z - `P(M=1|l,Z=1,W)`*`P(Z=1|l,a,W)`) +
    `P(M=1|l,Z=1,W)`*`P(Z=1|l,a,W)`# - mean(`P(M=1|l,Z=1,W)`*`P(Z=1|l,a,W)`)
}
