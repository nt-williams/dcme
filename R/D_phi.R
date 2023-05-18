D_phi <- function(data, npsem, a, l, g, p, phi) {
  # g = g(a|w), q = q(z|a, w), p = p(l|z, a, w), and c = c(m|l, z, w)
  M <- npsem$get(data, "M")
  Z <- npsem$get(data, "Z")
  A <- npsem$get(data, "A")
  L <- npsem$get(data, "L")

  `P(l|a,W)` <- l*p[, gl("p(L=1|{a},w)")] + (1 - l)*(1 - p[, gl("p(L=1|{a},w)")])
  `P(a|W)` <- a*g[, "g(1|w)"] + (1 - a)*g[, "g(0|w)"]
  `P(MZ=1|l,a,W)` <- phi[, gl("phi(MZ=1|{l},{a},w)")]

  # equation 5 in paper
  ((A == a)*(L == l)) / (`P(a|W)` * `P(l|a,W)`) * (M*Z - `P(MZ=1|l,a,W)`) +
    `P(MZ=1|l,a,W)`# - mean(`P(M=1|l,Z=1,W)`*`P(Z=1|l,a,W)`)
}
