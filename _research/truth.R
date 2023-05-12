library(dplyr)
source("_research/sim_data.R")

tmp <- expand.grid(W1 = c(0, 1), W2 = c(0, 1), W3 = c(0, 1))

tmp <- mutate(tmp,
              prob_W1 = W1*0.6 + (1 - W1)*0.4,
              prob_W2 = W2*0.3 + (1 - W2)*0.7,
              prob_W1_and_W2 = prob_W1*prob_W2,
              prob_W3_cond_W1_W2 = W3*pmin(0.2 + (W1 + W2) / 3, 1) +
                (1 - W3)*(1 - pmin(0.2 + (W1 + W2) / 3, 1)),
              prob_W = prob_W3_cond_W1_W2 * prob_W1_and_W2)

v_aprime_astar <- function(aprime, astar) {
  weighted.mean(mu(1, aprime, tmp[, paste0("W", 1:3)]) *
                  Gamma(1, astar, tmp[, paste0("W", 1:3)]), tmp$prob_W) +
    weighted.mean(mu(0, aprime, tmp[, paste0("W", 1:3)]) *
                    Gamma(0, astar, tmp[, paste0("W", 1:3)]), tmp$prob_W)
}

phi_al <- function(a, l) {
  weighted.mean(pm(1, l, 1, tmp[, paste0("W", 1:3)]) *
                  pz(1, a, l, tmp[, paste0("W", 1:3)]),
                tmp$prob_W)
}

Dv_aprime_astar <- function(data, aprime, astar) {
  a <- data$A
  l <- data$L
  m <- data$M
  z <- data$Z
  y <- data$Y
  w <- data[, paste0("W", 1:3)]

  `P(a'|L,W)` <- g(aprime, l, w)
  `P(a*|L,W)` <- g(astar, l, w)
  `P(L|W)` <- l*pl(1, w) + (1 - l)*pl(0, w)

  `mu(L,a',W)` <- l*mu(1, aprime, w) + (1 - l)*mu(0, aprime, w)
  `mu(M,a',W)` <- m*mu(1, aprime, w) + (1 - m)*mu(0, aprime, w)
  `mu(0,a',W)` <- mu(0, aprime, w)
  `mu(1,a',W)` <- mu(1, aprime, w)

  `mu(0,a',W)gamma(0|a*,W)` <- `mu(0,a',W)` * Gamma(0, astar, w)
  `mu(1,a',W)gamma(1|a*,W)` <- `mu(1,a',W)` * Gamma(1, astar, w)
  `gamma(L|a*,W)` <- l*Gamma(1, astar, w) + (1 - l)*Gamma(0, astar, w)

  # `v(a',a*)` <- mean(`mu(0,a',W)gamma(0|a*,W)`) + mean(`mu(1,a',W)gamma(1|a*,W)`)

  ((I(a == aprime)*`gamma(L|a*,W)`) / (`P(L|a',W)`*`P(a'|W)`)) * (y - `mu(L,a',W)`) +
    (I(a == astar)/`P(a*|W)`) * (`mu(M,a',W)` - (`mu(0,a',W)gamma(0|a*,W)` + `mu(1,a',W)gamma(1|a*,W)`)) +
    (`mu(0,a',W)gamma(0|a*,W)` + `mu(1,a',W)gamma(1|a*,W)`)# - `v(a',a*)`
}

Dphi_al <- function(data, a, l) {
  M <- data$M
  Z <- data$Z
  A <- data$A
  L <- data$L
  W <- data[, paste0("W", 1:3)]

  `P(Z=1|l,a,W)` <- pz(1, a, l, W)
  `P(l|a,W)` <- l*pl(1, a, W) + (1 - l)*pl(0, a, W)
  `P(a|W)` <- 0.5
  `P(M=1|l,Z=1,W)` <- pm(1, l, 1, W)

  ((A == a)*(L == l)) / (`P(a|W)` * `P(l|a,W)`) * (M*Z - `P(M=1|l,Z=1,W)`*`P(Z=1|l,a,W)`) +
    `P(M=1|l,Z=1,W)`*`P(Z=1|l,a,W)`# - mean(`P(M=1|l,Z=1,W)`*`P(Z=1|l,a,W)`)
}

v_11 <- v_aprime_astar(1, 1)
v_10 <- v_aprime_astar(1, 0)
v_00 <- v_aprime_astar(0, 0)

tiide <- v_10 - v_00
tiiie <- v_11 - v_10

# 1, 1
phi_11 <- phi_al(1, 1)
# 1, 0
phi_10 <- phi_al(1, 0)
# 0, 1
phi_01 <- phi_al(0, 1)
# 0, 0
phi_00 <- phi_al(0, 0)

jfs <- phi_11 - phi_10 - phi_01 + phi_00

cide <- tiide / jfs
ciie <- tiiie / jfs
