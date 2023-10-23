D_phi <- function(data, vars, a, l, phi, g, p) {
    A <- vars$get(data, "A")
    L <- vars$get(data, "L")
    M <- vars$get(data, "M")
    Z <- vars$get(data, "Z")

    `I(A=a,L=l)` <- as.numeric(A == a)*as.numeric(L == l)
    `P(A=a|w)` <- a*g[[gl("g(A=1|w)")]] + (1 - a)*(1 - g[[gl("g(A=1|w)")]])
    `P(L=l|a,w)` <- l*p[[gl("p(L=1|A={a},w)")]] + (1 - l)*(1 - p[[gl("p(L=1|A={a},w)")]])
    `P(MZ=1|l,a,w)` <- phi[[gl("phi(MZ=1|L={l},A={a},w)")]]

    (`I(A=a,L=l)` / (`P(A=a|w)`*`P(L=l|a,w)`))*(M*Z - `P(MZ=1|l,a,w)`) +
        `P(MZ=1|l,a,w)`
}

D_phi2 <- function(data, vars, a, l, c, q, g, p) {
    A <- vars$get(data, "A")
    L <- vars$get(data, "L")
    M <- vars$get(data, "M")
    Z <- vars$get(data, "Z")

    `I(A=a,L=l)` <- as.numeric(A == a)*as.numeric(L == l)
    `P(A=a|w)` <- a*g[[gl("g(A=1|w)")]] + (1 - a)*(1 - g[[gl("g(A=1|w)")]])
    `P(L=l|a,w)` <- l*p[[gl("p(L=1|A={a},w)")]] + (1 - l)*(1 - p[[gl("p(L=1|A={a},w)")]])
    `P(MZ=1|l,a,w)` <- phiN(a, l, c, q)

    (`I(A=a,L=l)` / (`P(A=a|w)`*`P(L=l|a,w)`))*(M*Z - `P(MZ=1|l,a,w)`) +
        `P(MZ=1|l,a,w)`
}
