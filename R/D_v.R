D_v <- function(data, vars, ap, as, mu, gamma, g, p) {
    A <- vars$get(data, "A")
    L <- vars$get(data, "L")
    Y <- vars$get(data, "Y")

    `I(A=a')` <- as.numeric(A == ap)
    `I(A=a*)` <- as.numeric(A == as)
    `P(A=a'|w)` <- ap*g[[gl("g(A=1|w)")]] + (1 - ap)*(1 - g[[gl("g(A=1|w)")]])
    `P(A=a*|w)` <- as*g[[gl("g(A=1|w)")]] + (1 - as)*(1 - g[[gl("g(A=1|w)")]])
    `P(L=l|a',w)` <- L*p[[gl("p(L=1|A={ap},w)")]] + (1 - L)*(1 - p[[gl("p(L=1|A={ap},w)")]])
    `gamma(L=l|a*,w)` <- L*gamma[[gl("gamma(M=1|A={as},w)")]] +
        (1 - L)*(1 - gamma[[gl("gamma(M=1|A={as},w)")]])

    eval_v <- function(l, ap, as) {
        `gamma(l|a*,w)` <- l*gamma[[gl("gamma(M=1|A={as},w)")]] +
            (1 - l)*(1 - gamma[[gl("gamma(M=1|A={as},w)")]])
        mu[[gl("mu(L={l},A={ap},w)")]]*`gamma(l|a*,w)`
    }

    `v(a',a*)` <- eval_v(0, ap, as) + eval_v(1, ap, as)
    `mu(l,a',w)` <- mu[[gl("mu(L=l,A={ap},w)")]]

    (`I(A=a')`*`gamma(L=l|a*,w)`) / (`P(L=l|a',w)`*`P(A=a'|w)`)*(Y - `mu(l,a',w)`) +
        (`I(A=a*)` / `P(A=a*|w)`)*(`mu(l,a',w)` - `v(a',a*)`) +
        `v(a',a*)`
}

D_v2 <- function(data, vars, ap, as, b, c, q, g, p) {
    A <- vars$get(data, "A")
    L <- vars$get(data, "L")
    Y <- vars$get(data, "Y")

    `I(A=a')` <- as.numeric(A == ap)
    `I(A=a*)` <- as.numeric(A == as)
    `P(A=a'|w)` <- ap*g[[gl("g(A=1|w)")]] + (1 - ap)*(1 - g[[gl("g(A=1|w)")]])
    `P(A=a*|w)` <- as*g[[gl("g(A=1|w)")]] + (1 - as)*(1 - g[[gl("g(A=1|w)")]])
    `P(L=l|a',w)` <- L*p[[gl("p(L=1|A={ap},w)")]] + (1 - L)*(1 - p[[gl("p(L=1|A={ap},w)")]])

    `gamma(L=l|a*,w)` <- L*gammaN(as, c, q, p) + (1 - L)*(1 - gammaN(as, c, q, p))

    eval_v <- function(l, ap, as) {
        `gamma(l|a*,w)` <- l*gammaN(as, c, q, p) + (1 - l)*(1 - gammaN(as, c, q, p))
        muN(ap, l, b, c, q)*`gamma(l|a*,w)`
    }

    `v(a',a*)` <- eval_v(0, ap, as) + eval_v(1, ap, as)

    `mu(l,a',w)` <- L*muN(ap, 1, b, c, q) + (1 - L)*muN(ap, 0, b, c, q)

    (`I(A=a')`*`gamma(L=l|a*,w)`) / (`P(L=l|a',w)`*`P(A=a'|w)`)*(Y - `mu(l,a',w)`) +
        (`I(A=a*)` / `P(A=a*|w)`)*(`mu(l,a',w)` - `v(a',a*)`) +
        `v(a',a*)`
}
