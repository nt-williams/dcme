dcmeSim <- R6::R6Class(
    "dcmeSim",
    public = list(
        n = NULL,
        data = NULL,
        seed = NULL,
        initialize = function(n) {
            self$n <- n

            U_1 <- rbinom(n, 1, 0.25)
            U_2 <- rbinom(n, 1, 0.75)
            U_3 <- rbinom(n, 1, 0.33)

            w_1 <- rbinom(n, 1, prob = 0.6)
            w_2 <- rbinom(n, 1, prob = 0.3)
            w <- cbind(w_1, w_2,
                       rbinom(n, 1, prob = pmin(0.2 + (w_1 + w_2) / 3, 1)))

            a <- as.numeric(rbinom(n, 1, prob = self$g(1, w)))
            l <- rbinom(n, 1, self$pl(1, a, w))
            z <- rbinom(n, 1, self$pz(1, a, l, w, U_1, U_2))
            m <- rbinom(n, 1, self$pm(1, l, z, w, U_1, U_3))
            y <- rbinom(n, 1, self$my(m, z, w, U_2, U_3))

            colnames(w) <- paste("W", seq_len(ncol(w)), sep = "")
            self$data <- cbind(w, data.frame(A = a, Z = z, L = l, M = m, Y = y, u1 = U_1, u2 = U_2, u3 = U_3))
        },
        truth = function() {
            Dv_11 <- self$D_v(1, 1)
            Dv_10 <- self$D_v(1, 0)
            Dv_00 <- self$D_v(0, 0)

            D_JFS <- self$Dphi(1, 1) - self$Dphi(1, 0) - self$Dphi(0, 1) + self$Dphi(0, 0)
            JFS <- mean(D_JFS)

            DCIDE <- mean(Dv_10 - Dv_00) / JFS
            DCIIE <- mean(Dv_11 - Dv_10) / JFS
            DCITE <- mean(Dv_11 - Dv_00) / JFS
            
            D_ciie <- ((Dv_11 - Dv_10) / JFS) - (( mean(Dv_11 - Dv_10) * D_JFS) / JFS^2)
            D_cide <- ((Dv_10 - Dv_00) / JFS) - ((mean(Dv_11 - Dv_10) * D_JFS) / JFS^2)
            
            list(TIIDE = mean(Dv_10 - Dv_00),
                 TIIIE = mean(Dv_11 - Dv_10),
                 CITE = DCITE,
                 CIDE = DCIDE,
                 CIIE = DCIIE, 
                 bound_CIIE = mean((D_ciie - DCIIE)^2),
                 bound_CIDE = mean((D_cide - DCIDE)^2))
        },
        g = function(a, w) {
            a * 0.5 + (1 - a) * (1 - 0.5)
        },
        pl = function(l, a, w) {
            p <- plogis(rowMeans(-log(1.1) * w) + a)
            l*p + (1 - l)*(1 - p)
        },
        pz = function(z, a, l, w, u1, u2) {
            p <- plogis(rowMeans(-log(2) + w + a + l) - 1 + 0.15*u1 - 0.2*u2)
            z*p + (1 - z)*(1 - p)
        },
        pm = function(m, l, z, w, u1, u3) {
            p <- plogis(rowSums(-log(3) * w[, -3]) + 3 * l + z - 1 + 0.25*u1 + 0.15*u3)
            m * p + (1 - m) * (1 - p)
        },
        pmz = function(l, a, w, u1, u2, u3) {
            self$pm(1, l, 1, w, u1, u3) * self$pz(1, a, l, w, u1, u2)
        },
        my = function(m, z, w, u2, u3) {
            plogis(rowSums(-log(5)*w) + z + m + .3 - 0.25*u2 + 0.1*u3)
        },
        # P(M=l|a,W)
        gamma = function(l, astar) {
            W <- self$data[, paste0("W", 1:3)]
            u1 <- self$data$u1
            u2 <- self$data$u2
            u3 <- self$data$u3

            M <- l

            (self$pm(M, 1, 1, W, u1, u3)*self$pz(1, astar, 1, W, u1, u2) +
                    self$pm(M, 1, 0, W, u1, u3)*self$pz(0, astar, 1, W, u1, u2))*self$pl(1, astar, W) +
                (self$pm(M, 0, 1, W, u1, u3)*self$pz(1, astar, 0, W, u1, u2) +
                     self$pm(M, 0, 0, W, u1, u3)*self$pz(0, astar, 0, W, u1, u2))*self$pl(0, astar, W)
        },
        # E(Y|L=l,A=a',W)
        mu_la = function(l, aprime) {
            W <- self$data[, paste0("W", 1:3)]
            u1 <- self$data$u1
            u2 <- self$data$u2
            u3 <- self$data$u3

            # integrate out M and then integrate out Z
            (self$my(1, 1, W, u2, u3)*self$pm(1, l, 1, W, u1, u3) +
                    self$my(0, 1, W, u2, u3)*self$pm(0, l, 1, W, u1, u3))*self$pz(1, aprime, l, W, u1, u2) +
                (self$my(1, 0, W, u2, u3)*self$pm(1, l, 0, W, u1, u3) +
                     self$my(0, 0, W, u2, u3)*self$pm(0, l, 0, W, u1, u3))*self$pz(0, aprime, l, W, u1, u2)
        },
        muv = function(aprime, astar) {
            self$mu_la(1, aprime)*self$gamma(1, astar) + self$mu_la(0, aprime)*self$gamma(0, astar)
        },
        Dphi = function(a, l) {
            W <- self$data[, paste0("W", 1:3)]
            A <- self$data$A
            Z <- self$data$Z
            L <- self$data$L
            M <- self$data$M
            u1 <- self$data$u1
            u2 <- self$data$u2
            u3 <- self$data$u3

            (A == a)*(L == l) / (self$g(a, W)*self$pl(l, a, W))*(M*Z - self$pmz(l, a, W, u1, u2, u3)) +
                self$pmz(l, a, W, u1, u2, u3)
        },
        D_v = function(aprime, astar) {
            W <- self$data[, paste0("W", 1:3)]
            A <- self$data$A
            Z <- self$data$Z
            L <- self$data$L
            M <- self$data$M
            Y <- self$data$Y
            u1 <- self$data$u1
            u2 <- self$data$u2
            u3 <- self$data$u3

            ((A == aprime)*self$gamma(L, astar)) / (self$pl(L, aprime, W)*self$g(aprime, W))*(Y - self$mu_la(L, aprime)) +
                ((A == astar) / self$g(astar, W))*(self$mu_la(L, aprime) - self$muv(aprime, astar)) +
                self$muv(aprime, astar)
        }
    )
)
