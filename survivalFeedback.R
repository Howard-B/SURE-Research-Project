n <- 1000

t0 <- 1
t1 <- 2

S1t0 <- rbeta(n, 2, 1)
rho <- rnorm(n, 0.1, 0.05)
S2t0 <- S1t0 + rho

index <- S2t0 >0 & S2t0 < 1

S1t0 <- S1t0[index]
S2t0 <- S2t0[index]

lambda1 <- -log(S1t0) / t0
lambda2 <- -log(S2t0) / t0

S1t1 <- exp(-t1 * lambda1)
S2t1 <- exp(-t1 * lambda2)

plot(S1t1, S2t1)
abline(v = quantile(S1t1, probs = c(0.025, 0.5, 0.975)),
       lty = 2)
abline(h = quantile(S2t1, probs = c(0.025, 0.5, 0.975)),
       lty = 2)
