a <- pnorm(1.33)
b <- 1 - a
c <- pnorm(-1.33)
d <- 1 - c
e <- pnorm(0.45)
f <- 1 - pnorm(-1.05)
g <- pnorm(0.65)
h <- pnorm(1.2) - pnorm(-0.45) # P(x < Z < y) = P(Z < y) - P(Z < x)
i <- pnorm(-0.1) - pnorm(-1.35)
j <- pnorm(-0.9) - pnorm(-2.1)