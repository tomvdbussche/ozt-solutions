m <- 20 # gemiddelde
s <- 16  # standaardafwijking
n <- 64  # steekproefgrootte

# a) verwachting & standaardafwijking van het steekproefgemiddelde

sm <- m
ssm <- s / sqrt(n)

# b
#
# /

# c) bereken z-score

x1 <- 15.5
x2 <- 23

z <- function(x) {
  (x - m) / ssm
}

z1 <- z(x1)
z2 <- z(x2)

# d) kans dat x < 16

xd <- 16
pd <- pnorm(xd, sm, ssm)

# e) kan dat x > 23

xe <- 23
pe <- 1 - pnorm(xe, sm, ssm)

# f) kans dat 16 < x < 22

xf1 <- 16
xf2 <- 22

pf1 <- pnorm(xf1, sm, ssm)
pf2 <- pnorm(xf2, sm, ssm)

pf <- pf2 - pf1