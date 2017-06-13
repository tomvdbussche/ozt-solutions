# QQ Plot

m <- 1000
s <- 50
n <- 50

obs <- rnorm(n, m, s)

qqnorm(obs) 

x <- seq(-3, 3, length = n)

lines(x, m+s*x, col="red")

# Superman

mr <- 5
sdr <- 1.5

z <- function(x, m, sd) {
  (x-m)/sd
}

zr <- function(x) {
  z(x, mr, sdr)
}

pr <- function(x) {
  pnorm(x, mr, sdr)
}

pr(4)
pr(7)
pr(3)

pr(6.5) - pr(2)

zr(4)
zr(7)
zr(3)

zr(6.5)
zr(2)