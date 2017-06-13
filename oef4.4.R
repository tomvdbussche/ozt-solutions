m <- 44           # populatiegemiddelde
s <- 6.2          # standaardafwijking (populatie)
n <- 72           # steekproefgrootte
sm <- 46.2        # steekproefgemiddelde
ss <- s / sqrt(n) # steekproef standaardafwijking
a <- 0.025        # significantieniveau

# 1. Kritieke grenswaarde

g <- m + qnorm(1 - a) * ss 

if (sm < g) {
  print("accept H0")
} else {
  print("reject H0")
}

# 2. Overschrijdingskans

p <- 1 - pnorm(sm, m, ss)

if (p < a) {
  print("reject H0")
} else {
  print("accept H0")
}

# 3. Significantieniveau
#
# De kans dat je H0 ten onrechte verwerpt (in dit geval 2.5%)
