m <- 44           # populatiegemiddelde
s <- 6.2          # standaardafwijking (populatie)
n <- 72           # steekproefgrootte
sm <- 46.2        # steekproefgemiddelde
ssm <- s / sqrt(n) # steekproefstandaardfout (standaardafwijking van het steekproefgemiddelde / standard deviation of the sample mean)
a <- 0.025        # significantieniveau

# H0: m = 44
# H1: m > 44

# 1. Kritieke grenswaarde

z <- qnorm(1 - a)
g <- m + z * ssd

if (sm < g) {
  print("accept H0")
} else {
  print("reject H0")
}

# 2. Overschrijdingskans

p <- 1 - pnorm(sm, m, ssd)

if (p < a) {
  print("reject H0")
} else {
  print("accept H0")
}

# 3. Significantieniveau
#
# De kans dat je H0 ten onrechte verwerpt (in dit geval 2.5%)
