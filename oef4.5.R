data <- c(400, 350, 400, 500, 300, 350, 200, 500, 200, 250, 250, 500, 350, 100)

m <- 300            # gemiddelde
n <- 14             # steekproefgrootte
sm <- mean(data)    # steekproefgemiddelde
ss <- sd(data)      # steekproefstandaardafwijking
ssm <- ss / sqrt(n) # steekproefstandaardfout
a <- 0.05

# H0 = P(m = 300)
# H1 = P(m > 300)

# Gebruik t-toets (n < 30)

t <- qt(1 - a, df = n -1)
g <- m + t * ssm

if (sm < g) {
  print("accept H0")
} else {
  print("reject H0")
}

# Alternatief

t.test(data, alternative = "greater", mu = m)

