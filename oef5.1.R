data <- read.csv("data/MuziekWijn.csv")

t <- with(data, table(Muziek, Wijn))

# Kruistabel

print(ftable(t))

# Marginalen

print(margin.table(t, 1)) # Muziek
print(margin.table(t, 2)) # Wijn

# Verwachte waarden

e <- as.array(
  margin.table(t, 1) %*% t(
    as.array(margin.table(t, 2))
  ) / margin.table(t)
) 

# X2

X2 <- sum((t - e)^2 / e)

# CramersV

n <- margin.table(t)       # totaal aantal wijnen
k <- min(nrow(t), ncol(t)) # aantal rijen / kollommen (laagste)

V <- sqrt(X2 / (n * (k - 1)))

writeLines(
  c(
    sprintf("X2 = %s", X2),
    sprintf("V  = %s", V)
  )
)

# Alternatief
library(lsr)
V <- cramersV(t)
print(sprintf("V  = %s", V))