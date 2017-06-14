library(lsr)

# Voorbeeld 5.3

data <- read.csv("data/tabel56.csv")

t <- table(data$Merk, data$Geslacht)

print(ftable(t))

X2 <- chisq.test(t)$statistic
V <- cramersV(t)

writeLines(
  c(
    "",
    sprintf("X2 = %s", X2),
    sprintf("V  = %s", V)
  )
)