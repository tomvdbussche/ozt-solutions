library(MASS)

t <- with(Aids2, table(T.categ, sex))

plot(t, main = "Aids")

chisq <- chisq.test(t)

a <- 0.05
X2 <- chisq$statistic
df <- chisq$parameter
g <- qchisq(1 - a, df = df)
p <- chisq$p.value

# Summary

print(ftable(t))

writeLines(
  c(
    "",
    sprintf("Chi² = %.2f", X2),
    sprintf("df   = %.0f", df),
    sprintf("g    = %.2f", g),
    sprintf("p    = %s", p)
  )
)
