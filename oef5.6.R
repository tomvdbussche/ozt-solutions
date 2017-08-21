Cats <- read.csv("data/Cats.csv")

x <- Cats$Bwt
y <- Cats$Hwt

regr <- lm(y ~ x) # Regressierechte

plot(x, y)
abline(regr, col="red")

pearson <- cor(y, x) # Pearson's Correlatiecoefficient

# +1 = Stijgend gecorreleerd verband
# -1 = Dalend gecorreleerd verband
# 0  = Niet gecorreleerde variabelen