Cats <- read.csv("data/Cats.csv")

# View(Cats)

Bwt <- Cats$Bwt
Hwt <- Cats$Hwt

r <- lm(Hwt ~ Bwt) # Regressierechte

plot(x = Bwt, y= Hwt)
abline(r)

r <- cor(Hwt, Bwt) # Pearson's Correlatiecoefficient

# +1 = Stijgend gecorreleerd verband
# -1 = Dalend gecorreleerd verband
# 0  = Nit gecorreleerde variabelen