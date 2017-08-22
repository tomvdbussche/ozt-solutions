source("functions.R")

# View(mtcars)

# 1 bereken mediaan hp, 3e kwartiel wt

median(mtcars$hp)
quantile(mtcars$wt)

# 2 toon het verband aan tussen hp en mpg

x <- mtcars$hp
y <- mtcars$mpg

print(regr)

cor.test(x, y)
pearson.test(x, y)

regr <- lm(y ~ x) # Regressierechte

regr$effects

plot(x, y)
abline(regr, col="red")

pearson <- cor(y, x) # Pearson's Correlatiecoefficient

# 3 

cars4cyl <- subset(mtcars, mtcars$cyl == 4)

m <- mean(cars4cyl$mpg)
s <- sd(cars4cyl$mpg)
n <- length(cars4cyl$mpg)
t <- t.calc(0.975, n)


g1 <- g.calc(m, t, n, s)
g2 <- g.calc(m, -t, n, s)

# 4 als een auto een gemiddelde x heeft van 14,6

m <- 14.6
n <- 100
x <- 14
s <- 3.139
a <- 0.05

z <- z.calc(0.05)
g <- g.calc(m, z, s, n)
p <- p.calc(x, m, s, n)

h.test(x < -g)
h.test(p > a)

# 5 is er een verband tussen am (automatisch/manueel) en qsec?

auto <- subset(mtcars, mtcars$am == 0)
man <- subset(mtcars, mtcars$am == 1)

m <- mean(auto$qsec)
x <- mean(man$qsec)

t <- table(mtcars$am, mtcars$qsec)

k <- cross.k(t)

chisq.test(mtcars$am, mtcars$qsec)

# 6 is er een verband tussen gear en cyl

chisq.test(mtcars$gear, mtcars$cyl)
qchisq(1 - 0.01, 2)


cor(mtcars$gear, mtcars$cyl)

g <- qchisq(1 - 0.01, 2)