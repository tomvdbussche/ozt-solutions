source("functions.R")

# Salmonella

x <- c(0.593, 0.142, 0.329, 0.691, 0.231, 0.793, 0.519, 0.392, 0.418)

t.log(t.test(x, alternative = "greater", mu = 0.3), 0.05)

# Reactiesnelheid nieuw medicijn

MC <- c(91,87,99,77,88,91)
MT <- c(101,110,103,93,99,104)

t.log(t.test(MC, MT, alternative = "less", mu = 0), 0.05)

# Benzine met additieven

reg <- c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
add <- c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)

t.log(t.test(add, reg, alternative = "greater", paired = TRUE), 0.05)