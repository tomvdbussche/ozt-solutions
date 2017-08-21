Aardbevingen <- read.csv("data/Aardbevingen.csv")

t <- table(Aardbevingen$Type, Aardbevingen$Source)

print(t)

# X2

chisq.result <- chisq.test(t)
chisq <- chisq.result$statistic

print(chisq.result)

# alternatief
#
# chisq.result <- summary(t)
# chisq <- chisq.result$statistic

# Cramers V

V <- cramersV(t)

print(sprintf("Cramers V: %s", V))