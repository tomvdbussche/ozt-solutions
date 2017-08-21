### Bereken standaardfout
# sd  = standaardafwijking
# n   = steekproefgrootte
se.calc <- function(sd, n) {
  return(sd / sqrt(n))
}

### Bereken z-score
# a   = significantieniveau
z.calc <- function(a) {
  return (qnorm(a));
}

### Bereken t-score
# a   = significantieniveau
# n   = steekproefgrootte
t.calc <- function(a, n) {
  return (qt(a, df = n - 1))
}

### Bereken kritieke grenswaarde
# m     = populatiegemiddelde
# score = z-score / t-score
# s     = standaardafwijking
# n     = steekproefgrootte
# se    = standaardsteekproeffout
g.calc <- function(m, score, s, n, se = se.calc(s, n)) {
  return(m - score * se)
}

### Bereken overschrijdingskans
# x   = steekproefgemiddelde
# m   = populatiegemiddelde
# s   = standaardafwijking
# n   = steekproefgrootte
# se  = standaardsteekproeffout
p.calc <- function(x, m, s, n, se = se.calc(s, n)) {
  return(pnorm(x, m, se))
}

### Test hypothese
# kijkt of hypothese verworpen moet worden en print het resultaat
h.test <- function(reject) {
  if (reject) {
    print("reject H0")
  } else {
    print("accept H0")
  }
}