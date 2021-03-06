#' Initializes the environment
env.init <- function() {
  source("packages.R") # Install missing packages
  # Try to load all scripts
  for (f in list.files(pattern="*.R$")) {
    source(f)
  }
}

#' Bereken standaardfout
#' 
#' @param sd standaardafwijking
#' @param n steekproefgrootte
#' 
#' @return standaardfout
se.calc <- function(sd, n) {
  return(sd / sqrt(n))
}

#' Bereken z-score
#' 
#' @param a significantieniveau
#' 
#' @return z-score
z.calc <- function(a) {
  return (qnorm(a));
}

#' Bereken t-score
#' 
#' @param a significantieniveau
#' @param n steekproefgrootte
#' 
#' @return t-score
t.calc <- function(a, n) {
  return (qt(a, df = n - 1))
}

#' Log resultaat van t.test
#' 
#' @param t resultaat t.test
#' @param a significantieniveau
t.log <- function(t, a) {
  print(t)
  h.test(t$p.value < a)
}

#' Bereken kritieke grenswaarde
#' 
#' @param m populatiegemiddelde
#' @param score = z-score / t-score
#' @param s standaardafwijking
#' @param n steekproefgrootte
#' @param se standaardsteekproeffout
#' 
#' @return grenswaarde
g.calc <- function(m, score, s, n, se = se.calc(s, n)) {
  return(m - score * se)
}

#' Bereken overschrijdingskans
#'
#' @param x steekproefgemiddelde
#' @param m populatiegemiddelde
#' @param s standaardafwijking
#' @param n steekproefgrootte
#' @param se standaardsteekproeffout
#'
#' @return overschrijdingskans
p.calc <- function(x, m, s, n, se = se.calc(s, n)) {
  return(pnorm(x, m, se))
}

#' Test hypothese
#'
#' @param reject verwerpingsconditie
h.test <- function(reject) {
  if (reject) {
    print("reject H0")
  } else {
    print("accept H0")
  }
}

#' Bereken puntschatter
#' 
#' @param t kruistabel
#' 
#' @return e
chisq.e <- function(t) {
  return(
    margin.table(t, 1) %*% t(
      as.array(margin.table(t, 2))
    ) / margin.table(t)
  )
}

#' Bereken chi-kwadraat
#' 
#' @param t kruistabel
#' 
#' @return chi-kwadraat
chisq.calc <- function(t, e = chisq.e(t)) {
  return(
    sum((t - e)^2 / e)
  )
}

#' Bereken aantal waarnemingen
#' 
#' @param t kruistabel
cross.n <- function(t) {
  return(
    margin.table(t)
  )
}

#' Bereken kleinste waarde aantal rijen of kolommen
#' 
#' @param t kruistabel
cross.k <- function(t) {
  return(
    min(nrow(t), ncol(t))
  )
}

#' Bereken Cramers' V
#' 
#' @param t kruistabel
#' @param x2 chi-kwadraat
#' @param n aantal waarnemingen
#' @param k kleinste waarde van aantal rijen of kolommen
cv.calc <- function(t, x2 = chisq.calc(t), n = cross.n(t), k = cross.k(t)) {
  return(
    sqrt(x2 / (n * (k - 1)))
  )
}

#' Toont de samenhang tussen variabelen in een kruistabel
#' 
#' @param t kruistabel
cv.test <- function(t, cv = cv.calc(t)) {
  if (cv == 0) {
    print("Geen samenhang")
  } else if (cv < 0.175) {
    print("Zwakke samenhang")
  } else if (cv < 0.375) {
    print("Redelijk sterke samenhang")
  } else if (cv < 0.625) {
    print("Sterke samenhang")
  } else if (cv < 0.875) {
    print("Zeer sterke samenhang")
  } else {
    print("volledige samenhang")
  }
}

#' Plot lineair verband en teken regressierechte
#' 
#' @param x x
#' @param y y
#' @param regr lineaire regressierechte
#' @param main titel
#' @param xlab label x
#' @param ylab label y
#' @param regr
regr.plot <- function(x, y, regr = lm(y ~ x), main = 'y ~ x', xlab = 'x', ylab = 'y') {
  plot(x, y, main, xlab, ylab)
  abline(regr, col = "red")
}

#' Test verband tussen 2 variabelen
#' 
#' @param x
#' @param y
#' @param r correlatiecoefficient
pearson.test <- function(x, y, r = cor(x, y)) {
  if (r > 0) {
    print("Positief lineair verband")
  } else if (r < 0) {
    print("Negatief lineair verband")
  } else {
    print("Geen lineaire samenhang")
  }
}