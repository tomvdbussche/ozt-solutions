# For documentation generation, install the following
# dnf install libxml2-devel libxslt-devel
# install.packages("roxygen2")

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

### Bereken overschrijdingskans
# x   = steekproefgemiddelde
# m   = populatiegemiddelde
# s   = standaardafwijking
# n   = steekproefgrootte
# se  = standaardsteekproeffout


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

### Test hypothese
# kijkt of hypothese verworpen moet worden en print het resultaat

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