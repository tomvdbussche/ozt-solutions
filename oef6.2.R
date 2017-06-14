library(MASS)
# View(survey)

analyze <- function(x, y, xlab = "x", ylab = "y") {
  title <- paste(xlab, ylab, sep = " ~ ")
  writeLines(
    c(
      "\n==============================",
      title, 
      "==============================\n"
    )
  )
  
  # Frequency table
  t <- table(x, y, dnn = c(xlab, ylab))
  
  print(ftable(t)) # show table
  writeLines("\n")
  print(summary(t)) # show sumarry
  
  p <- mosaicplot(t, main = title) # show graph
  
  chisq.res <- chisq.test(t, correct = FALSE) # calculate chi-square
  
  a <- 0.05
  X2 <- chisq.res$statistic
  df <- chisq.res$parameter
  g <- qchisq(1 - a, df = df)
  
  if(X2 < g) {
    result <- "accept H0"
  } else {
    result <- "reject H0"
  }
  
  # Besluit:
  writeLines(
    c(
      "",
      sprintf("Chi² = %f", X2),
      sprintf("g    = %f", g),
      "",
      sprintf("    => %s", result)
    )
  )
}

r1 <- with(survey, analyze(Exer, Smoke, "Exercise", "Smoking"))
r2 <- with(survey, analyze(W.Hnd, Fold, "Writing hand", "Arm fold"))
r3 <- with(survey, analyze(Sex, Smoke, "Gender", "Smoking"))
r4 <- with(survey, analyze(Sex, W.Hnd, "Gender", "Writing hand"))