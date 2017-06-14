data <- read.csv("data/bestat-vl-ages2.csv")

t <- with(data, table(Muziek, Wijn))

print(ftable(t))

print(sprintf("Totaal: %f", margin.table(t)))
print(margin.table(t, 1)) # Muziek
print(margin.table(t, 2)) # Wijn
