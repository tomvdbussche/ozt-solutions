
# install.packages("RcmdrMisc")
library(car)
library(RcmdrMisc)

# install.packages("readxl")
library(readxl)

# volk <- read_excel("data/bevolkingsgegevens.xlsx", sheet = "bevolkingsgegevens")
volk <- readXL("data/bevolkingsgegevens.xlsx", rownames=FALSE , header=TRUE , na="", sheet="bevolkingsgegevens", stringsAsFactors=TRUE)


mean_geboorte <- mean(volk$Bevolkingsgroei)
mean_sterfte <- mean(volk$Overledenen)
mean_scheiding <- mean(volk$Echtscheiding)

# numSummary(Bevolkingsgegevens[,c("Echtscheiding", "Levendgeborenen", "Overledenen")], statistics=c("mean"))

sd_immigratie <- sd(volk$Immigratie)
sd_emigratie <- sd(volk$Emigratie)

# numSummary(Bevolkingsgegevens[,c("Emigratie", "Immigratie")], statistics=c("sd"))

# install.packages("raster")
library(sp)
library(raster)

cv_huwelijksontbinxding <- cv(volk$Huwelijksontbindingen)
cv_echtscheidingen <- cv(volk$Echtscheiding)

# numSummary(Bevolkingsgegevens[,c("Echtscheiding", "Huwelijksontbindingen")],statistics=c("cv"))

maanden <- seq(1,200,6)
maandlabels <- volk$Periode[maanden]

plot(volk$Bevolking, type = "l", xaxt="n", xlab = "")
axis(1, at=maanden, labels=maandlabels,las=3)

em <- volk$Emigratie
im <- volk$Immigratie

plot(em, type = "l", col= "red", xlab = "", xaxt = "n", main = "Migratie")
lines(im, type = "l", col = "blue")
axis(1, at=maanden, labels=maandlabels,las=3)

# with(Bevolkingsgegevens, lineplot(1:200, Emigratie, Immigratie))

# Mean average

ma <- function(x,n=40){filter(x,rep(1/n,n), sides=2)}

me <- ma(volk$Emigratie)
mi <- ma(volk$Immigratie)

min <- min(c(me, mi), na.rm = TRUE)
max <- max(c(me, mi), na.rm = TRUE)

plot(me, type = "l", col= "red", xlab = "", xaxt = "n", main = "Migratie", ylim = c(min, max))
lines(mi, type = "l", col = "blue")
lines(me~mi, type = "l", col = "green")
axis(1, at=maanden, labels=maandlabels,las=3)