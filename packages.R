# Installs required packages (only installs missing packages)

pkgs <- c(
  "car",
  "gmodels", 
  "lsr",
  "RcmdrMisc",
  "sp",
  "raster"
)

missing <- setdiff(pkgs, rownames(installed.packages()))

if (length(missing) > 0) {
  install.packages(missing)
}