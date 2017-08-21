# Installs required packages (only installs missing packages)

pkgs <- c("gmodels", "lsr")

missing <- setdiff(pkgs, rownames(installed.packages()))

if (length(missing) > 0) {
  install.packages(missing)
}