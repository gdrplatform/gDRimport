REPO_DIR <- commandArgs(trailingOnly = TRUE)[1]
LIB_DIR <- commandArgs(trailingOnly = TRUE)[2]
PKG_DIR <- file.path(REPO_DIR, "gDRimport")
stopifnot(
  dir.exists(REPO_DIR),
  dir.exists(LIB_DIR),
  dir.exists(PKG_DIR)
)

# Load libraries
invisible(sapply(list.files(LIB_DIR, , pattern = "*.R$", full.names = TRUE), source, .GlobalEnv))

# If the package uses shinytest, tests should be done outside of devtools::check (check with '--no-tests')
cat("Lint")
gDRstyle::lintPkgDirs(PKG_DIR)
cat("Tests")
devtools::test(PKG_DIR, stop_on_failure = TRUE)
cat("Check")
devtools::check(PKG_DIR, error_on = "error", args = c('--no-build-vignettes', '--no-examples', '--no-manual', '--no-tests'))
cat("Deps")
gDRstyle::checkDependencies(
  desc_path = paste0(REPO_DIR, '/DESCRIPTION'), 
  dep_path = paste0(REPO_DIR, '/rplatform/dependencies.yaml')
)
