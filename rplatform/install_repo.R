# Settings
repos <- c(
CRAN = "https://cran.microsoft.com/snapshot/2021-08-25"
)
options(repos = repos)
repo_path <- "/tmp/gDRimport"
base_dir <- "/mnt/vol"

# Use GitHub access_token if available
gh_access_token_file <- file.path(base_dir, ".github_access_token.txt")
if (file.exists(gh_access_token_file)) {
  ac <- readLines(gh_access_token_file, n = 1L)
  stopifnot(length(ac) > 0)
  Sys.setenv(GITHUB_TOKEN = ac)
}

remotes::install_local(path = "/tmp/gDRimport")
