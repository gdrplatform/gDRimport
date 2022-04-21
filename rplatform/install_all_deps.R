# Settings
repos <- c(
CRAN = "https://cran.microsoft.com/snapshot/2021-08-25"
)
options(repos = repos)
essential_pkgs <- list(
  list(name = "git2r"),
  list(name = "yaml"),
  list(name = "BiocManager"),
  list(name = "curl")
)
base_dir <- "/mnt/vol"
deps_yaml <- file.path(base_dir, "/dependencies.yaml")
use_ssh <- FALSE

# Auxiliary functions
verify_version <- function(name, required_version) {
  pkg_version <- packageVersion(name)
  ## '>=1.2.3' => '>= 1.2.3'
  required_version <-
    gsub("^([><=]+)([0-9.]+)$", "\\1 \\2", required_version, perl = TRUE)
  if (!remotes:::version_satisfies_criteria(pkg_version, required_version)) {
    stop(sprintf(
      "Invalid version of %s. Installed: %s, required %s.",
      name,
      pkg_version,
      required_version
    ))
  }
}
#' this function help figuring out which GitHub domain should be used 
#' github.roche.com will be chosen if available 
#' otherwise github.com
get_github_hostname <- function() {
  conn_status <- tryCatch(
    curl::curl_fetch_memory("github.roche.com"),
    error = function(e) {
      e
    }
  )
  # error in connection to database will be returned as error list with exit_code = 2
  if (inherits(conn_status, "error")) {
    "api.github.com"
  } else {
    "github.roche.com/api/v3"
  }
}

# Install {remotes}
if (!"remotes" %in% installed.packages()) {
  install.packages(pkgs = "remotes")
}


# Install essential tools
for (pkg in essential_pkgs) {
  if (!pkg$name %in% installed.packages()) {
    remotes::install_version(
      package = pkg$name,
      version = pkg$version
    )
  }
}

# determine GitHub domain
gh_hostname <- get_github_hostname()

# Use GitHub access_token if available
gh_access_token_file <- file.path(base_dir, ".github_access_token.txt")
if (file.exists(gh_access_token_file)) {
  ac <- readLines(gh_access_token_file, n = 1L)
  stopifnot(length(ac) > 0)
  Sys.setenv(GITHUB_TOKEN = ac)
}

# Use SSH keys
keys <- if (isTRUE(use_ssh)) {
  git2r::cred_ssh_key(
    publickey = ssh_key_pub,
    privatekey = ssh_key_priv
  )
}


# Install all dependencies
deps <- yaml::read_yaml(deps_yaml)$pkgs
for (name in names(deps)) {
  pkg <- deps[[name]]
  if (is.null(pkg$source)) { pkg$source <- "Git" }
  switch(toupper(pkg$source),

    ## CRAN installation
    "CRAN" = {
      if (is.null(pkg$repos)) { pkg$repos <- repos }
      remotes::install_version(
        package = name,
        version = pkg$ver,
        repos = pkg$repos
      )
    },

    ## Bioconductor installation
    "BIOC" = {
      if (is.null(pkg$ver)) { pkg$ver <- BiocManager::version() }
      BiocManager::install(
        pkgs = name,
        update = FALSE,
        version = pkg$ver  ## Bioc version or 'devel'
      )
    },
    
    ## GitHub installation
    "GITHUB" = {
      if (is.null(pkg$ref)) { pkg$ref <- "HEAD" }
      remotes::install_github(
        repo = pkg$url,
        ref = pkg$ref,
        subdir = pkg$subdir,
        host = gh_hostname
      )
      verify_version(name, pkg$ver)
    },
    
    ## Git installation
    "GIT" = {
      remotes::install_git(
        url = pkg$url,
        subdir = pkg$subdir,
        ref = pkg$ref,
        credentials = keys
      )
      verify_version(name, pkg$ver)
    }
  )
}
