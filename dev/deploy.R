# Configure your Connect server/account in rsconnect first.
# Capture the repository location when this helper is sourced, so deployment
# does not depend on the working directory at the time it is called.
deploy_financialreports <- local({
  source_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
  sourced_file <- if (length(source_files)) tail(source_files, 1L)[[1L]] else NULL
  default_app_dir <- if (!is.null(sourced_file)) {
    dirname(dirname(normalizePath(sourced_file, winslash = "/", mustWork = TRUE)))
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }

  function(account = NULL, server = NULL, appName = "financialreports",
    appDir = default_app_dir, dry_run = FALSE) {
    app_dir <- normalizePath(appDir, winslash = "/", mustWork = TRUE)
    lockfile <- file.path(app_dir, "renv.lock")
    if (!file.exists(lockfile)) {
      stop("renv.lock was not found in the deployment directory: ", app_dir,
        ". Deploy the repository root containing both app.R and renv.lock.")
    }
    required <- c("app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", "renv.lock")
    missing <- required[!file.exists(file.path(app_dir, required))]
    if (length(missing)) stop("Missing deployment file(s): ", paste(missing, collapse = ", "))
    # Explicit allowlist excludes financial data, outputs, local libraries,
    # credentials, and the simulated dataset.
    app_files <- c(required,
      file.path("R", list.files(file.path(app_dir, "R"), recursive = TRUE)),
      file.path("inst", list.files(file.path(app_dir, "inst"), recursive = TRUE)))
    app_files <- rsconnect::listDeploymentFiles(appDir = app_dir, appFiles = app_files)
    if (!"renv.lock" %in% app_files) stop("The deployment file list must include renv.lock.")
    message("Deployment directory: ", app_dir)
    message("Lockfile: ", lockfile)
    if (dry_run) {
      return(invisible(list(appDir = app_dir, appFiles = app_files, lockfile = lockfile)))
    }
    rsconnect::deployApp(
      appDir = app_dir, appFiles = app_files, appName = appName,
      account = account, server = server, appPrimaryDoc = "app.R"
    )
  }
})
