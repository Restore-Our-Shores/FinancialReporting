local({
  project <- Sys.getenv("RENV_PROJECT", unset = getwd())
  # Keep this project's library and support files together, including on Windows.
  Sys.setenv(
    RENV_PATHS_ROOT = file.path(project, "renv", "local"),
    RENV_PATHS_LIBRARY = file.path(project, "renv", "library"),
    RENV_PATHS_SANDBOX = file.path(project, "renv", "sandbox")
  )
  source(file.path(project, "renv", "activate.R"))
})
