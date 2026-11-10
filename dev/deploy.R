# Configure your Connect server/account in rsconnect first. Run from project root.
# Supply your configured account and server to this function.
deploy_financialreports <- function(account, server, appName = "financialreports") {
  # Explicit allowlist: never bundle source financial data, generated reports,
  # local libraries, credentials, or the simulated dataset.
  app_files <- c("app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", "renv.lock",
    list.files("R", recursive = TRUE, full.names = TRUE),
    list.files("inst", recursive = TRUE, full.names = TRUE))
  rsconnect::deployApp(
    appDir = ".", appFiles = app_files, appName = appName,
    account = account, server = server, appPrimaryDoc = "app.R"
  )
}
