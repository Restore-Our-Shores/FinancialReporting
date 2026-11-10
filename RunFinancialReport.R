# Usage: Rscript RunFinancialReport.R config/report_config.R
# The configuration is an R expression returning a named list for build_reports().
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[1])) {
  stop("Usage: Rscript RunFinancialReport.R path/to/report_config.R")
}
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)
project_root <- dirname(script_path)
# Inputs in the config are relative to the calling working directory.
Sys.setenv(RENV_PROJECT = project_root)
source(file.path(project_root, ".Rprofile"), local = TRUE)
pkgload::load_all(project_root, export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
report_config <- dget(args[1])
if (!is.list(report_config) || is.null(names(report_config))) stop("Config must be a named list.")
result <- do.call(financialreports::build_reports, report_config)
cat("Report saved to:", normalizePath(result$paths$quarter), "\n")
if (!is.null(result$project_deliverables)) {
  cat("Project reports:", normalizePath(result$project_deliverables$root), "\n")
}
print(result$checks, row.names = FALSE)
